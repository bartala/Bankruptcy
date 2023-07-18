# App name ADSResearcher
import tweepy
import pandas as pd
import numpy as np
from datetime import datetime, date
import datetime
from time import gmtime, strftime
import subprocess
import time
import os, os.path
import math
import threading
import json
import gzip
import sys
import calendar
import re
from pathlib import Path
import json
import ast
from plotnine import *


PTH = '/path/to/data/'

"""## Download tweets"""

keys = pd.read_csv(os.path.join(PTH,'code','TwitterKeys.csv'))

# Twitter API V2
# APP: ADSResearcher

consumer_key = keys['value'].iloc[0]
consumer_secret= keys['value'].iloc[1]
access_token = keys['value'].iloc[2]
access_token_secret = keys['value'].iloc[3]

client = tweepy.Client(bearer_token= keys['value'].iloc[4],wait_on_rate_limit=True)

auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)
api = tweepy.API(auth, wait_on_rate_limit=True)


"""### collect tweets for companies that declared bankruptcy"""

def collect_tweets(company_user_name, S, E, fname):
  print(company_user_name)
  filename = os.path.join(PTH,'All_chapter11_2020',fname+'_'+company_user_name+'.json.gz')
  print(filename)
  for tweet in tweepy.Paginator(client.search_all_tweets,
                        query= '@'+ company_user_name +' lang:en',
                        start_time= S,
                        end_time = E,
                        expansions = ['author_id','referenced_tweets.id','referenced_tweets.id.author_id','in_reply_to_user_id','attachments.media_keys','entities.mentions.username'],
                        tweet_fields=['id','text','author_id','created_at','conversation_id','entities','public_metrics','referenced_tweets'],
                        user_fields=['id','name','username','created_at','description','public_metrics','verified'],
                        place_fields=['full_name','id'],
                        media_fields=['type','url','alt_text','public_metrics'],
                        max_results=100).flatten():
    with gzip.open(filename, 'a') as fout:
          fout.write(json.dumps(tweet.data).encode('utf-8'))
          fout.write('\n'.encode('utf-8'))


df = pd.read_csv(os.path.join(PTH,'data/All_chapter11_2020/All_chapter11_new.csv'))

# 1 month after and before

# mentiones of a company 1 month after it filed for bankruptcy
companies_before = df[['Twitter Username', 'before_start', 'before_end']]

# mentiones of the company 1 month before it filed for bankruptcy
companies_after = df[['Twitter Username', 'after_start', 'after_end']]

# after bankruptcy announcement - collect tweets and write to file
for i in range(0, len(companies_after)):
  collect_tweets(companies_after.iloc[i]['Twitter Username'],
                 companies_after.iloc[i]['after_start'],
                 companies_after.iloc[i]['after_end'],
                 'after'
                 )
  time.sleep(7)

# before bankruptcy announcement - collect tweets and write to file
for i in range(0, len(df)):
  collect_tweets(df.iloc[i]['Twitter_handle'],
                 df.iloc[i]['Twitter_start_date'],
                 df.iloc[i]['Twitter_end_date'],
                 'before'
                 )
  time.sleep(7)

# collect followers and following data
user_names = df['Twitter Username']

user_ids  = []
for user in user_names:
  user1 = api.get_user(screen_name=user)
  followers_count = user1.followers_count
  following_count = user1.friends_count
  user_ids.append([user1.id_str, user, user1.listed_count,followers_count,following_count]) # # [user_id, user_name,number of lists a user is on]

df_users = pd.DataFrame(user_ids)
df_users.columns = ['userid','username','lists','followers','following']
df_users

"""### combine tweets"""

PTH ='/path/to/data/All_chapter11_2020'

companies = [
              'Aeromexico',
              'amgreetings',
              'Avianca',
              'BriggsStratton',
              'chesapeake',
              'DAVIDsTEA',
              'ExideCare',
              'AskFrontier',
              'GarrettMotion',
              'Gordmans',
              'INTELSAT',
              'LATAMAirlines',
              'LSC_COM_',
              'MNK',
              'mcclatchy',
              'McDermott_News',
              'MoodMedia',
              'nyandcompany',
              'PREIT',
              'SeadrillLtd',
              'ShopStageStores',
              'TailorBrands',
              'TuesdayMorning',
              ]

# read the zipped file into a dataframe
df = pd.DataFrame()

for company in companies:
  print(company)
  tmp = pd.read_json(os.path.join(PTH,'before_'+company+'.json.gz'), compression='gzip',lines=True)
  tmp['company'] = company
  df = pd.concat([df, tmp], ignore_index=True)

df.to_csv(os.path.join(PTH,'before_all_2020.csv'), index=False)
del df

df = pd.DataFrame()
for company in companies:
  tmp = pd.read_json(os.path.join(PTH,'after_'+company+'.json.gz'), compression='gzip',lines=True)
  tmp['company'] = company
  df = pd.concat([df, tmp], ignore_index=True)

df.to_csv(os.path.join(PTH,'after_all_2020.csv'),index=False)

"""## Build the post-reply social network"""

PTH ='/path/to/data/All_chapter11_2020'

## functions

# casting to string
def read_tweets(filename):
  df = pd.read_csv(filename)
  df.loc[~df['in_reply_to_user_id'].isna(), 'in_reply_to_user_id'] = df.loc[~df['in_reply_to_user_id'].isna(),'in_reply_to_user_id'].astype(int).astype(str)
  df['author_id'] = df['author_id'].astype(str)
  return(df)


# REply edges (in the format of from-to edges)
def get_reply_edges(df):
  edges_reply = df[~df['in_reply_to_user_id'].isnull()][['author_id', 'in_reply_to_user_id','company']]
  edges_reply['Edge_type'] = "RE"
  edges_reply.columns = ["form","To","Company","Edge_type"]
  return(edges_reply)


# get the ids of mentioned users (RT or Mention)
def get_MT_TR_edges(df):
  edges_MT_RT = []
  tmp = []

  for i in range(len(df['entities'])):
    TweetType = "MT"
    row = df.iloc[i]['entities']
    if type(row)!=float:
      if 'mentions' in row:
        row = ast.literal_eval(row)
        if df.iloc[i]['text'].startswith("RT"):
          TweetType = 'RT'
        tmp =  [ [x['id'],df.iloc[i]['author_id'], df.iloc[i]['company'],TweetType] for x in row['mentions']]
        for x in tmp:
          edges_MT_RT.append(x)

  return(edges_MT_RT)


# group by [From],[To],[Company]
def create_weighted_edges(df):
  df = (df.groupby(['From', 'To', 'Company']).size()
                                            .sort_values(ascending=False)
                                            .reset_index(name='count')
                                            .drop_duplicates(subset='To')
                    )
  return(df)

# load the data
df_before = read_tweets(os.path.join(PTH,'before_all_2020.csv'))

df_after = read_tweets(os.path.join(PTH,'after_all_2020.csv'))

# create reply edges
edges_reply_before = get_reply_edges(df_before)

edges_reply_after = get_reply_edges(df_after)

edges_MT_RT_before = get_MT_TR_edges(df_before)
edges_MT_RT_before = pd.DataFrame(edges_MT_RT_before)
edges_MT_RT_before.columns = ["From","To","Company","Edge_type"]

edges_MT_RT_after = get_MT_TR_edges(df_after)
edges_MT_RT_after = pd.DataFrame(edges_MT_RT_after)
edges_MT_RT_after.columns = ["From","To","Company","Edge_type"]

activity_edges_before = edges_reply_before.append( edges_MT_RT_before )

activity_edges_after = edges_reply_after.append( edges_MT_RT_after )

# group by [From],[To],[Company]
activity_edges_before = create_weighted_edges(activity_edges_before)
# save to file
activity_edges_before.to_csv(os.path.join(PTH,'activity_edges_before_All_chapter11_2020.csv'), index = False)


# group by [From],[To],[Company]
activity_edges_before = create_weighted_edges(activity_edges_after)
# save to file
activity_edges_before.to_csv(os.path.join(PTH,'activity_edges_after_All_chapter11_2020.csv'), index = False)

"""###  Statistics for BEFORE bankruptcy announcement"""

PTH ='/path/to/data/All_chapter11_2020'

# read the activity (RT, MT, RE) edges
activity_edges_before = pd.read_csv(os.path.join(PTH,'activity_edges_before_All_chapter11_2020.csv'))

activity_edges_after = pd.read_csv(os.path.join(PTH,'activity_edges_after_All_chapter11_2020.csv'))

# number of companies
def num_companies(df):
  print(set(df['Company']))
  print(len(set(df['Company'])))


num_companies(activity_edges_before)

num_companies(activity_edges_after)

# number of unique users per Company BEFORE bankruptcy announcement

def plot_me(df):
    x_axis = []
    y_axis = []

    for comp in set(df['Company']):
      sub_df = df[df['Company'] == comp]
      y_axis.append(len(set(sub_df['From'].tolist() + sub_df['To'].tolist())) )
      x_axis.append(comp)

    g = pd.DataFrame({"Company":x_axis, "y":y_axis})
    return(g)

df_bar_before = plot_me(activity_edges_before)

df_bar_after = plot_me(activity_edges_after)

df_bar_after['before'] = df_bar_before['y']
df_all = df_bar_after
df_all.columns =['Company','After','Before']

del df_bar_after

"""Plot using R"""

# Commented out IPython magic to ensure Python compatibility.
# %load_ext rpy2.ipython

# https://www.askpython.com/python/examples/use-r-and-python-in-the-same-notebook#:~:text=Using%20R%20along%20with%20Python%20in%20Google%20Colab&text=To%20use%20R%20and%20python%20simultaneously%20in%20the%20same%20notebook,activate%20the%20rpy2%20package%20first.&text=rpy2%20is%20a%20high%2Dlevel,NumPy%20and%20pandas%20data%20structures.

# Commented out IPython magic to ensure Python compatibility.
# %%R
# install.packages('reshape2',repos = "http://cran.us.r-project.org")

# Commented out IPython magic to ensure Python compatibility.
# %%R
# library('ggplot2')
# library('reshape2')

# Commented out IPython magic to ensure Python compatibility.
# move pyton object to R
# %R -i df_all

# Commented out IPython magic to ensure Python compatibility.
# %%R
# 
# df_all <- melt(df_all, id.vars='Company')
# 
# ggplot(df_all, aes(x=Company, y=value, fill=variable)) +
#     geom_bar(stat='identity', position='dodge') +
#     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#     coord_flip()

"""## Get followers ids of a user"""

# load data
PTH ='/path/to/data/All_chapter11_2020'

activity_edges_before = pd.read_csv(os.path.join(PTH,'activity_edges_before_All_chapter11_2020.csv'))

activity_edges_after = pd.read_csv(os.path.join(PTH,'activity_edges_after_All_chapter11_2020.csv'))

all_users_activity_edges = list(set(
  activity_edges_after['From'].tolist() + activity_edges_after['To'].tolist() +
  activity_edges_before['From'].tolist() + activity_edges_before['To'].tolist()
))

follower_list = []

for user in all_users_activity_edges:
    print(user)
    followers = []
    try:
        for page in tweepy.Cursor(api.get_follower_ids, user_id=user).pages():
            followers.extend(page)
            print(len(followers))
            follower_list.append(followers)
    except tweepy.errors.TweepyException:
        print("error")
        continue
    follower_list = pd.DataFrame(follower_list[0])
    follower_list.to_csv(os.path.join('/content/drive/MyDrive/Colab Notebooks/BIU_HEB/data/All_chapter11_2020/followers',str(user)+'.csv'))
