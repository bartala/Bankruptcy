import pandas as pd
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
import re
import os
from textblob import TextBlob

# 8) Average tweet sentiment (see data_analysis.R)
# Get the sentiment score of a text vector

def sentiment_analyzer(file):
  x = pd.read_csv(os.path.join(PTH,file,'.csv'))
  analyzer = SentimentIntensityAnalyzer()
  
  sentiment_scores = []
  sentiment_pos = []
  sentiment_neg = []
  sentiment_neu = []
  
  for text in x['text']:
    scores = analyzer.polarity_scores(text)
    sentiment_scores.append( scores['compound'] )
    sentiment_neg.append( scores['neg'] )
    sentiment_pos.append( scores['pos'] )
    sentiment_neu.append( scores['neu'] )
  
  x['sentiment'] = sentiment_scores
  x['pos'] = sentiment_pos
  x['neg'] = sentiment_neg
  x['neu'] = sentiment_neu
  x.to_csv(os.path.join(PTH,'before.csv'))


def sentiment_tweets():
  tweets_R = pd.read_csv(os.path.join(PTH,'tweets_R.csv'))
  result = [re.sub(r'http\S+', '', x) for x in tweets_R]
  sentiment = []
  subjectivity = []
  
  for text in result:
    x = TextBlob(text)
    sentiment.append( x.sentiment[0] )
    subjectivity.append( x.sentiment[1] ) # The subjectivity is a float within the range [0.0, 1.0] where 0.0 is very objective and 1.0 is very subjective.

 sentiment.to_csv(os.path.join(PTH,'sentiment.csv'))
 subjectivity.to_csv(os.path.join(PTH,'subjectivity'))
