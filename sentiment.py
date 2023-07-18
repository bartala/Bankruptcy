import pandas as pd
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
import re
from textblob import TextBlob

# 8) Average tweet sentiment (see data_analysis.R)
# Get the sentiment score of a text vector

def sentiment_tweets_1():
  before = pd.read_csv(os.path.join(PTH,before.csv))
  analyzer = SentimentIntensityAnalyzer()
  
  sentiment_scores = []
  sentiment_pos = []
  sentiment_neg = []
  sentiment_neu = []
  
  for text in before['text']:
    scores = analyzer.polarity_scores(text)
    sentiment_scores.append( scores['compound'] )
    sentiment_neg.append( scores['neg'] )
    sentiment_pos.append( scores['pos'] )
    sentiment_neu.append( scores['neu'] )
  
  before['sentiment'] = sentiment_scores
  before['pos'] = sentiment_pos
  before['neg'] = sentiment_neg
  before['neu'] = sentiment_neu
  pd.to_csv(os.path.join(PTH,'before.csv'))


def sentiment_tweets():
  tweets_R = pd.read_csv(os.path.join(PTH,'tweets_R.csv'))
  result = [re.sub(r'http\S+', '', x) for x in tweets_R]
  sentiment = []
  subjectivity = []
  
  for text in result:
    x = TextBlob(text)
    sentiment.append( x.sentiment[0] )
    subjectivity.append( x.sentiment[1] ) # The subjectivity is a float within the range [0.0, 1.0] where 0.0 is very objective and 1.0 is very subjective.

 pd.to_csv(os.path.join(PTH,'sentiment.csv'))
 pd.to_csv(os.path.join(PTH,'subjectivity'))
