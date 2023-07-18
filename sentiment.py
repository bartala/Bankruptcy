import pandas as pd
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer


# 8) Average tweet sentiment
# Get the sentiment score of a text vector

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
