from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
analyser = SentimentIntensityAnalyzer()

def sentiment_analyzer_scores(sentence):
    score = analyser.polarity_scores(sentence)
    return score

import pandas as pd

pandas_df = pd.read_csv("corpus_df_#climatechange-usa.csv")
pandas_df_to_use = pandas_df['text'].dropna()

score_list = []
for i, item in enumerate(pandas_df_to_use):
    score_list.append(sentiment_analyzer_scores(item))

count_list = []
for i, item in enumerate(score_list):
    if score_list[i]['compound'] == 0:
        continue
    elif score_list[i]['compound'] >= 0.05:
        count_list.append('positive')
    elif (score_list[i]['compound'] > -0.05) and (score_list[i]['compound'] <0.05) :
        count_list.append('neutral')
    elif score_list[i]['compound'] <= -0.05:
        count_list.append('negative')
    else:
        count_list.append(0)


final_pandas = pd.DataFrame(np.array(count_list).reshape(len(count_list),1))

final_pandas['id'] = final_pandas.index + 1

final_pandas.to_csv("vader_counts.csv")