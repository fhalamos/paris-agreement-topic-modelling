# Comparing twitter data and surveys data: a topic modelling approach

## Goal of study

In this project we are aiming to study if twitter data is a good proxy of masses opinion, and in particular, if they reveal the same information as traditional surveys.

To acomplish this, we will study tweets related to climate change. In particular, we will develop topic modelling techniques so as to discover the most important subtopics of climate change people speak about in twitter. After that, we will compare these topics with those expressed by people in survey questions that directly ask for the most important topics in this issues. This way, we aim to discover is the topics revealed by twitter data are or not the same as the ones recovered by surveys.

Preliminary results can be seen [here](https://docs.google.com/document/d/1NvOWnngMwzfGeFFaSJ-vzFFmbpmuBK5nRDoR7XXlWgM/edit?usp=sharing)


## Files in repo

- download_tweets.py: python script that download tweets that contain words given a specific array of keywords

- topic_modeling.R: topic modeling given .csv files with tweets data

- topic_modelling_images folder: contains visual representations of topic models results

- config.yaml: configuration file specifying parameters for analysis (ex: keywords looked in tweets, countries analyzed, etc).

## Setup
- pip install [GetOldTweets3](https://github.com/Mottl/GetOldTweets3), library used to download tweets.

<!-- ## References -->
<!-- - Twitter data cleaning: https://scholarworks.wm.edu/cgi/viewcontent.cgi?article=2406&context=honorstheses -->
<!-- - Project scope: https://publichealth.jmir.org/2018/1/e16/ -->
