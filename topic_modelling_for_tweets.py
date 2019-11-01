import GetOldTweets3 as got

def main():

    #1. Get list of tweets associated to our key words
    key_words = ["paris agreement", "climate change"]

    #List of tweets that contain any of our keywords
    tweets = list()

    #For each keyword, make a search and append results to our list of tweets
    for key_word in key_words:
         #In the meantime getting max 5 tweets per keyword
        tweets.extend(querySearch(key_word,5))


    #2. Cleaning tweets


    #3. Topic modelling


    for tweet in tweets:
        print(tweet)
        print("\n")



def querySearch(search_text, maxTweets=None):

    if(maxTweets):
        tweetCriteria = got.manager.TweetCriteria().setQuerySearch(search_text)\
                                               .setMaxTweets(maxTweets)
    else:
        tweetCriteria = got.manager.TweetCriteria().setQuerySearch(search_text)

    tweets = got.manager.TweetManager.getTweets(tweetCriteria)

    #Just saving text attribute of tweets
    tweets_text = [tweet.text for tweet in tweets]

    return tweets_text


main()
