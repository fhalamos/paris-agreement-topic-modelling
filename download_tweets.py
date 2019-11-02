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

    #In the mean time just printing, should rather save them in a .csv
    for tweet in tweets:
        print(tweet.text)
        print("\n")


def querySearch(search_text, maxTweets=None):

    if(maxTweets):
        tweetCriteria = got.manager.TweetCriteria().setQuerySearch(search_text)\
                                               .setMaxTweets(maxTweets)
    else:
        tweetCriteria = got.manager.TweetCriteria().setQuerySearch(search_text)

    tweets = got.manager.TweetManager.getTweets(tweetCriteria)

    return tweets


main()
