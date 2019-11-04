import GetOldTweets3 as got
import yaml
import os

config = yaml.load(open('config.yaml', 'r'))


def main():

    #Access countries and keywords to be search from config file
    countries = config['countries']
    keywords = config['keywords']

    #Folder where we will save tweets
    if not os.path.exists("downloaded_tweets"):
        os.makedirs("downloaded_tweets")

    #For each country, search all keywords and append results to our list of tweets
    for country in countries:

        #List of tweets that contain any of our keywords
        tweets = list()
        tweet_id=1

        outputFile = open("downloaded_tweets/"+country+"_output.csv", "w+", encoding="utf8")
        outputFile.write('id,date,username,to,replies,retweets,favorites,text,geo,mentions,hashtags,id,permalink\n')

        for keyword in keywords:
            #In the meantime getting max 100 tweets per keyword
            tweets.extend(querySearch(keyword,country,100))

        for t in tweets:
            print(t.text)
            print("\n")

            data = [tweet_id,
                    t.date.strftime("%Y-%m-%d %H:%M:%S"),
                    t.username,
                    t.to or '',
                    t.replies,
                    t.retweets,
                    t.favorites,
                    '"'+t.text.replace('"','""')+'"',
                    t.geo,
                    t.mentions,
                    t.hashtags,
                    t.id,
                    t.permalink]

            tweet_id+=1

            data[:] = [i if isinstance(i, str) else str(i) for i in data]
            outputFile.write(','.join(data) + '\n')

        outputFile.flush()
        #Remember to clean tweets! We are gettings tweets from accounts that have the keyword in their name, but not in the tweets text

def querySearch(keyword, country, maxTweets):

    since_date = config['since_date']
    until_date = config['until_date']
    within_radius = config['within_radius']

    tweetCriteria = got.manager.TweetCriteria()\
                            .setQuerySearch(keyword)\
                            .setMaxTweets(maxTweets)\
                            .setSince(since_date)\
                            .setUntil(until_date)\
                            .setNear(country)\
                            .setWithin(within_radius)

    tweets = got.manager.TweetManager.getTweets(tweetCriteria)

    return tweets


main()
