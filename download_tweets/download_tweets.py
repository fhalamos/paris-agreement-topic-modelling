import GetOldTweets3 as got
import yaml
import os
import sys

config = yaml.load(open('config.yaml', 'r'))


def main():

    #args are expected to come in the following order: max_tweets, keyword_1, keyword_2, etc.


    n_args = len(sys.argv)
    print(n_args)

    max_tweets = int(sys.argv[1])
    keywords = []
    for i in range(2, n_args):
        keywords.append(sys.argv[i])

    print("max_twees")
    print(max_tweets)
    print("keywords")
    print(keywords)

    #Folder where we will save tweets
    if not os.path.exists("downloaded_tweets"):
        os.makedirs("downloaded_tweets")

    #For each country, search all keywords and append results to our list of tweets
    for country in config['countries']:

        #Create output file
        output_file_name = country+"-"+str(max_tweets)+"-"+"_".join(keywords)+".csv"
        outputFile = open("downloaded_tweets/"+output_file_name, "w+", encoding="utf8")
        outputFile.write('id,date,username,to,replies,retweets,favorites,text,geo,mentions,hashtags,id,permalink\n')

        #List of tweets that contain any of our keywords
        tweets = list()
        tweet_id=1

        #Case we wanna use keywords from configfile instead of manually imputed by user
        #for keyword in config['keywords']:
        for keyword in keywords:#config['keywords']:
            tweets_found = querySearch(keyword,country,max_tweets)
            tweets.extend(tweets_found)
            print("Founded "+str(len(tweets_found))+" for "+keyword)

            #Case we wanna use keywords from configfile instead of manually imputed by user
            #tweets.extend(querySearch(keyword,country,config['max_twees']))

        for t in tweets:
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

    print(tweetCriteria)

    tweets = got.manager.TweetManager.getTweets(tweetCriteria)

    return tweets


main()
