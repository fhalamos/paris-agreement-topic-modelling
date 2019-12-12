library(tidytext)
library(ggplot2)

# Begin sentiment analysis ------------------------------------------------

corpus_csv <- read_csv(file.choose())

#choose the file of tweets which is entitled "corpus_df_#climatechange-usa.csv"

tidy_df<- corpus_csv %>%
  unnest_tokens(word, text) %>%
  rename(id = "X1")

# Afinn Dictionary analysis ------------------------------------------------

w_sentiments <- tidy_df %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, id, value) 

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

grouped_words <- w_sentiments %>%
  group_by(id) %>%
  summarize(sent_mean = mean(value),
            sent_mode = Mode(value)) 

# Afinn analysis by Mean ------------------------------------------------

ggplot(grouped_words, aes(x=sent_mean)) + 
  geom_histogram(binwidth = 2) +
  labs(x= "Sentiment Score", y="Number of Tweets", title = "Afinn Sentiments of Climate Change Tweets, Mean")

# Afinn analysis by Mode ------------------------------------------------

ggplot(grouped_words, aes(x=sent_mode)) + 
  geom_histogram(binwidth = 2) +
  labs(x= "Sentiment Score", y="Number of Tweets", title = "Afinn Sentiments of Climate Change Tweets, Mode")

# Afinn analysis most common words by value ------------------------------------------------

afinn_word_counts <- tidy_df %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, value, sort = TRUE) %>%
  ungroup()

afinn_word_counts %>%
  group_by(value) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n, fill = value)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~value, scales = "free_y") +
  labs(y = "Number of Words",
       x = NULL) +
  ggtitle("Word Contributions to Afinn Sentiments, by Sentiment Value") +
  coord_flip()


# NRC Dictionary analysis ------------------------------------------------
nrc_word_counts <- tidy_df %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

nrc_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Number of Words",
       x = NULL) +
  ggtitle("Word Contributions to NRC Sentiments, by Sentiment Word") +
  coord_flip()


# # VADER Dictionary analysis ------------------------------------------------
sentiments_vader <- read_csv(file.choose())

#choose "vader_counts.csv"

grouped_sentiments <- sentiments_vader %>%
  rename("sentiment" = "0") %>%
  mutate(sentiment = factor(sentiment, levels = c("negative", "neutral", "positive")))

ggplot(grouped_sentiments, aes(x=sentiment)) + 
  geom_bar() +
  labs(x = "Sentiment Score", y= "Number of Tweets", title = "Vader Sentiments of Climate Change Tweets")

