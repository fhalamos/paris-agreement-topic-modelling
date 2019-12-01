#Setups
library(dplyr)
library(skimr)
library(seriation)
library(ggplot2)
library(dbscan)
library(mixtools)
library(plotGMM)
library(gridExtra)
library(tm)
library(wordcloud)
library(knitr)
library(tibble)
library(tidytext)
library(topicmodels)
library(tidyr)

#Selecting file of tweets
file <-file.choose()
data <- read.csv(file, header = TRUE)

#Select text and id column.
data <- data %>% select(id, text)


#First filter, remove tweets associated to retweets
data$text <- sub("RT.*", "", data$text)

# Now we can create our raw corpus
colnames(data)<-c("doc_id","text")
docs <- VCorpus(DataframeSource(data))


# PREPROCESSING
docs <- docs %>% 
  tm_map(content_transformer(tolower)) %>%
  tm_map(tolower) %>% # Remove captialization
  tm_map(removeWords, stopwords("english")) %>% #Remove stopwords
  tm_map(removeWords, c("#environment", "environment", "environmental")) %>% #Remove special stopwords to this case 
  tm_map(removeWords, c("wild", "wolf", "river", "cdnpoli","florida","usa","fmsphotoaday")) %>% #Remove words of hot topics at that tim. for 2019: "news construction network links"
  tm_map(removeWords, c("will","can", "read","see")) %>%
  tm_map(removeNumbers) %>% #Remove numbers
  tm_map(PlainTextDocument)


#Put together some specific words
for (j in seq(docs)) {
  docs[[j]] <- gsub("climate change", "climate_change", docs[[j]])
  docs[[j]] <- gsub("climatechange", "climate_change", docs[[j]])
}
docs <- tm_map(docs, PlainTextDocument)


# A bit more cleaning of unique characters
for (j in seq(docs)) {
  docs[[j]] <- gsub("/", "", docs[[j]])
  docs[[j]] <- gsub("’", "", docs[[j]])
  docs[[j]] <- gsub("—", "", docs[[j]])
  docs[[j]] <- gsub("-", "", docs[[j]])
  docs[[j]] <- gsub("…", "", docs[[j]])
  docs[[j]] <- gsub("“", "", docs[[j]])
  docs[[j]] <- gsub("\\|", "", docs[[j]])
  docs[[j]] <- gsub("@.*", "", docs[[j]]) #Remove mentions
  #docs[[j]] <- gsub("#.*", "", docs[[j]]) #Remove hastags
  docs[[j]] <- gsub("#", "", docs[[j]]) #Remove hastags
  docs[[j]] <- gsub("RT.*", "", docs[[j]]) #Remove anything with RT
  docs[[j]] <- gsub("http.*", "", docs[[j]]) #Remove anything with a link
  docs[[j]] <- gsub("//.*", "", docs[[j]]) #Remove anything with a link
  docs[[j]] <- gsub("\u2028", "", docs[[j]])  # an ascii character that does not translate
}
docs <- docs %>% 
  tm_map(PlainTextDocument)

docs <- docs %>% 
  tm_map(removePunctuation) %>%  #Remove Punctuation
  tm_map(stripWhitespace)  %>% #Remove white spaces
  tm_map(PlainTextDocument)


#Remove duplicate documents
corpus_df <- data.frame(text=unlist(sapply(docs, `[`, "content")), 
                        stringsAsFactors=F)
corpus_df_unique_t <- as.data.frame(unique(corpus_df$text))
colnames(corpus_df_unique_t)<-c("text")


#Remove empty rows
corpus_df_unique_t<-as.data.frame(corpus_df_unique_t[corpus_df_unique_t$text != "" | corpus_df_unique_t$text != " ",])
colnames(corpus_df_unique_t)<-c("text")

#Trim white spaces again
corpus_df_unique_t$text <- trimws(corpus_df_unique_t$text, which = c("both"))


#Save corpus df in csv
write.csv(corpus_df_unique_t, file="corpus_df.csv")

#Last, transform back to corpus format
corpus_df_unique_t$doc_id <- c(1:nrow(corpus_df_unique_t))
#Flipe order of columns
corpus_df_unique_t <- corpus_df_unique_t[,c(2,1)]
docs <- Corpus(DataframeSource(corpus_df_unique_t))




#Now that we have cleaned the data, we create the document term matrix.
dtm <- DocumentTermMatrix(docs)

#We remove documents without words
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm   <- dtm[rowTotals> 0, ] #remove all docs without words

#Lets visualize the most frequent words
frequency <- sort(colSums(as.matrix(dtm)), 
                  decreasing=TRUE)
wordcloud(names(frequency), frequency,
  min.freq = 1, # terms used at least once
  max.words = 50, # x most frequently used terms
  random.order = FALSE, # centers cloud by frequency, > = center
  rot.per = 0.001, # sets proportion of words oriented vertically
  main = "Title",
  colors = brewer.pal(8, "Dark2")
  ) 
kable(head(frequency, 7), caption="Most common words")


#In case we want to find best k
#We fit the models and find the one with lowest perplexity. We try k = 2:8
#best_perplexity <- ""
#best_k <- 0

#for (k in c(2:15))
#{
#  print(k)
#  p = perplexity(LDA(dtm, k = k))
#  print(p)
#  if(best_perplexity=="" | p < best_perplexity)
#  {
#    best_perplexity = p
#    best_k=k
#  }
#}
#best_k
#best_perplexity

#We are getting a weird result. As we increase k, perplexity always decreases. So, in the meantime, we will just choose a reasonable k which is readable, like 5.
lda_model <- LDA(dtm, k = 5)
perplexity(lda_model)

#Transform to tidy for analysis
lda_td <- tidy(lda_model)

#We present the top 6 terms of each topic.
top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(6, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

df<-as.data.frame(terms(lda_model,6))
kable(df, caption = "Top 6 words in k=5 model")

#Visualizations per topic. We also include the terms respective beta values (probabiliy of term being generated by topic):
top_terms %>%
  mutate(topic = factor(topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = topic)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free", ncol = 5) +
  coord_flip()
