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

#Selecting file of tweets (choose tweets from downloaded_tweets folder)
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
  tm_map(removeWords, c("wild", "wolf", "river", "cdnpoli","florida","usa","fmsphotoaday","thanksgiving")) %>% #Remove words of hot topics at that tim. for 2019: "news construction network links"
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
  #docs[[j]] <- gsub("#.*", "", docs[[j]]) #Do not remove hastags
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
write.csv(corpus_df_unique_t, file="corpus_df_#environment-usa.csv")