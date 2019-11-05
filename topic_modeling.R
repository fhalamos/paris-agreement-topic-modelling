library(tidyr)
library(dplyr)
library(tidytext)
library(textmineR)
library(reshape)

library(tm)
library(wordcloud)
library(RColorBrewer)

library(webshot)
library(htmlwidgets)


#Reference: https://towardsdatascience.com/beginners-guide-to-lda-topic-modelling-with-r-e57a5a8e7a25#https://www.kaggle.com/crowdflower/first-gop-debate-twitter-sentiment

#1.Load data
data <- read.csv(file.choose())

#select text and id column. looking at top 5000 rows
data <- data %>% select(id, text) %>% head(5000)

#2. Data cleaning

data$text <- sub("RT.*:", "", data$text)
data$text <- sub("@.* ", "", data$text)

text_cleaning_tokens <- data %>% 
  tidytext::unnest_tokens(word, text)

text_cleaning_tokens$word <- gsub('[[:digit:]]+', '', text_cleaning_tokens$word)
text_cleaning_tokens$word <- gsub('[[:punct:]]+', '', text_cleaning_tokens$word)
text_cleaning_tokens <- text_cleaning_tokens %>% filter(!(nchar(word) == 1))%>% 
  anti_join(stop_words)

#Lets remove "climate" "change" "climate_change" "http"
words_to_remove <- c("climate","change","climate_change", "climatechange", "http", "bitly")

text_cleaning_tokens <- 
  text_cleaning_tokens %>%
  filter(!grepl(paste(words_to_remove, collapse="|"), word))


tokens <- text_cleaning_tokens %>% filter(!(word==""))
tokens <- tokens %>% mutate(ind = row_number())
tokens <- tokens %>% group_by(id) %>% mutate(ind = row_number()) %>%
  tidyr::spread(key = ind, value = word)
tokens [is.na(tokens)] <- ""
tokens <- tidyr::unite(tokens, text,-id,sep =" " )
tokens$text <- trimws(tokens$text)




#3. Model building

#create DTM. matches documents-tweets in rows with terms in columns. 
dtm <- CreateDtm(tokens$text, 
                 doc_names = tokens$id, 
                 ngram_window = c(1, 2))

#explore the basic frequency
tf <- TermDocFreq(dtm = dtm)

#remove idf column
original_tf <- tf %>% select(term, term_freq,doc_freq)
#rename rows index with numbers
rownames(original_tf) <- 1:nrow(original_tf)


# Eliminate words appearing less than 2 times or in more than half of the
# documents... why morre than half?
vocabulary <- tf$term[ tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2 ]
dtm = dtm #??

#We set k = 20 (max number of possible topics)
k_list <- seq(1, 20, by = 1)

#Create model directory
model_dir <- paste0("models_", digest::digest(vocabulary, algo = "sha1"))
if (!dir.exists(model_dir)) dir.create(model_dir)

model_list <- TmParallelApply(X = k_list, FUN = function(k){
  filename = file.path(model_dir, paste0(k, "_topics.rda"))
  
  if (!file.exists(filename)) {
    m <- FitLdaModel(dtm = dtm, k = k, iterations = 500)
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  } else {
    load(filename)
  }
  
  m
}, export=c("dtm", "model_dir")) # export only needed for Windows machines


#model tuning
#choosing the best model
coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)

library(ggplot2)
ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,20,1)) + ylab("Coherence")


model <- model_list[which.max(coherence_mat$coherence)][[ 1 ]]
model$top_terms <- GetTopTerms(phi = model$phi, M = 20)
top20_wide <- as.data.frame(model$top_terms)


model$topic_linguistic_dist <- CalcHellingerDist(model$phi)
model$hclust <- hclust(as.dist(model$topic_linguistic_dist), "ward.D")
model$hclust$labels <- paste(model$hclust$labels, model$labels[ , 1])
plot(model$hclust)


#visualising topics of words based on the max value of phi
set.seed(1234)

final_summary_words <- data.frame(top_terms = t(model$top_terms))
final_summary_words$topic <- rownames(final_summary_words)
rownames(final_summary_words) <- 1:nrow(final_summary_words)

final_summary_words <- final_summary_words %>% melt(id.vars = c("topic"))
final_summary_words <- final_summary_words %>% rename(replace =c("value"="word")) %>% select(-variable)


final_summary_words <- final_summary_words %>% group_by(topic,word)

final_summary_words <- final_summary_words %>% group_by(topic, word) %>% filter(row_number() == 1) %>% 
  ungroup() %>% tidyr::separate(topic, into =c("t","topic")) %>% select(-t)

word_topic_freq <- left_join(final_summary_words, original_tf, by = c("word" = "term"))

#You need a images folder for this to work
for(i in 1:length(unique(word_topic_freq$topic)))
{
  
  filename <- paste(c(i,"_topic.png"), collapse = "")
  directory_and_filename <- paste(c("topic_modelling_images",filename), collapse = "/")
  
  png(directory_and_filename, width=12, height=8, units="in", res=300)
  
  wordcloud(words = subset(word_topic_freq ,topic == i)$word,
            freq = subset(word_topic_freq ,topic == i)$term_freq,
            min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  
  dev.off()
}






