library(haven)
library(tidyverse)
ffc.stata <- read_dta(file = file.choose())


# Creating histograms for US environmental Country-related question ----------
us_response_country <- ffc.stata %>%
  filter(c_alphan == "US") %>%
  mutate(v16_numeric = as.numeric(v16)) %>%
  group_by(v16_numeric) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(responses = case_when(v16_numeric == 1 ~ "air pollution",
                               v16_numeric == 2 ~ "chemicals and pesticides",
                               v16_numeric == 3 ~ "water shortage",
                               v16_numeric == 4 ~ "water pollution",
                               v16_numeric == 5 ~ "nuclear waste",
                               v16_numeric == 6 ~ "domestic waste disposal",
                               v16_numeric == 7 ~ "climate change",
                               v16_numeric == 8 ~ "genetically modified foods",
                               v16_numeric == 9 ~ "using up natural resources",
                               v16_numeric == 10 ~ "none of these",
                               v16_numeric == 98 ~ "can't choose",
                               v16_numeric == 99 ~ "no answer")) %>%
  filter(count >100) %>%
  filter(v16_numeric < 98) %>%
  mutate(responses = factor(responses, levels = c("using up natural resources", "air pollution", "water pollution", "climate change", "chemicals and pesticides")))

us_response_country %>%
  ggplot() +
  geom_bar(aes(x = responses, y = count), stat = "identity") +
  labs(x = "Response Code",
       y = "Number of Responses",
       title = "Country: Number of Responses by Topic") +
  theme_bw()

# Creating histograms for US environmental personal-related question ----------

us_response_personal <- ffc.stata %>%
  filter(c_alphan == "US") %>%
  mutate(v17_numeric = as.numeric(v17)) %>%
  filter(v17_numeric < 98) %>%
  group_by(v17_numeric) %>%
  summarize(count = n()) %>%
  mutate(responses = case_when(v17_numeric == 1 ~ "air pollution",
                               v17_numeric == 2 ~ "chemicals and pesticides",
                               v17_numeric == 3 ~ "water shortage",
                               v17_numeric == 4 ~ "water pollution",
                               v17_numeric == 5 ~ "nuclear waste",
                               v17_numeric == 6 ~ "domestic waste disposal",
                               v17_numeric == 7 ~ "climate change",
                               v17_numeric == 8 ~ "gm foods",
                               v17_numeric == 9 ~ "using up natural resources",
                               v17_numeric == 10 ~ "none of these")) %>%
  filter(count >100) %>%
  filter(v17_numeric != 10) %>%
  mutate(responses = factor(responses, levels = c("air pollution", "chemicals and pesticides", "using up natural resources", "gm foods", "water pollution")))

us_response_personal %>%
  ggplot() +
  geom_bar(aes(x = responses, y = count), stat = "identity") +
  labs(x = "Response Code",
       y = "Number of Responses",
       title = "Personal/Family: Number of Responses by Topic") +
  theme_bw()

# Creating histograms for England environmental Country-related question ----------

uk_response_country <- ffc.stata %>%
  filter(c_alphan == "GB-GBN") %>%
  mutate(v16_numeric = as.numeric(v16)) %>%
  filter(v16_numeric < 98) %>%
  group_by(v16_numeric) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(responses = case_when(v16_numeric == 1 ~ "air pollution",
                               v16_numeric == 2 ~ "chemicals and pesticides",
                               v16_numeric == 3 ~ "water shortage",
                               v16_numeric == 4 ~ "water pollution",
                               v16_numeric == 5 ~ "nuclear waste",
                               v16_numeric == 6 ~ "domestic waste disposal",
                               v16_numeric == 7 ~ "climate change",
                               v16_numeric == 8 ~ "genetically modified foods",
                               v16_numeric == 9 ~ "using up natural resources",
                               v16_numeric == 10 ~ "none of these")) %>%
  filter(count >70) %>%
  mutate(responses = factor(responses, levels = c("climate change", "using up natural resources", "domestic waste disposal", "water shortage", "air pollution")))

uk_response_country %>%
  ggplot() +
  geom_bar(aes(x = responses, y = count), stat = "identity") +
  labs(x = "Response Code",
       y = "Number of Responses",
       title = "Country: Number of Responses by Topic") +
  theme_bw()


# Creating histograms for England environmental personal-related question ----------

uk_response_personal <- ffc.stata %>%
  filter(c_alphan == "GB-GBN") %>%
  mutate(v17_numeric = as.numeric(v17)) %>%
  filter(v17_numeric < 98) %>%
  group_by(v17_numeric) %>%
  summarize(count = n()) %>%
  mutate(responses = case_when(v17_numeric == 1 ~ "air pollution",
                               v17_numeric == 2 ~ "chemicals and pesticides",
                               v17_numeric == 3 ~ "water shortage",
                               v17_numeric == 4 ~ "water pollution",
                               v17_numeric == 5 ~ "nuclear waste",
                               v17_numeric == 6 ~ "domestic waste disposal",
                               v17_numeric == 7 ~ "climate change",
                               v17_numeric == 8 ~ "gm foods",
                               v17_numeric == 9 ~ "using up natural resources",
                               v17_numeric == 10 ~ "none of these")) %>%
  filter(count >40) %>%
  filter(v17_numeric != 10) %>%
  mutate(responses = factor(responses, levels = c("domestic waste disposal", "air pollution", "using up natural resources", "climate change", "gm foods")))

uk_response_personal %>%
  ggplot() +
  geom_bar(aes(x = responses, y = count), stat = "identity") +
  labs(x = "Response Code",
       y = "Number of Responses",
       title = "Personal/Family: Number of Responses by Topic") +
  theme_bw()



# Creating histograms for US climate change question ----------

cc_country <- ffc.stata %>%
  filter(c_alphan == "US") %>%
  mutate(v43_numeric = as.numeric(v43)) %>%
  group_by(v43_numeric) %>%
  summarize(count = n()) %>%
  mutate(responses = case_when(v43_numeric == 1 ~ "extremely dangerous",
                               v43_numeric == 2 ~ "very dangerous",
                               v43_numeric == 3 ~ "somewhat dangerous",
                               v43_numeric == 4 ~ "not very dangerous",
                               v43_numeric == 5 ~ "not at all dangerous",
                               v43_numeric == 8 ~ "can't choose",
                               v43_numeric == 9 ~ "no answer")) %>%
  filter(v43_numeric != 8) %>%
  filter(v43_numeric != 9) %>%
  mutate(responses = factor(responses, levels = c("extremely dangerous", "very dangerous", "somewhat dangerous", "not very dangerous", "not at all dangerous")))
  

cc_country %>%
  ggplot() +
  geom_bar(aes(x = responses, y = count), stat = "identity") +
  labs(x = "Response Code",
       y = "Number of Responses",
       title = "Number of Responses by Topic") +
  theme_bw()
