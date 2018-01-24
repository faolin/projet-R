library(data.table)
library(dplyr)
library(tidytext)
df <- fread("C:/Users/Totus/Desktop/frnaçois/projet-R/data_transformer.csv")

N <- floor(nrow(df) * (1/100))
df1 <- df[sample(1:nrow(df),N),]

#sentiment analysis

df_tidy <- df1
df_tidy$Text <- as.character(df_tidy$Text)
df_tidy <- df_tidy %>%
  unnest_tokens(word, Text)

df_tidy$linenumber <- NA
nrowasin_tidy <- nrow(df_tidy)

df$linenumber <- 1:nrow(df_tidy)

#methode nrc
nrc<-get_sentiments("nrc")

nrc_word_counts <- df_tidy %>%
  inner_join(nrc) %>%
  count(asin, sentiment, sort = TRUE) %>%
  ungroup()

#methode bing

bing <- get_sentiments("bing")

bing_word_counts <- df_tidy %>%
  inner_join(bing) %>%
  count(asin, sentiment, sort = TRUE) %>%
  ungroup()

#système de recommendation
