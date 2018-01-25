library(data.table)
library(dplyr)
library(tidytext)
library(ggplot2)
library(recommenderlab)
df <- fread("C:/Users/faolinv2/Documents/projet-R/data_transformer.csv")

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

#tracer de bing, on visualise le score de chaque item du set random

bing_word_counts %>%
  filter(n > 10) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(asin = reorder(asin, n)) %>%
  ggplot(aes(asin, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Contribution to sentiment")

#tracer du nrc les couleurs représentent chaque sentiment exprimé par type de produit
nrc_word_counts %>%
  filter(n > 10) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(asin = reorder(asin, n)) %>%
  ggplot(aes(asin, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Contribution to sentiment")

#système de recommendation

data_reco<-merge (df1,bing_word_counts,by="asin",all.x=TRUE)

data_mtx<-as(data_reco,"realRatingMatrix")

data_mtx_1000 <- data_mtx[1:1000]

e <- evaluationScheme(data_mtx_1000, method="split", train=0.8, given=1 ,goodRating=NA)

r1 <- Recommender(data_mtx_1000, method = "UBCF")
pre <- predict(r1, data_mtx_1000, n = 2)

as(pre, "list")
p1 <- predict(r1, getData(e, "known"), type="ratings")

as(p1, "matrix")[,1:10]

error <- rbind(rbind(UBCF = calcPredictionAccuracy(p1, getData(e, "unknown"))))
