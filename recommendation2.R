library(data.table)
library(dplyr)
library(tidytext)
library(ggplot2)
library(recommenderlab)
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

data_rec_2<- subset(data_reco,select=c(asin,reviewerID,n))

#data_rec_2$asin<-as.numeric(data_rec_2$asin)
#data_rec_2$n<-as.numeric(data_rec_2$n)

data_mtx<-as(data_rec_2,"realRatingMatrix")

data_mtx_1000 <- data_mtx

e <- evaluationScheme(data_mtx_1000, method="split", train=0.8, given=1 ,goodRating=2)

r1 <- Recommender(getData(e, "train"), "UBCF")

#p1 <- predict(r1, getData(e, "known"), type="ratings")

pre <- predict(r1, data_mtx_1000, n = 2)

as(pre, "list")

#as(p1, "matrix")[,1:10]

error <- rbind(rbind(UBCF = calcPredictionAccuracy(p1, getData(e, "unknown"))))

