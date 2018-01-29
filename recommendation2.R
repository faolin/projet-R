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

data_rec_2<- subset(data_reco,select=c(asin,reviewerID,n))
data_rec_3 <- as.matrix(data_rec_2)
#data_rec_2$asin<-as.numeric(data_rec_2$asin)
data_rec_3<- sapply(data.frame(data_rec_3),as.numeric)

data_mtx<- as(data_rec_3,"realRatingMatrix")

#e <- evaluationScheme(data_mtx_1000, method="split", train=0.8, given=1 ,goodRating=3)
#as(e, "list")
r1 <- Recommender(data_mtx, "UBCF")

p1 <- predict(r1, data_mtx, type="ratings")
as(p1, "matrix")
pre <- predict(r1, data_mtx, n = 1)
pre
as(pre, "list")

#recommandation avec nrc
data_reco_nrc<-merge (df1,nrc_word_counts,by="asin",all.x=TRUE)

data_rec_nrc_2<- subset(data_reco_nrc,select=c(asin,reviewerID,n))
data_rec_nrc_3 <- as.matrix(data_rec_nrc_2)
#data_rec_2$asin<-as.numeric(data_rec_2$asin)
data_rec_nrc_3<- sapply(data.frame(data_rec_nrc_3),as.numeric)

data_mtx_nrc <- as(data_rec_nrc_3,"realRatingMatrix")

r1_nrc <- Recommender(data_mtx_nrc, "UBCF")

p1_nrc <- predict(r1_nrc, data_mtx_nrc, type="ratings")
as(p1_nrc, "matrix")