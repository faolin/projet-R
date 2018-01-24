library(data.table)
library(dplyr)
library(tidytext)
df <- fread("C:/Users/faolinv2/Documents/projet-R/data_transformer.csv")

N <- floor(nrow(df) * (50/100))
df1 <- df[sample(1:nrow(df),N),]



df_tidy <- df1
df_tidy$Text <- as.character(df_tidy$Text)
df_tidy <- df_tidy %>%
  unnest_tokens(word, Text)

df_tidy$linenumber <- NA
nrowasin_tidy <- nrow(df_tidy)

df$linenumber <- 1:nrow(df_tidy)


