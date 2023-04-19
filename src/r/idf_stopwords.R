rm(list = ls())

suppressMessages(library(pbmcapply))
suppressMessages(library(stringr))
suppressMessages(library(tonR))
suppressMessages(library(dplyr))
suppressMessages(library(tidytext))
suppressMessages(library(quanteda))
suppressMessages(library(readr))

# read.conll("../../gitDebates/talk-of-norway", "tale000043", )[, c("id", "lemma")]

suppressWarnings(suppressMessages(meta <- read_csv("../../gitDebates/talk-of-norway/data/ton_updated.csv")))


tmp_anot <- pbmclapply(meta$id, function(x){
  read.conll("../../gitDebates/talk-of-norway", x)[, c("id", "lemma")]
}, mc.cores = detectCores()-2)

tmp_anot <- bind_rows(tmp_anot)


tmp_dfm <- tmp_anot %>% 
  count(id, lemma) %>% 
  filter(str_detect(lemma, "[[:punct:]]") == FALSE) %>% 
  filter(str_detect(lemma, "[0-9]+") == FALSE) %>% 
  bind_tf_idf(lemma, id, n) %>% 
  group_by(lemma) %>% 
  summarize(idf = unique(idf)) %>% 
  arrange(idf)

writeLines(as.character(tmp_dfm$lemma[1:1000]), con = "data/stopwords.txt")
