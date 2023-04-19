rm(list = ls())
library(readr)
library(tonR)
library(dplyr)

set.seed(4753)

suppressWarnings(suppressMessages(meta <- read_csv("../../gitDebates/talk-of-norway/data/ton_updated.csv") %>% 
                                    filter(speaker_role == "Representant") %>% 
                                    filter(party_id %in% c("A", "FrP", "H", "KrF", "Sp", "SV", "V")) %>% 
                                    select(id, session, party_id)))

sessions <- sort(unique(meta$session))


for(i in 1:length(sessions)){
  
  if(dir.exists(paste0("data/folds/", sessions[i], "/train/")) == FALSE){
    dir.create(paste0("data/folds/", sessions[i], "/train/"), recursive = T)
  }
  
  if(dir.exists(paste0("data/folds/", sessions[i], "/test/")) == FALSE){
    dir.create(paste0("data/folds/", sessions[i], "/test/"), recursive = T)
  }
  
  tmp <- meta[which(meta$session == sessions[i]), ]
  
  tmp$fold <- sample(paste0("fold", 0:9), nrow(tmp), replace = TRUE)
  
  lapply(sort(unique(tmp$fold)), function(x){
    test <- tmp[which(tmp$fold == x), ]
    train <- tmp[which(tmp$fold != x), ]
    
    writeLines(test$id, con = paste0("data/folds/", sessions[i], "/test/", x, ".txt"))
    writeLines(train$id, con = paste0("data/folds/", sessions[i], "/train/", x, ".txt"))
  })
}
