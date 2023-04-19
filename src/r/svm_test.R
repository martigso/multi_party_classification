rm(list = ls())

suppressMessages(library(pbmcapply))
suppressMessages(library(stringr))
suppressMessages(library(tonR))
suppressMessages(library(dplyr))
suppressMessages(library(tidytext))
suppressMessages(library(quanteda))
suppressMessages(library(readr))
suppressMessages(library(kernlab))

suppressWarnings(suppressMessages(meta <- read_csv("../../gitDebates/talk-of-norway/data/ton_updated.csv")))

stwords <- readLines("data/stopwords.txt")[1:100]

sessions <- list.dirs("data/folds", recursive = FALSE)

svm_results <-list()

for(i in sessions){
  
  message(paste("Starting session", gsub("data/folds/", "", i)))
  
  tests <- list.files(paste0(i, "/test"))
  trains <- list.files(paste0(i, "/train"))
  
  test <- list()
  for(j in tests){
    test[[j]] <- readLines(paste0(i, "/test/", j))
  }
  
  train <- list()
  for(j in trains){
    train[[j]] <- readLines(paste0(i, "/train/", j))
    
    
    tmp_meta <- meta %>% 
      filter(id %in% c(test[[1]], train[[1]]))
  }
  
  message(paste0("\t Reading annotated files for ", gsub("data/folds/", "", i), ":"))
  tmp_anot <- pbmclapply(c(test[[1]], train[[1]]), function(x){
    read.conll("../../gitDebates/talk-of-norway/", x)
  }, mc.cores = detectCores()-2, ignore.interactive = TRUE)
  
  tmp_anot <- bind_rows(tmp_anot)
  
  
  tmp_dfm <- tmp_anot %>% 
    mutate(token = tolower(token)) %>%
    filter(str_detect(token, "[[:punct:]]") == FALSE) %>% 
    filter(str_detect(token, "[0-9]+") == FALSE) %>% 
    filter(token %in% stwords == FALSE) %>% 
    filter(token != "" & str_detect(token, "^\\s+$") == FALSE)  %>% 
    filter(str_detect(token, "\\s") == FALSE) %>% 
    mutate(token = char_wordstem(token, language = "norwegian")) %>% 
    count(id, token) %>% 
    cast_dfm(id, token, n) %>% 
    dfm_trim(min_docfreq = 20, min_termfreq = 5)
  
  tmp_dfm <- convert(tmp_dfm, to = "data.frame")
  # summary(colSums(tmp_dfm[, 2:ncol(tmp_dfm)]))
  # tmp_dfm[1:10, 1:10]
  
  # x = 2
  # lapply here
  preds <- lapply(1:length(train), function(x){
    
    ton_train <- tmp_dfm[which(tmp_dfm$document %in% train[[x]]), ]
    ton_train <- left_join(ton_train, tmp_meta[, c("id", "party_id")],
                           by = c("document" = "id"))
    rownames(ton_train) <- ton_train$document
    ton_train$document <- NULL
    
    # ton_test[1:10, 1:10]
    ton_test <- tmp_dfm[which(tmp_dfm$document %in% test[[x]]), ]
    ton_test <- left_join(ton_test, tmp_meta[, c("id", "party_id")],
                          by = c("document" = "id"))
    rownames(ton_test) <- ton_test$document
    ton_test$document <- NULL
    
    message(paste0("\t Estimating SVM ", x, " of ", length(train)))
    
    tmp_mod <- ksvm(party_id ~ ., data = ton_train,
                    kernel = "vanilladot",
                    C = 1, scaled = FALSE)
    
    pred <- data.frame(id = rownames(ton_test),
                       party_id = ton_test$party_id,
                       party_pred = predict(tmp_mod, ton_test),
                       stringsAsFactors = FALSE)
    
    
    message(paste0("\t Macro F = ", round(Fscores_maker(data = pred, actual = "party_id", pred = "party_pred")$f1_macro, digits = 2)))
    
    return(pred)
  })
  preds <- bind_rows(preds)
  
  message(paste0("Results for ", gsub("data/folds/", "", i)))
  
  print(Fscores_maker(data = preds, actual = "party_id", pred = "party_pred"))
  
  preds$session <- gsub("data/folds/", "", i)
  
  svm_results[[gsub("data/folds/", "", i)]] <- preds
}

save(svm_results, file = "data/tmp_svmresults.rda")