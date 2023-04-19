# Starting with clean environment
rm(list = ls())

# Loading packages
suppressMessages(library(parallel))     # For parallel processes
suppressMessages(library(stringr))      # Manipulate text
suppressMessages(library(tonR))         # Reading ToN data
suppressMessages(library(dplyr))        # Faster data pre-processing
suppressMessages(library(tidytext))     # Faster text pre-processing
suppressMessages(library(quanteda))     # Text data utilities
suppressMessages(library(readr))        # Faster read/write of data
suppressMessages(library(caret))        # Classification models
suppressMessages(library(tictoc))       # Timing functions

# Reading ToN data
suppressWarnings(suppressMessages(meta <- read_csv("../../gitDebates/talk-of-norway/data/ton_updated.csv")))

# Reading stopwords
stwords <- readLines("data/stopwords.txt")[1:100]

# Tuning control for GBM (no tuning)
tr <- trainControl(method = "none",
                   classProbs = TRUE,
                   verboseIter = TRUE) 

# Extracting parliamentary sessions
sessions <- list.dirs("data/folds", recursive = FALSE)
# i = sessions[4]
# Looping over sessions
for(i in sessions){
  message(paste0("\n=========================================\n",
                 "\tStarting session ", gsub("data/folds/", "", i),
                 "\n=========================================\n"))
  
  # Making sure all folders are in place for session
  # dir.create(paste0("data/models/0baseline/", gsub("data/folds/", "", i)), recursive = TRUE, showWarnings = FALSE)
  dir.create(paste0("data/nnet_estimations/preds/0baseline/", gsub("data/folds/", "", i)), recursive = TRUE, showWarnings = FALSE)
  dir.create(paste0("data/nnet_estimations/prob_grids/0baseline/", gsub("data/folds/", "", i)), recursive = TRUE, showWarnings = FALSE)
  
  done_folds <- list.files(paste0("data/nnet_estimations/preds/0baseline/", gsub("data/folds/", "", i)))
  
  if(length(done_folds) == 10){
    message("Session already estimated")
    next()
  }
  
  # Listing folds for session
  tests <- list.files(paste0(i, "/test"))
  trains <- list.files(paste0(i, "/train"))
  
  # Reading ids for test folds
  test <- list()
  for(j in tests){
    test[[j]] <- readLines(paste0(i, "/test/", j))
  }
  
  # Reading ids for train folds
  train <- list()
  for(j in trains){
    train[[j]] <- readLines(paste0(i, "/train/", j))
    
  }
  
  # Also subsetting the relevant rows from meta to save time later
  tmp_meta <- meta %>% 
    filter(id %in% c(test[[1]], train[[1]]))
  
  # Reading annotated speeches for session
  tic(paste0("\t Reading annotated files for ", gsub("data/folds/", "", i)))
  tmp_anot <- mclapply(c(test[[1]], train[[1]]), function(x){
    read.conll("../../gitDebates/talk-of-norway/", x)
  }, mc.cores = detectCores()-1)
  toc()
  message("\n")
  
  # Binding all speeches together in long format
  tmp_anot <- bind_rows(tmp_anot)
  
  # Making document-feature matrix
  tmp_dfm <- tmp_anot %>% 
    mutate(token = tolower(token)) %>%                                 # Lowercasing tokens
    filter(str_detect(token, "[[:punct:]]") == FALSE) %>%              # Removing punctuation
    filter(str_detect(token, "[0-9]+") == FALSE) %>%                   # Removing numbers
    filter(token %in% stwords == FALSE) %>%                            # Removing stopwords
    filter(token != "" & str_detect(token, "^\\s+$") == FALSE)  %>%    # Cleaning som noise
    filter(str_detect(token, "\\s") == FALSE) %>%                      # Cleaning som noise
    mutate(token = char_wordstem(token, language = "norwegian")) %>%   # Stemming tokens
    count(id, token) %>%                                               # Counting tokens
    group_by(id) %>% filter(sum(n) > 50) %>% ungroup() %>%             # Removing documents shorter than 50 tokens
    group_by(token) %>% filter(sum(n) > 20) %>% ungroup() %>%          # Removing tokens with frequency lower than 20
    bind_tf_idf(token, id, n) %>%                                      # Calculating tf-idf
    cast_dfm(id, token, tf_idf)
  
  
  # Converting to data frame for modelling
  tmp_dfm <- convert(tmp_dfm, to = "data.frame")
  
  # Removing annotations to save memory
  rm(tmp_anot)
  
  preds <- lapply(1:length(train), function(x){
    
    # Extracting fold name
    fold <- gsub(".txt", "", tests[x])
    
    # Making train set for model in the fold
    ton_train <- tmp_dfm[which(tmp_dfm$document %in% train[[x]]), ]
    ton_train <- left_join(ton_train, tmp_meta[, c("id", "party_id")],
                           by = c("document" = "id"))
    rownames(ton_train) <- ton_train$document
    ton_train$document <- NULL
    
    # Making test set for model in the fold
    ton_test <- tmp_dfm[which(tmp_dfm$document %in% test[[x]]), ]
    ton_test <- left_join(ton_test, tmp_meta[, c("id", "party_id")],
                          by = c("document" = "id"))
    rownames(ton_test) <- ton_test$document
    ton_test$document <- NULL
    
    # Constructing weights based on formula from scikit learn (python) package
    wt <- nrow(ton_train) / (length(unique(ton_train$party_id)) * table(ton_train$party_id))
    
    # Assigning weights
    wt <- ifelse(ton_train$party_id == "A", wt["A"],
                 ifelse(ton_train$party_id == "FrP", wt["FrP"],
                        ifelse(ton_train$party_id == "H", wt["H"],
                               ifelse(ton_train$party_id == "KrF", wt["KrF"],
                                      ifelse(ton_train$party_id == "Sp", wt["Sp"],
                                             ifelse(ton_train$party_id == "SV", wt["SV"],
                                                    ifelse(ton_train$party_id == "V", wt["V"], NA)))))))
    tg <- expand.grid(size = 0,
                      decay = 0)
    
    # Estimating model
    message(paste0("--------------------------------\n",
                   "Estimating ", fold, " for ", gsub("data/folds/", "", i),
                   "\n--------------------------------"))
    tic()
    tmp_grid <- train(party_id ~ ., data = ton_train,
                      method = "nnet", 
                      trControl = tr,
                      tuneGrid = tg,
                      weights = wt,
                      skip = TRUE,
                      MaxNWts = 100000,
                      maxit = 1000)
    tikktakk <- toc(log = TRUE, quiet = TRUE)
    message(paste0("Model estimation time: ", 
                   ifelse(tikktakk$toc - tikktakk$tic > 60, 
                          paste(round((tikktakk$toc - tikktakk$tic) / 60, digits = 1), "min"), 
                          paste(round(tikktakk$toc - tikktakk$tic, digits = 0), "sec")),
                   "\n--------------------------------\n"))
    
    # Producing a grid of probabilies based on model for the test set
    prob_grid <- data.frame(id = rownames(ton_test),
                            party_id = ton_test$party_id,
                            predict(tmp_grid, ton_test[, -ncol(ton_test)], 
                                    type = "prob"))
    # Producing class predictions based on model for the test set
    pred <- data.frame(id = rownames(ton_test),
                       party_id = ton_test$party_id,
                       party_pred = predict(tmp_grid, ton_test[, -ncol(ton_test)]),
                       stringsAsFactors = FALSE)
    
    # Printing F-scores to console
    message(paste0("Results for ", gsub("data/folds/", "", i), ", ", fold, ":"))
    print(Fscores_maker(data = pred, actual = "party_id", pred = "party_pred"))
    
    # Writing predictions to file
    write_csv(pred, path = paste0("data/nnet_estimations/preds/0baseline/", gsub("data/folds/", "", i), "/", fold, ".csv"))
    write_csv(prob_grid, path = paste0("data/nnet_estimations/prob_grids/0baseline/", gsub("data/folds/", "", i), "/", fold, ".csv"))
    # write_rds(tmp_grid, path = paste0("data/models/0baseline/", gsub("data/folds/", "", i), "/", fold, ".rds"))
    
    rm(prob_grid, pred, tmp_grid, wt, tg, ton_test, ton_train, fold)
    
  })
}