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

# Tuning control for random forest (no tuning)
tr <- trainControl(method = "none",
                   classProbs = TRUE,
                   verboseIter = TRUE) 

# Extracting parliamentary sessions
sessions <- list.dirs("data/folds", recursive = FALSE)

# Looping over sessions
for(i in sessions){
  message(paste0("\n=========================================\n",
                 "\tStarting session ", gsub("data/folds/", "", i),
                 "\n=========================================\n"))
  
  # Making sure all folders are in place for session
  # dir.create(paste0("data/models/8lemma_pos_meta/", gsub("data/folds/", "", i)), recursive = TRUE, showWarnings = FALSE)
  dir.create(paste0("data/randomforest_estimations/preds/8lemma_pos_meta/", gsub("data/folds/", "", i)), recursive = TRUE, showWarnings = FALSE)
  dir.create(paste0("data/randomforest_estimations/prob_grids/8lemma_pos_meta/", gsub("data/folds/", "", i)), recursive = TRUE, showWarnings = FALSE)
  
  done_folds <- list.files(paste0("data/randomforest_estimations/preds/8lemma_pos_meta/", gsub("data/folds/", "", i)))
  
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
    filter(id %in% c(test[[1]], train[[1]])) %>% 
    mutate(case_type = ifelse(is.na(case_type) == TRUE, "andre", case_type))
  
  # Reading annotated speeches for session
  tic(paste0("\t Reading annotated files for ", gsub("data/folds/", "", i)))
  tmp_anot <- mclapply(c(test[[1]], train[[1]]), function(x){
    read.conll("../../gitDebates/talk-of-norway/", x)
  }, mc.cores = 1)
  toc()
  message("\n")
  
  # Binding all speeches together in long format
  tmp_anot <- bind_rows(tmp_anot)
  
  # Unigram pre-processing
  tmp_unigram <- tmp_anot %>% 
    mutate(lemma_pos = paste(lemma, part_of_speech, sep = "_")) %>% 
    filter(str_detect(lemma, "[[:punct:]]") == FALSE) %>%              # Removing punctuation
    filter(str_detect(lemma, "[0-9]+") == FALSE) %>%                   # Removing numbers
    filter(lemma %in% stwords == FALSE) %>%                            # Removing stopwords
    filter(lemma != "" & str_detect(lemma, "^\\s+$") == FALSE)  %>%    # Cleaning som noise
    filter(str_detect(lemma, "\\s") == FALSE) %>%                      # Cleaning som noise
    count(id, lemma_pos)                                               # Counting lemmas
  
  
  tmp_dfm <- tmp_unigram %>%
    arrange(id, n) %>% 
    group_by(id) %>% filter(sum(n) > 50) %>% ungroup() %>%                 # Removing documents shorter than 50 lemmas
    group_by(lemma_pos) %>% filter(sum(n) > 20) %>% ungroup() %>%          # Removing lemmas with frequency lower than 20
    bind_tf_idf(lemma_pos, id, n) %>%                                      # Calculating tf-idf
    cast_dfm(id, lemma_pos, tf_idf)
  
  
  
  # Converting to data frame for modelling
  tmp_dfm <- convert(tmp_dfm, to = "data.frame")
  # tmp_dfm[1:10, 1:10]
  
  # Meta variables
  meta_vars <- c("rep_gender", "county", "debate_type", "case_type", "party_role", "language")
  
  # gender and county of provenance are the speaker
  # level attributes, and type of debate (minutes, question hour, interpellations,
  #                                       and so on), keywords (for instance, ``taxes'', ``research'', ``immigration'' and
  #                                                             so on), the name of the committee leading the debate, and finally the type of
  # case (general issue, budget, law) are the debate level attributes.
  
  tmp_dfm <- left_join(tmp_dfm, tmp_meta[, c("id", meta_vars)],
                       by = c("document" = "id")) %>% 
    mutate(rep_gender = ifelse(is.na(rep_gender), "other", rep_gender),
           county = ifelse(is.na(county), "other", county),
           debate_type = ifelse(is.na(debate_type), "other", debate_type),
           debate_type = ifelse(debate_type == "referatsaker", "saksreferat", debate_type),
           debate_type = ifelse(debate_type == "formalia", "saksreferat", debate_type),
           debate_type = ifelse(debate_type == "voteringer", "saksreferat", debate_type),
           case_type = ifelse(is.na(case_type), "other", case_type),
           party_role = ifelse(is.na(party_role), "other", party_role),
           language = ifelse(is.na(language), "other", language))
  # table(tmp_dfm$county)
  # tmp_dfm$document[which(tmp_dfm$county == "other")]
  # meta$rep_name[which(meta$id == "tale194912")]
  # Fixing a corrupted cell in the data
  
  if(any(tmp_dfm$county == "other")){
    tmp_dfm$county[which(tmp_dfm$document == "tale090960")] <- "Rogaland"
    tmp_dfm$county[which(tmp_dfm$document == "tale097103")] <- "Sogn og Fjordane"
    tmp_dfm$county[which(tmp_dfm$document == "tale149381")] <- "Oslo"
    tmp_dfm$county[which(tmp_dfm$document == "tale162286")] <- "Hordaland"
    tmp_dfm$county[which(tmp_dfm$document == "tale194912")] <- "Sogn og Fjordane"
    tmp_dfm$county[which(tmp_dfm$document == "tale105813")] <- "Vestfold"
    tmp_dfm$county[which(tmp_dfm$document == "tale119035")] <- "Nord-Trøndelag"
    tmp_dfm$county[which(tmp_dfm$document == "tale199195")] <- "Troms"
    tmp_dfm$county[which(tmp_dfm$document == "tale216383")] <- "Hordaland"
    tmp_dfm$county[which(tmp_dfm$document == "tale239923")] <- "Akershus"
    tmp_dfm$county[which(tmp_dfm$document == "tale230637")] <- "Vestfold"
  }
  # Removing annotations to save memory
  rm(tmp_unigram, tmp_anot, meta_vars)
  
  
  preds <- lapply(1:length(train), function(x){
    
    gc(verbose = FALSE, reset = TRUE)
    
    # Extracting fold name
    fold <- gsub(".txt", "", tests[x])
    
    if(any(grepl(fold, done_folds))){
      message(paste0("Fold ", fold, " for ", i, " already done."))
      return()
    }
    
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
    # wt <- ((table(ton_train$party_id) / nrow(ton_train)) * -1) + 1
    
    # Assigning weights
    # wt <- ifelse(ton_train$party_id == "A", wt["A"],
    #              ifelse(ton_train$party_id == "FrP", wt["FrP"],
    #                     ifelse(ton_train$party_id == "H", wt["H"],
    #                            ifelse(ton_train$party_id == "KrF", wt["KrF"],
    #                                   ifelse(ton_train$party_id == "Sp", wt["Sp"],
    #                                          ifelse(ton_train$party_id == "SV", wt["SV"],
    #                                                 ifelse(ton_train$party_id == "V", wt["V"], NA)))))))
    # 
    # Tuning grid for random forest (on average best performing tuning grid)
    tg <- expand.grid(mtry = floor(sqrt(ncol(ton_train)-1)))
    
    
    # Estimating model
    message(paste0("--------------------------------\n",
                   "Estimating ", fold, " for ", gsub("data/folds/", "", i),
                   "\n--------------------------------"))
    tic()
    tmp_grid <- train(party_id ~ ., data = ton_train,
                      method = "rf", 
                      trControl = tr,
                      tuneGrid = tg,
                      do.trace = TRUE,
                      ntree = 200,
                      classwt = wt)
    
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
    write_csv(pred, path = paste0("data/randomforest_estimations/preds/8lemma_pos_meta/", gsub("data/folds/", "", i), "/", fold, ".csv"))
    write_csv(prob_grid, path = paste0("data/randomforest_estimations/prob_grids/8lemma_pos_meta/", gsub("data/folds/", "", i), "/", fold, ".csv"))
    # write_rds(tmp_grid, path = paste0("data/models/9full/", gsub("data/folds/", "", i), "/", fold, ".rds"))
  })
  
  rm(prob_grid, pred, tmp_grid, wt, ton_test, ton_train, fold)
  
}
