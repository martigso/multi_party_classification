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
suppressMessages(library(gbm))
suppressMessages(library(tictoc))       # Timing functions

# Reading ToN data
suppressWarnings(suppressMessages(meta <- read_csv("../../gitDebates/talk-of-norway/data/ton_updated.csv")))

meta <- meta %>% 
  filter(subject_committee_id == "ENERGI") %>% 
  filter(party_id != "Kp" & party_id != "TF" & party_id != "MDG")

set.seed(4753)

meta$fold <- sample(paste0("fold", 0:9), nrow(meta), replace = TRUE)
table(meta$fold, meta$party_id)

# Reading stopwords
stwords <- readLines("data/stopwords.txt")[1:100]

# Tuning control for GBM (no tuning)
tr <- trainControl(method = "none",
                   classProbs = TRUE,
                   verboseIter = TRUE) 

# Tuning grid for GBM (on average best performing tuning grid)
tg <- expand.grid(n.trees = c(100),
                  interaction.depth = c(2), 
                  shrinkage = 0.01, 
                  n.minobsinnode = 10)



# Making sure all folders are in place for session
# dir.create(paste0("data/models/1lemma/", gsub("data/folds_period/", "", i)), recursive = TRUE, showWarnings = FALSE)
dir.create(paste0("data/gbm_energy_estimations/preds/1lemma/"), recursive = TRUE, showWarnings = FALSE)
dir.create(paste0("data/gbm_energy_estimations/prob_grids/1lemma/"), recursive = TRUE, showWarnings = FALSE)
dir.create(paste0("data/gbm_energy_estimations/var_importance/1lemma/"), recursive = TRUE, showWarnings = FALSE)

done_folds <- list.files(paste0("data/gbm_energy_estimations/preds/1lemma/"))
  
if(length(done_folds) == 10){
  message("Already estimated")
  next()
}

# Reading annotated speeches for session
tic(paste0("\t Reading annotated files"))
tmp_anot <- mclapply(meta$id, function(x){
  read.conll("../../gitDebates/talk-of-norway/", x)
}, mc.cores = detectCores()-1)
toc()
message("\n")

# Binding all speeches together in long format
tmp_anot <- bind_rows(tmp_anot)

# Making document-feature matrix
tmp_dfm <- tmp_anot %>% 
  filter(str_detect(lemma, "[[:punct:]]") == FALSE) %>%              # Removing punctuation
  filter(str_detect(lemma, "[0-9]+") == FALSE) %>%                   # Removing numbers
  filter(lemma %in% stwords == FALSE) %>%                            # Removing stopwords
  filter(lemma != "" & str_detect(lemma, "^\\s+$") == FALSE)  %>%    # Cleaning som noise
  filter(str_detect(lemma, "\\s") == FALSE) %>%                      # Cleaning som noise
  count(id, lemma) %>%                                               # Counting lemmas
  group_by(id) %>% filter(sum(n) > 50) %>% ungroup() %>%             # Removing documents shorter than 50 lemmas
  group_by(lemma) %>% filter(sum(n) > 20) %>% ungroup() %>%          # Removing lemmas with frequency lower than 20
  bind_tf_idf(lemma, id, n) %>%                                      # Calculating tf-idf
  cast_dfm(id, lemma, tf_idf)


# Converting to data frame for modelling
tmp_dfm <- convert(tmp_dfm, to = "data.frame")
meta <- meta[which(meta$id %in% tmp_dfm$document), ]

# Removing annotations to save memory
rm(tmp_anot)

preds <- lapply(sort(unique(meta$fold)), function(x){
  
  gc(verbose = FALSE, reset = TRUE, full = TRUE)
  
  # Extracting fold name
  fold <- x
  
  if(any(grepl(fold, done_folds))){
    message(paste0(fold, " already done."))
  }
  
  # Making train set for model in the fold
  ton_train <- tmp_dfm[which(tmp_dfm$document %in% meta$id[which(meta$fold != fold)]), ]
  ton_train <- left_join(ton_train, meta[, c("id", "party_id")],
                         by = c("document" = "id"))
  rownames(ton_train) <- ton_train$document
  ton_train$document <- NULL
  
  # Making test set for model in the fold
  ton_test <- tmp_dfm[which(tmp_dfm$document %in% meta$id[which(meta$fold == fold)]), ]
  ton_test <- left_join(ton_test, meta[, c("id", "party_id")],
                        by = c("document" = "id"))
  rownames(ton_test) <- ton_test$document
  ton_test$document <- NULL
  
  # Constructing weights based on formula from scikit learn (python) package
  wt <- nrow(ton_train) / (length(unique(ton_train$party_id)) * table(ton_train$party_id))
  # wt <- ((table(ton_train$party_id) / nrow(ton_train)) * -1) + 1
  
  # Assigning weights
  wt <- ifelse(ton_train$party_id == "A", wt["A"],
               ifelse(ton_train$party_id == "FrP", wt["FrP"],
                      ifelse(ton_train$party_id == "H", wt["H"],
                             ifelse(ton_train$party_id == "KrF", wt["KrF"],
                                    ifelse(ton_train$party_id == "Sp", wt["Sp"],
                                           ifelse(ton_train$party_id == "SV", wt["SV"],
                                                  ifelse(ton_train$party_id == "V", wt["V"], NA)))))))
  
  # Estimating model
  message(paste0("--------------------------------\n",
                 "Estimating ", fold,
                 "\n--------------------------------"))
  tic()
  tmp_grid <- train(party_id ~ ., data = ton_train,
                    method = "gbm", 
                    trControl = tr,
                    tuneGrid = tg,
                    weights = wt)
  
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
  message(paste0("Results for ", fold, ":"))
  print(Fscores_maker(data = pred, actual = "party_id", pred = "party_pred"))
  
  # Generating variable importance
  imp <- data.frame(cbind(varImp(tmp_grid, scale = FALSE)$importance, varImp(tmp_grid)$importance))
  imp$var <- rownames(imp)
  rownames(imp) <- 1:nrow(imp)
  names(imp) <- c("unscaled", "scaled", "var")
  imp <- imp[, c("var", "unscaled", "scaled")] %>% 
    arrange(desc(unscaled), desc(scaled))
  imp <- imp[1:10, ]
  
  message(paste0("Variable importance:"))  
  print(imp)
  
  # Writing predictions to file
  write_csv(pred, path = paste0("data/gbm_energy_estimations/preds/1lemma/", fold, ".csv"))
  write_csv(prob_grid, path = paste0("data/gbm_energy_estimations/prob_grids/1lemma/", fold, ".csv"))
  write_csv(prob_grid, path = paste0("data/gbm_energy_estimations/var_importance/1lemma/", fold, ".csv"))

  rm(prob_grid, pred, tmp_grid, wt, ton_test, ton_train, fold)
  
})