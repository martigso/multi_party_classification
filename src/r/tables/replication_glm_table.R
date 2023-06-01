# Claring environment
rm(list = ls())

# Loading packages
library(tonR)
library(readr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(stringr)
library(pbmcapply)
library(tidyr)
library(multiwayvcov)

# Load book chapter data
load("../../pol_legdebate_book/data/choice_set.rda")

# Load Talk of Norway
suppressWarnings(suppressMessages(meta <- read_csv("../../gitDebates/talk-of-norway/data/ton_updated.csv")))

# Converting duration to years
choice_set$rep_cumdur <- choice_set$rep_cumdur / 365.25
choice_set$rep_cumdur_term <- choice_set$rep_cumdur / 4
choice_set$age_sq <- choice_set$rep_age^2
choice_set$rep_cumdur_sq <- choice_set$rep_cumdur_term^2
choice_set$speech_com_role2 <- factor(choice_set$speech_com_role_cat,
                                      levels = c("Medlem","Nestleder", "Leder", "Ikke medlem"))
choice_set$prev_speeches_log <- log(choice_set$prev_speeches+1)

original <- glm(rep_speechholder_bin ~ speech_com_role2 +
                  prev_speeches_log + speech_parlleader + speech_partyleader + speech_listnum +
                  rep_age + age_sq + rep_cumdur_term + rep_cumdur_sq + pool + speech_party_role +
                  party_id*rep_gender + parl_period,
                data = choice_set, family = binomial())## interactions

# Holder for mods
mods <- c("0baseline", "1lemma", "2lemma_pos", "3lemma_bigram", "4lemma_pos_bigram",
          "5lemma_trigram", "6lemma_pos_trigram", "88meta", "7lemma_meta", "8lemma_pos_meta",
          "9lemma_pos_bigram_meta", "99lemma_pos_trigram_meta")

regs <- lapply(mods[c(1, 2, 9, 10)], function(x){
  
  message(paste("Starting:", x))
  files <- c(list.files(paste0("./data/gbm_estimations/prob_grids/", x), full = TRUE, recursive = TRUE))
  
  raw <- lapply(files, function(y) suppressMessages(read_csv(y)))
  
  names(raw) <- files
  
  base <- bind_rows(raw, .id = "model")
  
  test <- left_join(base, meta[, c("id", "rep_id", "parl_period")],
                    by = "id")
  
  test <- melt(test, id.vars = c("model", "id", "rep_id", "party_id", "parl_period"))
  
  
  test <- test[which(test$variable == test$party_id), ]
  
  
  test2 <- test %>%
    filter(is.na(rep_id) == FALSE) %>% 
    group_by(party_id, parl_period) %>% 
    mutate(m_party = mean(value)) %>% 
    ungroup() %>% 
    group_by(rep_id, party_id, parl_period) %>% 
    summarize(m = mean(value))
  
  test3 <- left_join(choice_set, test2,
                     by = c("rep_id" = "rep_id",
                            "party_id" = "party_id",
                            "parl_period" = "parl_period"))
  
  
  reg2 <- glm(rep_speechholder_bin ~ m + speech_com_role2 + 
                prev_speeches_log + speech_parlleader + speech_partyleader + speech_listnum +
                rep_age + age_sq + rep_cumdur_term + rep_cumdur_sq + pool + speech_party_role +
                party_id*rep_gender + parl_period,         
              data = test3, family = binomial())## interactions
  
  # cl <- makeCluster(1)
  # 
  # reg2_cl <- cluster.vcov(reg2, cbind(test3$rep_id, test3$id), 
  #                         parallel = cl)
  # coeftest(reg2, reg2_cl)
  
  return(reg2)

})

library(parallel)

cl <- makeCluster(1)

cl_original <- cluster.vcov(original, cbind(choice_set$rep_id, choice_set$id), 
                            parallel = cl)

cl_baseline <- cluster.vcov(regs[[1]], cbind(choice_set$rep_id, choice_set$id), 
                        parallel = cl)

cl_lemma <- cluster.vcov(regs[[2]], cbind(choice_set$rep_id, choice_set$id), 
                            parallel = cl)

cl_lemmameta <- cluster.vcov(regs[[3]], cbind(choice_set$rep_id, choice_set$id), 
                         parallel = cl)

cl_lemmapartyrole <- cluster.vcov(regs[[4]], cbind(choice_set$rep_id, choice_set$id), 
                         parallel = cl)

library(stargazer)

var_labels <- c("Cohesion", 
                "Deputy committee chair", "Committee chair", "Not committee member", 
                "Previous speeches (log)",
                "Parliamentary leader", "Party leader", 
                "List placement", 
                "Age","Age squared", 
                "Experience", "Experience squared",
                "N MPs", 
                "Opposition party", "Support party", 
                "Right wing party", "Conservatives", "Christian Democrats", "Center", "Socialist Left", "Liberals",
                "Male",
                "2001-2005", "2005-2009", "2009-2013","2013-2017", 
                "Right wing party*Male", "Conservatives*Male", "Christian Democrats*Male", "Center*Male", "Socialist Left*Male", "Liberals*Male",
                "Intercept")

restab <- stargazer(original, regs[[1]], regs[[2]], regs[[3]], regs[[4]],
                    se = list(cl_original, cl_baseline, cl_lemma, cl_lemmameta, cl_lemmapartyrole),
                    type = "latex",  star.char = "*",
                    dep.var.labels = "MP holding speech", 
                    column.labels = c("Original", "Baseline", "Lemma", "Lemma/Meta", "Lemma/Party role"),
                    star.cutoffs = 0.05,
                    covariate.labels = var_labels,
                    out = "./paper/tables/glm_reg.tex",
                    font.size = "tiny",
                    no.space = TRUE,
                    label = "tab:glm_reg1",
                    notes =  "* p<0.05", keep.stat = "n")

lapply(regs, \(x) x$coefficients[2])
