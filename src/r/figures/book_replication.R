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
library(lmtest)
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

# Holder for mods
mods <- c("0baseline", "1lemma", "2lemma_pos", "3lemma_bigram", "4lemma_pos_bigram",
          "5lemma_trigram", "6lemma_pos_trigram", "88meta", "7lemma_meta", "7lemma_meta_party_role", "8lemma_pos_meta",
          "9lemma_pos_bigram_meta", "99lemma_pos_trigram_meta")
x = mods[1]
lapply(mods, function(x){
  
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
  
  
  # reg2 <- glm(rep_speechholder_bin ~ speech_com_role2 +
  #               prev_speeches_log + speech_parlleader + speech_partyleader + speech_listnum +
  #               rep_age + age_sq + rep_cumdur_term + rep_cumdur_sq + pool + speech_party_role +
  #               party_id*rep_gender + parl_period,
  #             data = choice_set, family = binomial())## interactions
  reg2 <- glm(rep_speechholder_bin ~ m + speech_com_role2 + 
                prev_speeches_log + speech_parlleader + speech_partyleader + speech_listnum +
                rep_age + age_sq + rep_cumdur_term + rep_cumdur_sq + pool + speech_party_role +
                party_id*rep_gender + parl_period,         
              data = test3, family = binomial())## interactions
  
  require(parallel)
  cl <- makeCluster(1)
  
  reg2_cl <- cluster.vcov(reg2, cbind(test3$rep_id, test3$id), 
                          parallel = cl)
  coeftest(reg2, reg2_cl)
  # 1.81767454 = baseline
  # 0.43826203 = lemma
  # 1.78173013 = lemma_meta
  # 1.14175407 = partyrole
  
  regplot <- data.frame(cov = names(coef(reg2)),
                        coefs = coef(reg2),
                        lwr = coef(reg2) - 1.96 * sqrt(diag(reg2_cl)),
                        upr = coef(reg2) + 1.96 * sqrt(diag(reg2_cl)),
                        row.names = NULL, stringsAsFactors = FALSE)
  
  # Mean confidence base
  m_base <- sum(
    regplot$coefs[which(regplot$cov == "(Intercept)")], 
    regplot$coefs[which(regplot$cov == "prev_speeches_log")]           * median(test3$prev_speeches_log), 
    regplot$coefs[which(regplot$cov == "speech_listnum")]              * median(test3$speech_listnum),  
    regplot$coefs[which(regplot$cov == "rep_age")]                     * median(test3$rep_age, na.rm = T), 
    regplot$coefs[which(regplot$cov == "age_sq")]                      * median(test3$age_sq, na.rm = T), 
    regplot$coefs[which(regplot$cov == "rep_cumdur_term")]             * median(test3$rep_cumdur_term), 
    regplot$coefs[which(regplot$cov == "rep_cumdur_sq")]               * median(test3$rep_cumdur_sq), 
    regplot$coefs[which(regplot$cov == "pool")]                        * median(test3$pool), 
    regplot$coefs[which(regplot$cov == "speech_party_roleOpposition")] * 1, 
    regplot$coefs[which(regplot$cov == "parl_period2009-2013")]        * 1,
    regplot$coefs[which(regplot$cov == "rep_gendermann")]              * 1
  )

  
  m_iter <- seq(0.15, 0.5, 0.025)
  m_list <- list()
  for(i in 1:length(m_iter)){
    m_list[[i]] <- data.frame(cohesion = m_iter[i],
                              fit = m_base + regplot$coef[which(regplot$cov == "m")] * m_iter[i],
                              lwr = m_base + regplot$lwr[which(regplot$cov == "m")] * m_iter[i],
                              upr = m_base + regplot$upr[which(regplot$cov == "m")] * m_iter[i])
  }
  
  pred_data <- bind_rows(m_list)
  
  
  pred_data[, c("fit", "lwr", "upr")] <- apply(pred_data[, c("fit", "lwr", "upr")], 2, function(x) exp(x) / (1+exp(x)))
  
  
  ggplot(pred_data, aes(x = cohesion, y = fit)) + 
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = .2) +
    geom_line() +
    scale_y_continuous(limits = c(.2, 0.6), breaks = seq(0, 1, 0.05), expand = c(0, 0)) +
    scale_x_continuous(limits = c(0.12, 0.525), breaks = seq(0, 1, .05), expand = c(0, 0)) +
    labs(y = "Predicted probability", x = "Mean classification probability",
         subtitle = paste0("Logit slope: ", round(regplot$coef[which(regplot$cov == "m")], 2), " (", round(sqrt(diag(reg2_cl))[2], 2), ")")) +
    theme_classic() + 
    theme(panel.grid.major = element_line(color = "gray95"))
  ggsave(paste0("./paper/figures/book_replication/", x, ".pdf"))
  
})