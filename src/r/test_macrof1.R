rm(list = ls())

library(readr)
library(dplyr)
library(tonR)
library(stringr)
library(ggplot2)

files <- c(list.files("./data/gbm_energy_estimations/preds/1lemma/", full = TRUE, recursive = TRUE))

# files <- c(list.files("~/backups/cohesiveness/gbm_estimations/preds/7lemma_meta/1998-1999/", full = TRUE, recursive = TRUE))

raw <- lapply(files, function(x) suppressMessages(read_csv(x)))

names(raw) <- files

base <- bind_rows(raw, .id = "model")
Fscores_maker(base, "party_id", "party_pred")

# var(Fscores_maker(base, "party_id", "party_pred")$f1$f1)
# hm <- c(0.001992870, 0.002958528, 0.003029715, 0.002639972, 
#         0.002636935, 0.002514809, 0.002563486,0.004002102,
#         0.001938134, 0.001916300, 0.001709198, 0.001694849,0.003372424)
# 
# ggplot(NULL, aes(x = 1:13, y = hm)) + geom_point() + geom_text(aes(label = c("Baseline", "Lemma", "", "", "", "",
#                                                                              "Lemma/PoS/Trigram", "Meta", "Lemma/Meta",
#                                                                              "", "", "", "Party role")), vjust = 2)


# base$fold <- str_remove(sapply(str_split(base$model, "\\/"), "[[", 8), "\\.csv")
base$session <- sapply(str_split(base$model, "\\/"), "[[", 6)
base$estimation <- sapply(str_split(base$model, "\\/"), "[[", 3)
base$model <- sapply(str_split(base$model, "\\/"), "[[", 5)

model_f1 <- lapply(unique(base$session), function(x){
  
  tmp <- base[which(base$session == x), ]
  
  f1 <- Fscores_boot(tmp, "party_id", "party_pred", sample_divider = 2, nsim = 1000, seed = 854, cores = detectCores()-2)
  f1$f1_sims_conf$estimation <- x
  f1$f1_sims_conf$f1_macro_lwr <- f1$f1_macro_conf[1]
  f1$f1_sims_conf$f1_macro_m <- f1$f1_macro_conf[2]
  f1$f1_sims_conf$f1_macro_upr <- f1$f1_macro_conf[3]
  f1$f1_sims_conf$accuracy <- f1$f1_full_data$accuracy

  return(f1$f1_sims_conf)
})

model_f1 <- bind_rows(model_f1)


ggplot(model_f1, aes(x = substr(estimation, 1, 4), y = m, group = labs)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) + 
  geom_line() +
  facet_wrap(~ labs) + 
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  theme_classic()+
  #geom_hline(aes(yintercept = f1_macro_m), linetype = "dashed") +
  #geom_hline(aes(yintercept = f1_macro_lwr), linetype = "dashed", color = "gray70") +
  #geom_hline(aes(yintercept = f1_macro_upr), linetype = "dashed", color = "gray70") +
  theme(panel.grid.major.y = element_line(linetype = "solid", color = "gray70"))
