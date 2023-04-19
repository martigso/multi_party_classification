rm(list = ls())

library(readr)
library(dplyr)
library(tonR)
library(stringr)
library(ggplot2)
library(pbmcapply)

files <- c(list.files("./data/gbm_estimations/preds", full = TRUE, recursive = TRUE))

raw <- pbmclapply(files, function(x) suppressMessages(read_csv(x)),
                  mc.cores = detectCores()-1)

names(raw) <- files

base <- bind_rows(raw, .id = "model")

base$session <- str_extract(base$model, "[0-9]{4}\\-[0-9]{4}")
base$model <- sapply(str_split(base$model, "\\/"), "[[", 5)
# base$fold <- str_remove(sapply(str_split(base$model, "\\/"), "[[", 8), "\\.csv")
# base$estimation <- str_extract(base$model, "[0-9]+[a-z]+")

models <- c("0baseline", 
            "1lemma", 
            # "2lemma_pos",
            # "3lemma_bigram", 
            # "4lemma_pos_bigram", 
            # "5lemma_trigram", 
            "6lemma_pos_trigram", 
            # "88meta", 
            "7lemma_meta")

macro_f1_all <- lapply(models, function(x){
  
  tmp <- Fscores_boot(base[which(base$model == x), ], "party_id", "party_pred")
  tmp$f1_sims_conf$f1_macro <- tmp$f1_full_data$f1_macro
  tmp$f1_sims_conf$accuracy <- tmp$f1_full_data$accuracy
  tmp$f1_sims_conf$model <- x
  return(tmp$f1_sims_conf)
})

macro_f1 <- bind_rows(macro_f1_all) %>% 
  filter(model == "0baseline" | model == "1lemma" | model == "6lemma_pos_trigram" | model == "7lemma_meta") %>% 
  group_by(model) %>% mutate(vari = var(m)) %>% ungroup()


macro_f1$model <- factor(macro_f1$model, levels = c("0baseline", 
                                                    "1lemma", 
                                                    # "2lemma_pos",
                                                    # "3lemma_bigram", 
                                                    # "4lemma_pos_bigram", 
                                                    # "5lemma_trigram", 
                                                    "6lemma_pos_trigram", 
                                                    # "88meta", 
                                                    "7lemma_meta"))
                                                    # "8lemma_pos_meta", 
                                                    # "9lemma_pos_bigram_meta", 
                                                    # "99lemma_pos_trigram_meta"))

macro_f1$model <- factor(macro_f1$model, labels = c("Baseline", 
                                                    "Lemma (1)", 
                                                    # "L/PoS",
                                                    # "L/B", 
                                                    # "L/PoS/B",
                                                    # "L/T", 
                                                    "Lemma/PoS/Trigram (6)",
                                                    # "M", 
                                                    "Lemma/Meta (8)"))
                                                    # "L/PoS/M",
                                                    # "L/PoS/B/M",
                                                    # "L/PoS/T/M"))

macro_f1$labs <- droplevels(factor(macro_f1$labs, levels = c("SV", "A", "Sp", "KrF", "V", "H", "FrP")))


ggplot(macro_f1, aes(x = labs, y = m, color = labs)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) + 
  facet_wrap(~ model, ncol = 4) + 
  # geom_text(aes(label = sprintf("%.5f", vari), x = 2, y = .625, color = NULL)) +
  theme_bw() +
  scale_y_continuous(limits = c(.4, .65), breaks = seq(0, 1, 0.05), expand = c(0, 0)) +
  scale_color_manual(values = c("red", "red4", "forestgreen", "yellow", "green", "lightblue", "darkblue")) +
  theme_classic() +
  geom_hline(aes(yintercept = f1_macro), linetype = "dashed", size = 1) +
  guides(color = guide_legend(ncol = 7)) +
  labs(y = bquote(F[1]), x = NULL, color = NULL) +
  theme(panel.grid.major.y = element_line(linetype = "solid", color = "gray70"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom")
ggsave("./paper/figures/party_f1_boot.pdf", width = 10, height = 5)
#save(macro_f1, file = "../phd_thesis/trial_lecture/data/macro_f1.rda")
