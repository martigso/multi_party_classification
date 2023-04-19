rm(list = ls())

library(readr)
library(dplyr)
library(tonR)
library(stringr)
library(ggplot2)
library(reshape2)
files <- c(list.files("./data/gbm_estimations/preds", full = TRUE, recursive = TRUE))

raw <- lapply(files, function(x) suppressMessages(read_csv(x)))


names(raw) <- files

base <- bind_rows(raw, .id = "model")

base$session <- str_extract(base$model, "[0-9]{4}\\-[0-9]{4}")
base$model <- sapply(str_split(base$model, "\\/"), "[[", 5)

base <- base %>% 
  filter(model == "0baseline" | model == "1lemma" | model == "6lemma_pos_trigram" | model == "7lemma_meta" | model == "7lemma_meta_partyrole") 

baseline <- prop.table(table(base$party_id[which(base$model == "0baseline")], 
                             base$party_pred[which(base$model == "0baseline")]),margin = 1)
lemma <- prop.table(table(base$party_id[which(base$model == "1lemma")], 
                          base$party_pred[which(base$model == "1lemma")]), margin = 1)
lemma_pos_trigram <- prop.table(table(base$party_id[which(base$model == "6lemma_pos_trigram")], 
                                      base$party_pred[which(base$model == "6lemma_pos_trigram")]), margin = 1)
lemma_meta <- prop.table(table(base$party_id[which(base$model == "7lemma_meta")], 
                               base$party_pred[which(base$model == "7lemma_meta")]), margin = 1)

lemma_meta_partyrole <- prop.table(table(base$party_id[which(base$model == "7lemma_meta_partyrole")], 
                               base$party_pred[which(base$model == "7lemma_meta_partyrole")]), margin = 1)


baseline <- melt(baseline) %>% 
  mutate(model = "Baseline")

lemma <- melt(lemma) %>% 
  mutate(model = "Lemma")

lemma_pos_trigram <- melt(lemma_pos_trigram) %>% 
  mutate(model = "Lemma/PoS/Trigram")

lemma_meta <- melt(lemma_meta) %>% 
  mutate(model = "Lemma/Meta")

lemma_meta_partyrole <- melt(lemma_meta_partyrole) %>% 
  mutate(model = "Lemma/Meta/Party role")


all <- bind_rows(baseline, lemma, lemma_pos_trigram, lemma_meta, lemma_meta_partyrole)
all$value <- all$value * 100
all$Var1 <- droplevels(factor(all$Var1, levels = c("SV", "A", "Sp", "KrF", "V", "H", "FrP")))
all$Var2 <- droplevels(factor(all$Var2, levels = rev(c("SV", "A", "Sp", "KrF", "V", "H", "FrP"))))

ggplot(all[which(all$model == "Baseline"),], aes(x = Var1, y = Var2, fill = value))+
  geom_tile() +
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))+
  scale_fill_gradient(limits = c(0, 100), low = "white", high = "grey20",
                      guide = guide_colorbar(title = NULL))+
  geom_text(aes(label = sprintf("%.1f", round(value, digits = 1))))+
  # facet_wrap(~model)+
  labs(x = NULL, y = NULL)+
  theme_classic() +
  theme(axis.line = element_blank(),
        legend.position = "none",
        legend.direction = "horizontal",
        strip.text = element_text(size = 12))
ggsave("./paper/figures/partypos_accuracy/0baseline.pdf", width = 7, height = 5)


ggplot(all[which(all$model == "Lemma"),], aes(x = Var1, y = Var2, fill = value))+
  geom_tile() +
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))+
  scale_fill_gradient(limits = c(0, 100), low = "white", high = "grey20",
                      guide = guide_colorbar(title = NULL))+
  geom_text(aes(label = sprintf("%.1f", round(value, digits = 1))))+
  # facet_wrap(~model)+
  labs(x = NULL, y = NULL)+
  theme_classic() +
  theme(axis.line = element_blank(),
        legend.position = "none",
        legend.direction = "horizontal",
        strip.text = element_text(size = 12))
ggsave("./paper/figures/partypos_accuracy/1lemma.pdf", width = 7, height = 5)

ggplot(all[which(all$model == "Lemma/PoS/Trigram"),], aes(x = Var1, y = Var2, fill = value))+
  geom_tile() +
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))+
  scale_fill_gradient(limits = c(0, 100), low = "white", high = "grey20",
                      guide = guide_colorbar(title = NULL))+
  geom_text(aes(label = sprintf("%.1f", round(value, digits = 1))))+
  # facet_wrap(~model)+
  labs(x = NULL, y = NULL)+
  theme_classic() +
  theme(axis.line = element_blank(),
        legend.position = "none",
        legend.direction = "horizontal",
        strip.text = element_text(size = 12))
ggsave("./paper/figures/partypos_accuracy/6lemma_pos_trigram.pdf", width = 7, height = 5)



ggplot(all[which(all$model == "Lemma/Meta"),], aes(x = Var1, y = Var2, fill = value))+
  geom_tile() +
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))+
  scale_fill_gradient(limits = c(0, 100), low = "white", high = "grey20",
                      guide = guide_colorbar(title = NULL))+
  geom_text(aes(label = sprintf("%.1f", round(value, digits = 1))))+
  # facet_wrap(~model)+
  labs(x = NULL, y = NULL)+
  theme_classic() +
  theme(axis.line = element_blank(),
        legend.position = "none",
        legend.direction = "horizontal",
        strip.text = element_text(size = 12))
ggsave("./paper/figures/partypos_accuracy/7lemma_meta.pdf", width = 7, height = 5)

ggplot(all[which(all$model == "Lemma/Meta/Party role"),], aes(x = Var1, y = Var2, fill = value))+
  geom_tile() +
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))+
  scale_fill_gradient(limits = c(0, 100), low = "white", high = "grey20",
                      guide = guide_colorbar(title = NULL))+
  geom_text(aes(label = sprintf("%.1f", round(value, digits = 1))))+
  # facet_wrap(~model)+
  labs(x = NULL, y = NULL)+
  theme_classic() +
  theme(axis.line = element_blank(),
        legend.position = "none",
        legend.direction = "horizontal",
        strip.text = element_text(size = 12))
ggsave("./paper/figures/partypos_accuracy/7lemma_meta_partyrole.pdf", width = 7, height = 5)