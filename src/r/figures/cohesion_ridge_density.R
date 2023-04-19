rm(list = ls())

library(readr)
library(dplyr)
library(tonR)
library(stringr)
library(ggplot2)
library(ggridges)
library(reshape2)

suppressWarnings(suppressMessages(meta <- read_csv("../../gitDebates/talk-of-norway/data/ton_updated.csv")))

mods <- c("0baseline", "1lemma", "2lemma_pos", "3lemma_bigram", "4lemma_pos_bigram",
          "5lemma_trigram", "6lemma_pos_trigram", "88meta", "7lemma_meta", "7lemma_meta_party_role", "8lemma_pos_meta",
          "9lemma_pos_bigram_meta", "99lemma_pos_trigram_meta")

title <- data.frame(mod = c("0baseline", "1lemma", "2lemma_pos", "3lemma_bigram", "4lemma_pos_bigram",
                            "5lemma_trigram", "6lemma_pos_trigram", "88meta", "7lemma_meta", "8lemma_pos_meta",
                            "9lemma_pos_bigram_meta", "99lemma_pos_trigram_meta", "7lemma_meta_party_role"),
                    title = c("Baseline", "Lemma", "Lemma/PoS",
                              "Lemma/Bigram", "Lemma/PoS/Bigram",
                              "Lemma/Trigram", "Lemma/PoS/Trigram",
                              "Meta", "Lemma/Meta", "Lemma/PoS/Meta",
                              "Lemma/PoS/Bigram/Meta",
                              "Lemma/PoS/Trigram/Meta",
                              "Lemma/Meta/PartyRole"),
                    stringsAsFactors = FALSE)

lapply(mods, function(x){
  files <- c(list.files(paste0("./data/gbm_estimations/prob_grids/", x), full = TRUE, recursive = TRUE))
  
  raw <- lapply(files, function(x) suppressMessages(read_csv(x)))
  
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
    summarize(m = mean(value))#,
  #m_party = unique(m_party))
  
  sorting <- test2 %>% group_by(party_id) %>% summarize(m_party = mean(m)) %>% arrange(m_party)
  sorting$color <- NA 
  sorting$color[which(sorting$party_id == "Sp")] <- "forestgreen"
  sorting$color[which(sorting$party_id == "KrF")] <- "yellow"
  sorting$color[which(sorting$party_id == "V")] <- "green"
  sorting$color[which(sorting$party_id == "H")] <- "lightblue"
  sorting$color[which(sorting$party_id == "A")] <- "red4"
  sorting$color[which(sorting$party_id == "FrP")] <- "darkblue"
  sorting$color[which(sorting$party_id == "SV")] <- "red"
  
  test2$party_id <- factor(test2$party_id, levels = rev(sorting$party_id))
  
  ggplot(test2, aes(x = m)) +
    geom_density_ridges(aes(y = party_id, color = party_id, fill = party_id), alpha = .5, rel_min_height = 0.025) +
    # facet_grid() +
    scale_color_manual(values = rev(sorting$color)) +
    scale_fill_manual(values = rev(sorting$color)) +
    geom_segment(y = 1, yend = 1.95, x = sorting$m_party[7], xend = sorting$m_party[7], linetype = "dashed", size = .5) +
    geom_segment(y = 2, yend = 2.95, x = sorting$m_party[6], xend = sorting$m_party[6], linetype = "dashed", size = .5) +
    geom_segment(y = 3, yend = 3.95, x = sorting$m_party[5], xend = sorting$m_party[5], linetype = "dashed", size = .5) +
    geom_segment(y = 4, yend = 4.95, x = sorting$m_party[4], xend = sorting$m_party[4], linetype = "dashed", size = .5) +
    geom_segment(y = 5, yend = 5.95, x = sorting$m_party[3], xend = sorting$m_party[3], linetype = "dashed", size = .5) +
    geom_segment(y = 6, yend = 6.95, x = sorting$m_party[2], xend = sorting$m_party[2], linetype = "dashed", size = .5) +
    geom_segment(y = 7, yend = 7.95, x = sorting$m_party[1], xend = sorting$m_party[1], linetype = "dashed", size = .5) +
    scale_x_continuous(limits = c(0, mean(sorting$m_party[7]*2)), breaks = seq(0, mean(sorting$m_party[7]*2), .05)) +
    scale_y_discrete(expand = c(0, 0)) +
    theme_classic() +
    labs(y = NULL, x = NULL) +
    #title = title$title[which(title$mod == x)]) +
    theme(legend.position = "none",
          panel.grid.major.y = element_line(color = "gray50", linetype = "dotted"),
          axis.line.y = element_blank())
  ggsave(paste0("./paper/figures/cohesion_ridgedensity/", x, ".pdf"), width = 7, height = 7)
  
})