rm(list = ls())

library(readr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggridges)
library(gridExtra)
library(stringr)

suppressWarnings(suppressMessages(meta <- read_csv("../../gitDebates/talk-of-norway/data/ton_updated.csv")))


prob_ridge_density <- function(estimation = c("gbm", "random_forest"), 
                               model = "0baseline", 
                               party = "FrP", 
                               sessions = NULL,
                               keyword = NULL){

  if(estimation == "gbm"){
    
    files <- list.files(paste0("./data/gbm_estimations/prob_grids/", model), full = TRUE, recursive = TRUE)
  
  } else if(estimation == "random_forest"){
    
    files <- list.files(paste0("./data/randomforest_estimations/prob_grids/", model), full = TRUE, recursive = TRUE)
    
  } else {
    stop("Estimation needs to be either 'gbm' or 'random_forest'")
  }
  
  if(is.null(sessions) == FALSE){
    files <- files[which(str_extract(files, "[0-9]{4}\\-[0-9]{4}") %in% sessions)]
  }
  
  
  raw <- lapply(files, function(x) suppressMessages(read_csv(x)))
  base <- bind_rows(raw)
  
  titles <- data.frame(party_id = unique(base$party_id))
  titles$title <- NA
  titles$title[which(titles$party_id == "A")] <- "Labor Party (A)"
  titles$title[which(titles$party_id == "H")] <- "Conservative Party (H)"
  titles$title[which(titles$party_id == "KrF")] <- "Christian People's Party (KrF)" 
  titles$title[which(titles$party_id == "SV")] <- "Socialist Left Party (SV)"
  titles$title[which(titles$party_id == "FrP")] <- "Progress Party (FrP)" 
  titles$title[which(titles$party_id == "Sp")] <- "Central Party (Sp)"
  titles$title[which(titles$party_id == "V")] <- "Liberal Party (V)"
  
  base_long <- melt(base, id.vars = c("id", "party_id")) %>% 
    arrange(id)
  
  party_sub <- base_long[which(base_long$party_id == party), ]
  
  if(is.null(keyword) == FALSE){
    keyword_ids <- meta$id[which(grepl(keyword, meta$keywords))]
    
    party_sub <- party_sub[which(party_sub$id %in% keyword_ids), ]
  }
  
  sorting <- party_sub %>% mutate(variable = as.character(variable)) %>% group_by(variable) %>% summarize(m = mean(value)) %>% arrange(m)
  
  party_sub$variable <- factor(party_sub$variable, levels = rev(sorting$variable))
  
  party_sub$color <- ifelse(party_sub$variable == party, "yes", "no")
  
  ggplot(party_sub, aes(x = value, color = color, fill = color)) +
    geom_density_ridges(aes(y = variable), alpha = .2, rel_min_height = 0.025) +
    scale_color_manual(values = c("gray50", "darkcyan")) +
    scale_fill_manual(values = c("gray50", "darkcyan")) +
    geom_segment(y = 1, yend = 1.95, x = sorting$m[7], xend = sorting$m[7], linetype = "dashed", size = .5) +
    geom_segment(y = 2, yend = 2.95, x = sorting$m[6], xend = sorting$m[6], linetype = "dashed", size = .5) +
    geom_segment(y = 3, yend = 3.95, x = sorting$m[5], xend = sorting$m[5], linetype = "dashed", size = .5) +
    geom_segment(y = 4, yend = 4.95, x = sorting$m[4], xend = sorting$m[4], linetype = "dashed", size = .5) +
    geom_segment(y = 5, yend = 5.95, x = sorting$m[3], xend = sorting$m[3], linetype = "dashed", size = .5) +
    geom_segment(y = 6, yend = 6.95, x = sorting$m[2], xend = sorting$m[2], linetype = "dashed", size = .5) +
    geom_segment(y = 7, yend = 7.95, x = sorting$m[1], xend = sorting$m[1], linetype = "dashed", size = .5) +
    scale_x_continuous(limits = c(0, mean(sorting$m[7]*2)), breaks = seq(0, mean(sorting$m[7]*2), .05)) +
    scale_y_discrete(expand = c(0, 0)) +
    theme_classic() +
    labs(x = NULL, y = NULL) +
    theme(legend.position = "none",
          panel.grid.major.y = element_line(color = "gray50", linetype = "dotted"),
          axis.line.y = element_blank())
  
}

# hm <- prob_ridge_density(estimation = "gbm", model = "7lemma_meta", party = "FrP", keyword = "[Ii]nnvand|[Aa]syl")
# save(hm, file = "../../Papers/phd_thesis/trial_lecture/data/frp_immig_ridgdens.rda")

s <- paste(seq(2005, 2008, 1), seq(2006, 2009), sep = "-")

prob_ridge_density(estimation = "gbm", model = "7lemma_meta", party = "FrP", keyword = "[Mm]iljøv|[Nn]atur")
ggsave("./paper/figures/partypos_ridgedensity/frp_envir.pdf", width = 9, height = 6)
system("convert -density 300 ./paper/figures/partypos_ridgedensity/frp_envir.pdf ./paper/figures/partypos_ridgedensity/frp_envir.pdf")

prob_ridge_density(estimation = "gbm", model = "7lemma_meta", party = "FrP", keyword = "[Bb]omp")
ggsave("./paper/figures/partypos_ridgedensity/frp_roadtoll.pdf", width = 9, height = 6)
system("convert -density 300 ./paper/figures/partypos_ridgedensity/frp_roadtoll.pdf ./paper/figures/partypos_ridgedensity/frp_roadtoll.pdf")

prob_ridge_density(estimation = "gbm", model = "7lemma_meta", party = "FrP", keyword = "[Aa]syl")
ggsave("./paper/figures/partypos_ridgedensity/frp_asylum.pdf", width = 9, height = 6)
system("convert -density 300 ./paper/figures/partypos_ridgedensity/frp_asylum.pdf ./paper/figures/partypos_ridgedensity/frp_asylum.pdf")


prob_ridge_density(estimation = "gbm", model = "0baseline", party = "A", sessions = s)
ggsave("./paper/figures/partypos_ridgedensity/0baseline.pdf", width = 9, height = 6)
system("convert -density 300 ./paper/figures/partypos_ridgedensity/0baseline.pdf ./paper/figures/partypos_ridgedensity/0baseline.pdf")

prob_ridge_density(estimation = "gbm", model = "1lemma", party = "A", sessions = s)
ggsave("./paper/figures/partypos_ridgedensity/1lemma.pdf", width = 9, height = 6)
system("convert -density 300 ./paper/figures/partypos_ridgedensity/1lemma.pdf ./paper/figures/partypos_ridgedensity/1lemma.pdf")

prob_ridge_density(estimation = "gbm", model = "6lemma_pos_trigram", party = "A", sessions = s)
ggsave("./paper/figures/partypos_ridgedensity/6lemma_pos_trigram.pdf", width = 9, height = 6)
system("convert -density 300 ./paper/figures/partypos_ridgedensity/6lemma_pos_trigram.pdf ./paper/figures/partypos_ridgedensity/6lemma_pos_trigram.pdf")

prob_ridge_density(estimation = "gbm", model = "7lemma_meta", party = "A", sessions = s)
ggsave("./paper/figures/partypos_ridgedensity/7lemma_meta.pdf", width = 9, height = 6)
system("convert -density 300 ./paper/figures/partypos_ridgedensity/7lemma_meta.pdf ./paper/figures/partypos_ridgedensity/7lemma_meta.pdf")

prob_ridge_density(estimation = "gbm", model = "7lemma_meta_party_role", party = "A", sessions = s)
ggsave("./paper/figures/partypos_ridgedensity/7lemma_meta_partyrole.pdf", width = 9, height = 6)
system("convert -density 300 ./paper/figures/partypos_ridgedensity/7lemma_meta_partyrole.pdf ./paper/figures/partypos_ridgedensity/7lemma_meta_partyrole.pdf")
