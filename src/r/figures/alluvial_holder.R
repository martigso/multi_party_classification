rm(list = ls())

suppressMessages(library(tonR))
suppressMessages(library(readr))
suppressMessages(library(ggplot2))
suppressMessages(library(reshape2))
suppressMessages(library(dplyr))
suppressMessages(library(ggalluvial))
suppressMessages(library(gridExtra))

# Strip down theme
theme_set(theme_classic())

source("./src/r/figures/alluvial_function.R")

# Load all the models and bind them to the data
ton <- suppressMessages(suppressWarnings(read_csv("../../gitDebates/talk-of-norway/data/ton_updated.csv")))

files <- list.files("data/gbm_estimations/preds/9full", include.dirs = FALSE, recursive = TRUE, full.names = TRUE)

lemma <- lapply(files, function(x){
  suppressMessages(read_csv(x))
})

lemma <- bind_rows(lemma) %>% 
  left_join(ton, by = c("id", "party_id"))


##############################################
### Road toll debates, with SV, V, and FrP ###
##############################################

class_alluvium(data = lemma,
               parties = c("SV", "H", "FrP"), 
               model = "party_pred",
               keyword = "[Bb]ompeng",
               type = "count")
# ggsave("./Figures/alluvial_sv_v_frp_roadtoll_baseline.pdf")


##############################################
## Example all periods, with SV, H, and FrP ##
##############################################

class_alluvium(data = lemma, 
               parties = c("SV", "H", "FrP"), 
               model = "party_pred",
               type = "count")
# ggsave("./Figures/alluvial_sv_h_frp_allperiods_baseline.pdf")

#############################################
######### All periods, all parties ##########
#############################################
class_alluvium(data = lemma, 
               model = "party_pred",
               type = "count")
# ggsave("./Figures/alluvial_allparties_allperiods_baseline.pdf")


#############################################
####### All parties and periods #############
#############################################

class_alluvium(data = lemma, 
               model = "party_pred",
               parl_period = "1997-2001",
               type = "count") +
  ggtitle(label = "1997-2001") +
  theme(plot.title = element_text(hjust = 0.5, size = 26))
# ggsave("./Figures/alluvial_allparties_19972001_meta.pdf")

class_alluvium(data = lemma, 
               model = "party_pred",
               parl_period = "2001-2005",
               type = "count")+
  ggtitle(label = "2001-2005") +
  theme(plot.title = element_text(hjust = 0.5, size = 26))
# ggsave("./Figures/alluvial_allparties_20012005_meta.pdf")


class_alluvium(data = lemma, 
               model = "party_pred",
               parl_period = "2005-2009",
               type = "count") +
  ggtitle(label = "2005-2009") +
  theme(plot.title = element_text(hjust = 0.5, size = 26))
# ggsave("./Figures/alluvial_allparties_20052009_meta.pdf")


class_alluvium(data = lemma, 
               model = "party_pred",
               parl_period = "2009-2013",
               type = "count") +
  ggtitle(label = "2009-2013") +
  theme(plot.title = element_text(hjust = 0.5, size = 26))
# ggsave("./Figures/alluvial_allparties_20092013_meta.pdf")


class_alluvium(data = lemma, 
               model = "party_pred",
               parl_period = "2013-2017",
               type = "count") +
  ggtitle(label = "2013-2017") +
  theme(plot.title = element_text(hjust = 0.5, size = 26))
# ggsave("./Figures/alluvial_allparties_20132017_meta.pdf")

#############################################
############### Immigration #################
#############################################

class_alluvium(data = lemma, 
               model = "party_pred",
               parties = c("SV", "V", "A", "FrP"), 
               party_order = c("SV", "V", "A", "FrP"),
               keyword = "[Ii]nnvandr",
               type = "count")

#############################################
############### EU/EFTA #################
#############################################

class_alluvium(data = lemma, 
               model = "party_pred",
               parties = c("FrP", "A", "H"), 
               party_order = c("FrP", "A", "H"), 
               keyword = "EØS-avtalen",
               type = "count")
# ggsave("./Figures/alluvial_frp_a_h_eu_baseline.pdf")


#############################################
############### Sp switch ###################
#############################################

# Baseline
class_alluvium(data = lemma, 
               model = "party_pred",
               parties = c("A", "Sp", "H"),
               parl_period = "1997-2001",
               type = "count")
# ggsave("./Figures/alluvial_spswitch19972001_baseline.pdf")

class_alluvium(data = lemma, 
               model = "party_pred",
               parties = c("A", "Sp", "H"),
               parl_period = "2001-2005",
               type = "count")
# ggsave("./Figures/alluvial_spswitch20012005_baseline.pdf")

class_alluvium(data = lemma, 
               model = "party_pred",
               parties = c("A", "Sp", "H"),
               parl_period = "2005-2009",
               type = "count")
# ggsave("./Figures/alluvial_spswitch20052009_baseline.pdf")


class_alluvium(data = lemma, 
               model = "party_pred",
               parties = c("SV", "A", "FrP"), 
               keyword = "Frivillige organisasjoner",
               type = "count")


yo <- head(sort(table(unlist(strsplit(lemma$keywords, " ; "))), decreasing = TRUE), n = 100)
yo
