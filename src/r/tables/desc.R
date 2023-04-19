rm(list = ls())
library(readr)
library(dplyr)
library(xtable)

suppressWarnings(suppressMessages(meta <- read_csv("../../gitDebates/talk-of-norway/data/ton_updated.csv")))

folds <- list.files("./data/folds", recursive = TRUE, full.names = TRUE)

ids <- unique(as.character(unlist(lapply(folds, function(x) suppressMessages(read_csv(x))))))


meta <- meta %>% 
  filter(id %in% ids) %>% 
  filter(is.na(rep_id) == FALSE) %>% 
  filter(is.na(party_id) == FALSE) %>% 
  mutate(nwords = stringi::stri_count_words(text)) %>% 
  filter(nwords > 50)



meta$rep_age <- round(as.numeric(meta$date - as.Date(meta$rep_birth, "%d.%m.%Y")) / 365.25, 2)

meta$rep_gender_num <- ifelse(meta$rep_gender == "mann", 0, 1)

meta$party_role_cab <- ifelse(meta$party_role == "Cabinet", 1, 0)
meta$party_role_opp <- ifelse(meta$party_role == "Opposition", 1, 0)
meta$party_role_sup <- ifelse(meta$party_role == "Support", 1, 0)

meta$language_num <- ifelse(meta$language == "nob", 0, 1)

meta$debate_type_int <- ifelse(meta$debate_type == "interpellasjon", 1, 0)
meta$debate_type_mun <- ifelse(meta$debate_type == "muntligsporretime", 1, 0)
meta$debate_type_ord <- ifelse(meta$debate_type == "ordinarsporretime", 1, 0)
meta$debate_type_sak <- ifelse(meta$debate_type == "saksreferat", 1, 0)

n_speeches_rep <- meta %>% group_by(rep_id, party_id, session) %>% summarize(speeches = length(rep_id))
n_speeches_session <- meta %>% group_by(session) %>% summarize(speeches = length(session))

# meta_vars <- c("rep_gender", "county", "debate_type", "case_type", "party_role", "language")


desc <- data.frame(min_stat = c(min(n_speeches_rep$speeches, na.rm = TRUE), 
                                #min(n_speeches_session$speeches, na.rm = TRUE), 
                                min(meta$nwords, na.rm = TRUE), 
                                min(meta$rep_age, na.rm = TRUE), 
                                min(meta$rep_gender_num, na.rm = TRUE), 
                                min(meta$party_role_cab, na.rm = TRUE),
                                min(meta$party_role_opp, na.rm = TRUE),
                                min(meta$party_role_sup, na.rm = TRUE),
                                min(meta$language_num, na.rm = TRUE),
                                min(meta$debate_type_int, na.rm = TRUE),
                                min(meta$debate_type_mun, na.rm = TRUE),
                                min(meta$debate_type_ord, na.rm = TRUE),
                                min(meta$debate_type_sak, na.rm = TRUE)),
                   mean_stat = c(mean(n_speeches_rep$speeches, na.rm = TRUE), 
                                 #mean(n_speeches_session$speeches, na.rm = TRUE), 
                                 mean(meta$nwords, na.rm = TRUE), 
                                 mean(meta$rep_age, na.rm = TRUE), 
                                 mean(meta$rep_gender_num, na.rm = TRUE), 
                                 mean(meta$party_role_cab, na.rm = TRUE),
                                 mean(meta$party_role_opp, na.rm = TRUE),
                                 mean(meta$party_role_sup, na.rm = TRUE),
                                 mean(meta$language_num, na.rm = TRUE),
                                 mean(meta$debate_type_int, na.rm = TRUE),
                                 mean(meta$debate_type_mun, na.rm = TRUE),
                                 mean(meta$debate_type_ord, na.rm = TRUE),
                                 mean(meta$debate_type_sak, na.rm = TRUE)),
                   median_stat = c(median(n_speeches_rep$speeches, na.rm = TRUE), 
                                   #median(n_speeches_session$speeches, na.rm = TRUE), 
                                   median(meta$nwords, na.rm = TRUE), 
                                   median(meta$rep_age, na.rm = TRUE), 
                                   median(meta$rep_gender_num, na.rm = TRUE), 
                                   median(meta$party_role_cab, na.rm = TRUE),
                                   median(meta$party_role_opp, na.rm = TRUE),
                                   median(meta$party_role_sup, na.rm = TRUE),
                                   median(meta$language_num, na.rm = TRUE),
                                   median(meta$debate_type_int, na.rm = TRUE),
                                   median(meta$debate_type_mun, na.rm = TRUE),
                                   median(meta$debate_type_ord, na.rm = TRUE),
                                   median(meta$debate_type_sak, na.rm = TRUE)),
                   max_stat = c(max(n_speeches_rep$speeches, na.rm = TRUE), 
                                #max(n_speeches_session$speeches, na.rm = TRUE), 
                                max(meta$nwords, na.rm = TRUE),
                                max(meta$rep_age, na.rm = TRUE), 
                                max(meta$rep_gender_num, na.rm = TRUE), 
                                max(meta$party_role_cab, na.rm = TRUE),
                                max(meta$party_role_opp, na.rm = TRUE),
                                max(meta$party_role_sup, na.rm = TRUE),
                                max(meta$language_num, na.rm = TRUE),
                                max(meta$debate_type_int, na.rm = TRUE),
                                max(meta$debate_type_mun, na.rm = TRUE),
                                max(meta$debate_type_ord, na.rm = TRUE),
                                max(meta$debate_type_sak, na.rm = TRUE)))

desc <- data.frame(desc)

rownames(desc) <- c("Speeches pr. MP", #"Speeches pr. session",
                    "Words pr. speech", "Age", "Gender (female)", 
                    "Cabinet party", "Opposition", "Support", 
                    "Nynorsk", 
                    "Interpellation", "Oral question", "Ordinary question", "Ordinary debate")
colnames(desc) <- c("Min", "Mean", "Median", "Max")
bold <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}

if(dir.exists("./paper") == TRUE){
  save_here <- "paper/tables/desc.tex"
} else {
  save_here <- "paper/desc.tex"
}

print(xtable(desc, caption = "Descriptive stats for selected variables",
             label = "tab:desc"),
      file = save_here,
      sanitize.rownames.function	= bold, 
      sanitize.colnames.function = bold)
