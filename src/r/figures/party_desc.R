rm(list = ls())
suppressMessages(library(tonR))
suppressMessages(library(readr))
suppressMessages(library(ggplot2))
suppressMessages(library(reshape2))
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(library(pbmcapply))
suppressMessages(library(xtable))
suppressMessages(library(stargazer))

# Strip down theme
theme_set(theme_classic())

# Load all the models and bind them to the data
suppressWarnings(suppressMessages(meta <- read_csv("../../gitDebates/talk-of-norway/data/ton_updated.csv")))

folds <- list.files("./data/folds", recursive = TRUE, full.names = TRUE)

ids <- unique(as.character(unlist(lapply(folds, function(x) suppressMessages(read_csv(x))))))


ton <- meta %>% 
  filter(id %in% ids) 

ton$year <- as.numeric(format(as.Date(ton$date), "%Y"))

ton$party_id <- factor(ton$party_id, levels = c("SV", "A", "Sp", "KrF", "V", "H", "FrP"))
ton$session2 <- paste0(substr(ton$session, 3, 4), "/", substr(ton$session, 8, 9))


ton$session2 <- factor(ton$session2, levels = unique(ton$session2))

ton2 <- ton %>% group_by(session2, party_id) %>% 
  summarize(n = length(party_id))

ggplot(ton2, aes(x = session2, y = n, group = party_id)) +
  geom_point() +
  geom_line()+
  facet_wrap(~ party_id) +
  #scale_y_continuous(expand = c(.001,.001), limits = c(0, 12000)) +
  scale_fill_manual(values = c("red", "darkred", "darkgreen", "yellow3", "forestgreen", "blue", "darkblue")) +
  labs(y = "Frequency", x = NULL, fill = NULL) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90))
ggsave("./paper/figures/tenfold_nspeeches.pdf", width = 8, height = 3.5)


ton_party_session <- ton %>%
  group_by(party_id, session2) %>%
  summarize(n_party_speeches = length(party_id),
            n_party_seats = party_seats[which.max(party_seats)]) %>%
  group_by(session2) %>%
  mutate(n_session_speeches = sum(n_party_speeches),
         prop_party_speech = n_party_speeches / n_session_speeches,
         party_speech_per_seat = n_party_speeches / n_party_seats)

ggplot(ton_party_session, aes(x = session2, y = prop_party_speech, group = party_id)) +
  geom_point(stat = "identity") +
  geom_line()+
  facet_wrap(~ party_id) +
  # scale_y_continuous(expand = c(.001,.001), limits = c(0, .325)) +
  scale_fill_manual(values = c("red", "darkred", "darkgreen", "yellow3", "forestgreen", "blue", "darkblue")) +
  labs(y = "Proportion of speeches", x = NULL, fill = NULL) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90))
ggsave("./paper/figures/tenfold_propspeeches.pdf", width = 8, height = 3.5)

ggplot(ton_party_session, aes(x = session2, y = party_speech_per_seat, group = party_id)) +
  geom_point(stat = "identity") +
  geom_line()+
  facet_wrap(~ party_id) +
  #scale_y_continuous(expand = c(.001,.001), limits = c(0, 450)) +
  scale_fill_manual(values = c("red", "darkred", "darkgreen", "yellow3", "forestgreen", "blue", "darkblue")) +
  labs(y = "Speeches per seat", x = NULL, fill = NULL) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90))
ggsave("./paper/figures/tenfold_speechperseat.pdf", width = 8, height = 3.5)