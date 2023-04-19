# Building function for plotting Alluvial diagrams based on SVM
class_alluvium <- function(data, model, parl_period = NULL, parties = NULL, keyword = NULL, type, title = NULL, party_order = NULL){
  
  # Converting to data frame (if it is tibble etc)
  data <- as.data.frame(data)
  
  # Extracting model
  model_lab <- sapply(strsplit(model, "_"), "[[", 1)
  
  # Uppercasing first letter, for visual purpose
  model_lab <- paste0(toupper(substring(model_lab, 1, 1)), substring(model_lab, 2, nchar(model_lab)))
  
  # Extracting the model variable to generic new variable
  data$model <- as.character(data[, model])
  
  # Subsetting on period, if set
  if(is.null(parl_period) == FALSE){
    data <- data[which(data$parl_period == parl_period), ]
  }
  
  # Subsetting parties, if set
  if(is.null(parties) == FALSE){
    data <- data[which(data$party_id %in% parties & data$model %in% parties), ]
  } else {
    parties <- unique(data$party_id)
  }
  
  # Subsetting on keywords, if set
  if(is.null(keyword) == FALSE){
    data <- data[which(grepl(keyword, data$keywords)), ]
  }
  
  # Counting (mis)classifications
  if(type == "count"){
    data <- data %>%
      filter(party_id != model) %>%          # Exclude true positives
      group_by(party_id, model) %>%          # Grouping on actual and classification
      count() %>%                            # Counting within groups
      ungroup() %>%                          # Ungrouping
      as.data.frame()                        # Converting from tibble to data.frame
  } else if(type == "percent"){
    data <- data %>% 
      filter(party_id != model) %>%          # Exclude true positives
      mutate(total = length(party_id)) %>%   # Counting total misclassifications in sample
      group_by(party_id, model) %>%          # Grouping on actual and classification
      summarize(n = length(party_id),        
                total = unique(total)) %>%   # Counting within groups
      mutate(n = n / total * 100) %>%        # Converting to percent
      as.data.frame()                        # Converting from tibble to data.frame
  } else{
    stop("Argument 'type' must be one of 'count' or 'percent'")
  }
  
  # Setting party order
  if(is.null(party_order) == TRUE){
    
    col_order <- as.character(sort(droplevels(factor(parties, levels = c("SV", "A", "Sp", "KrF", "V", "H", "FrP")))))
  
  } else {
    
    col_order <- as.character(sort(droplevels(factor(parties, levels = party_order))))
    
  }
  
  
  
  # Setting up correct colors for parties
  col_order[which(col_order == "SV")] <- "red"
  col_order[which(col_order == "A")] <- "red4"
  col_order[which(col_order == "Sp")] <- "forestgreen"
  col_order[which(col_order == "KrF")] <- "yellow"
  col_order[which(col_order == "V")] <- "green"
  col_order[which(col_order == "H")] <- "lightblue"
  col_order[which(col_order == "FrP")] <- "darkblue"
  
  # Getting order according to left-right
  if(is.null(party_order) == TRUE){
    
    data$strata1 <- droplevels(factor(data$party_id, levels = c("SV", "A", "Sp", "KrF", "V", "H", "FrP")))
    data$strata2 <- droplevels(factor(data$model, levels = c("SV", "A", "Sp", "KrF", "V", "H", "FrP")))
  
  } else {
    
    data$strata1 <- droplevels(factor(data$party_id, levels = party_order))
    data$strata2 <- droplevels(factor(data$model, levels = party_order))
    
  }
  
  
  y1_ticks <- data %>% group_by(strata1) %>% summarize(n = sum(n)) %>% arrange(desc(strata1))
  y1_ticks <- round(c(0, cumsum(y1_ticks$n)), digits = 0)
  
  y2_ticks <- data %>% group_by(strata2) %>% summarize(n = sum(n)) %>% arrange(desc(strata2))
  y2_ticks <- round(c(0, cumsum(y2_ticks$n)), digits = 0)
  
  ggplot(data, aes(y = n, axis1 = strata1, axis2 = strata2)) +
    geom_alluvium(aes(fill = strata1), width = 1/20, alpha = .7) +
    geom_stratum(width = 1/10, fill = "white", color = "gray60") +
    geom_text(stat = "stratum", label.strata = TRUE, size = 10) +
    scale_fill_manual(values = col_order) +
    scale_x_continuous(breaks = 1:2, labels = c("True class", model_lab), expand = c(0, 0)) +
    scale_y_continuous(breaks = y1_ticks, sec.axis = sec_axis(~ ., breaks = y2_ticks), expand = c(0, 0)) +
    labs(title = title) +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.line.x = element_blank(),
          axis.text.y = element_text(size = 26, vjust = -.05),
          axis.text.x = element_text(face = "bold", size = 26),
          axis.ticks.x = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1), units = "cm"))
  
}
