set.seed(1)

library(tidyverse)
library(informationeffects)

compl_data <- readRDS("data/bes_preprocessed_scales.rds")

# create binary knowledge variables
qs <- c("0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1")
quantile(compl_data$know, probs = seq(0, 1, 0.1)) 
compl_data$know_binary <- ifelse(compl_data$know > 0, 1, 0) # use > 0, for above estimated mean

compl_data %>% 
  group_by(know_binary) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) # 49% informed

# create propensity score
compl_data$prop_score <- info_prop_scores("know_binary", 
                                          c("gender",
                                            "education",
                                            "income",
                                            "age"),
                                          compl_data)

compl_data %>% 
  ggplot() +
  aes(x = prop_score) +
  geom_histogram(binwidth=0.1, color = "black", fill = "salmon")

# check balance
bal_plots <- info_bal_plots(knowledge_var = "know_binary", 
                              covariates = c("gender",
                                             "education",
                                             "income",
                                             "age"),
                              prop_score ="prop_score", 
                              data = compl_data)

saveRDS(compl_data, "data/bes_preprocessed_scales_prop.rds")
