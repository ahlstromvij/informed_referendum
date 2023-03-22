set.seed(1)

library(tidyverse)
library(informationeffects)

compl_data <- readRDS("data/bes_preprocessed_scales.rds")

# create binary knowledge variables
qs <- c("0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1")
quantile(compl_data$know, probs = seq(0, 1, 0.1)) 
max_score <- quantile(compl_data$know, 1) 
compl_data$know_binary <- ifelse(compl_data$know == max_score, 1, 0) # use > 0, for above estimated mean

compl_data %>% 
  group_by(know_binary) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) # 26% informed

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

# look at extreme scores
mean(compl_data$prop_score)
iqr <- IQR(compl_data$prop_score)
third_quartile <- quantile(compl_data$prop_score, 0.75)
outlier_threshold <- third_quartile + 1.5 * iqr

table(compl_data$prop_score[compl_data$prop_score > outlier_threshold])
max_prop_score <- sort(compl_data$prop_score)[length(compl_data$prop_score)] 

# inspect the outlier
compl_data[which(compl_data$prop_score == max(compl_data$prop_score), arr.ind=TRUE),]
# a highly informed working class woman with no qualifications, no party affiliation, and who did not vote

# reduce the outlier to the second largest score
second_max_prop_score <- sort(compl_data$prop_score)[length(compl_data$prop_score)-1] 
compl_data <- compl_data %>% 
  mutate(prop_score = case_when(prop_score == max_prop_score ~ second_max_prop_score,
                                TRUE ~ prop_score))

# check distribution again
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
