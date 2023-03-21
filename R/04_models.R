set.seed(1)

library(tidyverse)
library(nnet)
library(RColorBrewer)

compl_data <- readRDS("data/bes_preprocessed_scales_prop.rds")

# Actual referendum outcome
remain_actual <- 0.481
leave_actual <- 0.519

# Actual proportion of Remain in sample
compl_data %>%
  filter(eu_vote != "no_vote") %>% 
  group_by(eu_vote) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n))

# Actual weighted proportion of Remain in data
sample_remain <- sum(compl_data$wt_vote[compl_data$eu_vote=="remain"])
sample_leave <- sum(compl_data$wt_vote[compl_data$eu_vote=="leave"])
sample_wt_remain <- sample_remain/(sample_remain + sample_leave)
sample_wt_remain

# PARTY ID MODEL
m_partisan <- multinom(eu_vote ~ 
                        know_binary +
                        party_id +
                        income +
                        gender +
                        religion +
                        age +
                        ethnic_group +
                        education + 
                        marital_status +
                        social_class,
                      data = compl_data, 
                      Hess = TRUE,
                      weights = prop_score)

# Simulation with fully informed participants
informed_df <- compl_data %>% 
  mutate(know_binary = 1)

sim_fullinf_partisan <- data.frame(predict(m_partisan, newdata = informed_df, "probs")) %>% 
  mutate(wt_vote = compl_data$wt_vote)

# Predicted (survey) weighted probability of Remain
pred_remain_partisan <- weighted.mean(sim_fullinf_partisan$remain, sim_fullinf_partisan$wt_vote)
pred_leave_partisan <- weighted.mean(sim_fullinf_partisan$leave, sim_fullinf_partisan$wt_vote)
pred_wt_remain_partisan <- pred_remain_partisan/(pred_remain_partisan + pred_leave_partisan)
pred_wt_remain_partisan
  
# Effect
pred_wt_remain_partisan - remain_actual

# PURELY DEMOGRAPHIC MODEL
m_demographic <- multinom(eu_vote ~ 
                            know_binary +
                            # party_id +
                            income +
                            gender +
                            religion +
                            age +
                            ethnic_group +
                            education + 
                            marital_status +
                            social_class,
                          data = compl_data, 
                          Hess = TRUE,
                          weights = prop_score)

# Simulation with fully informed participants
sim_fullinf_demographic <- data.frame(predict(m_demographic, newdata = informed_df, "probs")) %>% 
  mutate(wt_vote = compl_data$wt_vote)

# Predicted (survey) weighted probability of Remain
pred_remain_demographic <- weighted.mean(sim_fullinf_demographic$remain, sim_fullinf_demographic$wt_vote)
pred_leave_demographic <- weighted.mean(sim_fullinf_demographic$leave, sim_fullinf_demographic$wt_vote)
pred_wt_remain_demographic <- pred_remain_demographic/(pred_remain_demographic + pred_leave_demographic)
pred_wt_remain_demographic

# Effect
pred_wt_remain_demographic - remain_actual

# Graph
df_graph <- data.frame("Scenario" = c("Actual", "Actual",
                                      "Informed (Partisanship)", "Informed (Partisanship)",
                                      "Informed (Demographic)","Informed (Demographic)"),
                       "Vote" = c("Leave","Remain","Leave","Remain","Leave","Remain"),
                       "Support" = c(leave_actual,
                                     remain_actual,
                                     1 - pred_wt_remain_partisan,
                                     pred_wt_remain_partisan,
                                     1 - pred_wt_remain_demographic,
                                     pred_wt_remain_demographic))

df_graph <- df_graph %>% 
  mutate(Support = round(Support * 100,1))

df_graph <- within(df_graph, Scenario <- factor(Scenario,
                                                levels=c("Actual", "Informed (Partisanship)", "Informed (Demographic)")))

png(file="plots/informed_referendum.png", width = 8, height = 6, units = 'in', res = 300)
ggplot(data=df_graph, aes(x=Scenario, y=Support, fill=Vote)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=50, linetype="dotted", color="black") +
  scale_fill_brewer(palette="Blues") +
  annotate("text", x = 1, y = 75, label = paste(df_graph$Support[1],"%", sep="")) +
  annotate("text", x = 1, y = 25, label = paste(df_graph$Support[2],"%", sep="")) +
  annotate("text", x = 2, y = 75, label = paste(df_graph$Support[3],"%", sep="")) +
  annotate("text", x = 2, y = 25, label = paste(df_graph$Support[4],"%", sep="")) +
  annotate("text", x = 3, y = 75, label = paste(df_graph$Support[5],"%", sep="")) +
  annotate("text", x = 3, y = 25, label = paste(df_graph$Support[6],"%", sep="")) +
  ylab("Support (%)") +
  xlab("") +
  theme_minimal()
dev.off()
