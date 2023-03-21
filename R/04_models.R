set.seed(1)

library(tidyverse)
library(nnet)
library(performance)
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

# Set reference level
compl_data$eu_vote <- relevel(compl_data$eu_vote, ref = "no_vote")

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
summary(m_partisan)
# moving from uninformed to informed is associated with a with an increase in the log odds of 
# voting leave in the amount of .45; and an increase in the log odds of voting remain in the amount
# of 0.86.

# relative risk
exp(coef(m_partisan))

# predicted probabilities
table(compl_data$party_id)
table(compl_data$age) # 55-64
table(compl_data$ethnic_group) # british
table(compl_data$social_class) # working_class
table(compl_data$religion) # no_religion
table(compl_data$marital_status) # married
median(compl_data$income) # 7
table(compl_data$education) # undergrad

pred_prob_partisan <- data.frame(party_id = rep(c("cons", "labour", "lib_dem", "no_party"),2), 
                                 know_binary = c(1,1,1,1,0,0,0,0),
                                 gender = rep("female",8),
                                 age = rep("55-64",8),
                                 ethnic_group = rep("british",8),
                                 social_class = rep("working_class",8),
                                 religion = rep("no_religion",8),
                                 marital_status = rep("married",8),
                                 income = rep(7,8),
                                 education = rep("undergrad",8))

pred_prob_partisan <- cbind(pred_prob_partisan[,1:2], 
                            predict(m_partisan, newdata = pred_prob_partisan, "probs")) %>% 
  pivot_longer(cols = c(no_vote, leave, remain),
               names_to = "vote",
               values_to = "probability") %>% 
  mutate(know_binary = factor(know_binary))

pred_prob_partisan %>% 
  ggplot() +
  aes(x = know_binary, y = probability, group = vote, color = vote) +
  geom_point() +
  geom_line() +
  facet_wrap(~party_id)

# p values
z_partisan <- summary(m_partisan)$coefficients/summary(m_partisan)$standard.errors
p_partisan <- data.frame((1 - pnorm(abs(z_partisan), 0, 1)) * 2)
p_partisan

# Diagnostics
r2_mcfadden(m_partisan) # 0.174

# Assess colinearity. The smallest possible value of VIF is one (absence of multicollinearity). 
# As a rule of thumb, a VIF value that exceeds 5 or 10 indicates a problematic amount of collinearity (James et al. 2014).
car::vif(m_partisan)

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
summary(m_demographic)
# moving from uninformed to informed is associated with a with an increase in the log odds of 
# voting leave in the amount of .48; and an increase in the log odds of voting remain in the amount
# of 0.90.

# relative risk
exp(coef(m_demographic))

pred_prob_demographic <- data.frame(party_id = rep(c("cons", "labour", "lib_dem", "no_party"),2), 
                                 know_binary = c(1,1,1,1,0,0,0,0),
                                 gender = rep("female",8),
                                 age = rep("55-64",8),
                                 ethnic_group = rep("british",8),
                                 social_class = rep("working_class",8),
                                 religion = rep("no_religion",8),
                                 marital_status = rep("married",8),
                                 income = rep(7,8),
                                 education = rep("undergrad",8))

pred_prob_demographic <- cbind(pred_prob_demographic[,1:2], 
                            predict(m_demographic, newdata = pred_prob_demographic, "probs")) %>% 
  pivot_longer(cols = c(no_vote, leave, remain),
               names_to = "vote",
               values_to = "probability") %>% 
  mutate(know_binary = factor(know_binary))

pred_prob_demographic %>% 
  ggplot() +
  aes(x = know_binary, y = probability, group = vote, color = vote) +
  geom_point() +
  geom_line() +
  facet_wrap(~party_id)

pred_prob_partisan$model <- "partisanship"
pred_prob_demographic$model <- "demographic"
pred_prob_combined <- rbind(pred_prob_partisan,
                            pred_prob_demographic)

png(file="plots/pred_probs_party.png", width = 8, height = 7, units = 'in', res = 300)
pred_prob_combined %>% 
  ggplot() +
  aes(x = know_binary, y = probability, group = interaction(vote, model), color = vote, linetype = model) +
  geom_point() +
  geom_line() +
  facet_wrap(~party_id)
dev.off

# p values
z_demographic <- summary(m_demographic)$coefficients/summary(m_demographic)$standard.errors
p_demographic <- data.frame((1 - pnorm(abs(z_demographic), 0, 1)) * 2)
p_demographic

# Diagnostics
r2_mcfadden(m_demographic) # 0.130

# Assess colinearity. The smallest possible value of VIF is one (absence of multicollinearity). 
# As a rule of thumb, a VIF value that exceeds 5 or 10 indicates a problematic amount of collinearity (James et al. 2014).
car::vif(m_demographic)

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
