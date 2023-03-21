set.seed(1)

library(tidyverse)
library(informationeffects)
library(psych)
library(lavaan)
library(mirt)
library(gridExtra)

compl_data <- read_csv("data/bes_preprocessed.csv")

compl_data <- compl_data %>% 
  mutate(gender = factor(gender),
         age = factor(age,
                      levels = c("18-24","25-34","35-44","45-54","55-64","65-74","75-84","85plus")),
         ethnic_group = factor(ethnic_group),
         social_class = factor(social_class),
         religion = factor(religion),
         marital_status = factor(marital_status),
         education = factor(education,
                            levels = c("no_qual","below_gcse","gcse","a_level","undergrad","postgrad")),
         party_id = factor(party_id),
         eu_vote = factor(eu_vote))

know_scale <- info_scale(items = c("x01_1",
                                   "x01_2",
                                   "x01_3",
                                   "x01_4",
                                   "x01_5",
                                   "x01_6"),
                         data = compl_data,
                         binary_cutoff = 0.9)

# exploratory on 2 factors
fa_expl <- fa.poly(compl_data[,11:16], 2)
print(fa_expl$fa$loadings)

# confirmatory with one factor
mod_1f <- 'know =~ x01_1 + x01_2 + x01_3 + x01_4 + x01_5 + x01_6'
mod_1f.fit <- cfa(mod_1f, data=compl_data, ordered = TRUE)
summary(mod_1f.fit, standardized=TRUE)

# fit measures for model - it performs well
fitmeasures(mod_1f.fit)["rmsea"] # 0.0303529 (less than 0.05 or 0.01 correspond to good and very good fit)
fitmeasures(mod_1f.fit)["cfi"] # 0.9943167 (want > 0.95)
fitmeasures(mod_1f.fit)["tli"] # 0.9905279 (want > 0.95)
fitmeasures(mod_1f.fit)["agfi"] # 0.9892577 (the higher the better)

# see if we have to factor in guessing
mod_2pl <- mirt(data=compl_data[,11:16],
                model=1,
                itemtype = "2PL")

mod_3pl <- mirt(data=compl_data[,11:16],
                model=1,
                itemtype = "3PL")

# not significantly different so use 2pl model
anova(mod_2pl, mod_3pl)

# summary of model
summary(mod_2pl)
plot(mod_2pl, type="trace")
plot(mod_2pl, type="info")
coef(mod_2pl, IRTpars=T)

# model fit
itemfit(mod_2pl, empirical.plot = 1)
itemfit(mod_2pl, empirical.plot = 2)
itemfit(mod_2pl, empirical.plot = 3)
itemfit(mod_2pl, empirical.plot = 4)
itemfit(mod_2pl, empirical.plot = 5)
itemfit(mod_2pl, empirical.plot = 6)

# local independence, highest: 0.365
Q3resid <- data.frame(residuals(mod_2pl, type="Q3")) # max = 0.279

# each respondent's score
compl_data$know <- fscores(mod_2pl)[,1] # each person's expected score

# distribution
compl_data %>% 
  ggplot() +
  aes(x = know) +
  geom_histogram(bins = 40, color = "black", fill = "salmon")

# construct validity: coefficients
m <- lm(know ~
          gender +
          education +
          age +
          income,
        data = compl_data)
summary(m)

# construct validity: marginal means
marginal_means <- compl_data %>% 
  info_emmeans("know", c("gender","education","age","income"), .)
marginal_means

mm_gender <- data.frame(marginal_means[[1]]) %>% 
  ggplot() +
  aes(x = gender, y = emmean, color = "salmon") +
  geom_pointrange(aes(ymax=upper.CL, ymin=lower.CL)) +
  geom_line(group = 1) +
  geom_hline(yintercept=0, color = "grey") +
  theme(legend.position="none")
mm_gender

mm_education <- data.frame(marginal_means[[2]]) %>% 
  ggplot() +
  aes(x = education, y = emmean, color = "salmon") +
  geom_pointrange(aes(ymax=upper.CL, ymin=lower.CL)) +
  geom_line(group = 1) +
  geom_hline(yintercept=0, color = "grey") +
  theme(legend.position="none")
mm_education

mm_age <- data.frame(marginal_means[[3]]) %>% 
  ggplot() +
  aes(x = age, y = emmean, color = "salmon") +
  geom_pointrange(aes(ymax=upper.CL, ymin=lower.CL)) +
  geom_line(group = 1) +
  geom_hline(yintercept=0, color = "grey") +
  theme(legend.position="none")
mm_age

png(file="plots/marginal_means.png", width = 8, height = 11, units = 'in', res = 300)
grid.arrange(mm_gender, mm_education, mm_age)
dev.off()

saveRDS(compl_data, "data/bes_preprocessed_scales.rds")
