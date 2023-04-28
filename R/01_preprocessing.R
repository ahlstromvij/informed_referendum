set.seed(1)

library(tidyverse)
library(haven)
library(naniar)
library(Hmisc)

# data available at https://www.britishelectionstudy.com/data-object/2017-face-to-face/
sav_data = read_sav("data/bes_f2f_2017_v1.5.sav")

all_data <- sav_data %>% 
  dplyr::select(y09, # gender
         y10_banded, # age
         y11, # ethnicity
         w01, # social class
         w02, # social class squeeze
         y06, # religion
         y26, # marital status
         y01, # income
         edlevel, # education
         d01, # partisanship
         p01, # referendum vote
         x01_1, x01_2, x01_3, x01_4, x01_5, x01_6, # knowledge items
         wt_vote) # survey weight

all_data <- all_data %>% 
  dplyr::rename(gender = y09,
         age = y10_banded,
         ethnic_group = y11,
         social_class = w01,
         social_class_squeeze = w02,
         religion = y06,
         marital_status = y26,
         income = y01,
         education = edlevel,
         party_id = d01,
         eu_vote = p01)

summary(all_data)

all_data <- all_data %>% 
  mutate(gender = case_when(gender == 1 ~ "male",
                            gender == 2 ~ "female",
                            TRUE ~ NA_character_),
         age = case_when(age == 1 ~ "18-24",
                         age == 2 ~ "25-34",
                         age == 3 ~ "35-44",
                         age == 4 ~ "45-54",
                         age == 5 ~ "55-64",
                         age == 6 ~ "65-74",
                         age == 7 ~ "75-84",
                         age == 8 ~ "85plus",
                         TRUE ~ NA_character_),
         ethnic_group = case_when(ethnic_group == 1 ~ "british",
                                  ethnic_group > 1 ~ "other",
                                  TRUE ~ NA_character_),
         social_class = case_when(social_class == 1 ~ "middle_class",
                                  social_class == 2 ~ "working_class",
                                  social_class == 5 ~ "lower_class",
                                  social_class == 6 ~ "middle_class",
                                  TRUE ~ NA_character_),
         social_class = case_when(social_class_squeeze == 1 ~ "middle_class",
                                  social_class_squeeze == 2 ~ "working_class",
                                  TRUE ~ social_class),
         religion = case_when(religion == 0 ~ "no_religion",
                              religion > 0 & religion <= 11 ~ "christian",
                              religion >= 12 & religion <= 16 ~ "other_religion",
                              religion >= 17 & religion <= 19 ~ "christian",
                              TRUE ~ NA_character_),
         marital_status = case_when(marital_status == 1 ~ "married",
                                    marital_status == 2 ~ "married",
                                    marital_status == 3 ~ "single",
                                    marital_status == 4 ~ "widowed",
                                    marital_status == 5 ~ "separated",
                                    marital_status == 6 ~ "separated",
                                    TRUE ~ NA_character_),
         income = as.numeric(income),
         income = case_when(income == -1 ~ NA_real_,
                            income == -2 ~ NA_real_,
                            TRUE ~ income),
         education = case_when(education == 0 ~ "no_qual",
                               education == 1 ~ "below_gcse",
                               education == 2 ~ "gcse",
                               education == 3 ~ "a_level",
                               education == 4 ~ "undergrad",
                               education == 5 ~ "postgrad",
                               TRUE ~ NA_character_),
         party_id = case_when(party_id == 0 ~ "no_party",
                              party_id == 1 ~ "labour",
                              party_id == 2 ~ "cons",
                              party_id == 3 ~ "lib_dem",
                              party_id == 4 ~ "snp",
                              party_id == 5 ~ "plaid_cymru",
                              party_id == 6 ~ "green_party",
                              party_id == 7 ~ "ukip",
                              party_id == 9 ~ "other_party",
                              TRUE ~ NA_character_),
         eu_vote = case_when(eu_vote == 1 ~ "no_vote",
                             eu_vote == 2 ~ "leave",
                             eu_vote == 3 ~ "remain",
                             TRUE ~ NA_character_),
         x01_1 = case_when(x01_1 == 1 ~ 1,
                           x01_1 == 2 ~ 0,
                           x01_1 == -1 ~ 0,
                           TRUE ~ NA_real_),
         x01_2 = case_when(x01_2 == 1 ~ 1,
                           x01_2 == 2 ~ 0,
                           x01_2 == -1 ~ 0,
                           TRUE ~ NA_real_),
         x01_3 = case_when(x01_3 == 1 ~ 0,
                           x01_3 == 2 ~ 1,
                           x01_3 == -1 ~ 0,
                           TRUE ~ NA_real_),
         x01_4 = case_when(x01_4 == 1 ~ 1,
                           x01_4 == 2 ~ 0,
                           x01_4 == -1 ~ 0,
                           TRUE ~ NA_real_),
         x01_5 = case_when(x01_5 == 1 ~ 1,
                           x01_5 == 2 ~ 0,
                           x01_5 == -1 ~ 0,
                           TRUE ~ NA_real_),
         x01_6 = case_when(x01_6 == 1 ~ 0,
                           x01_6 == 2 ~ 1,
                           x01_6 == -1 ~ 0,
                           TRUE ~ NA_real_)) %>% 
  dplyr::select(-social_class_squeeze)

# check missing values
vis_miss(all_data[,1:16])

# remove rows with missing survey weights
all_data <- all_data[complete.cases(all_data[17]), ] # 127 observations missing weights

# impute missing values
all_data$x01_1 <- factor(all_data$x01_1)
all_data$x01_2 <- factor(all_data$x01_2)
all_data$x01_3 <- factor(all_data$x01_3)
all_data$x01_4 <- factor(all_data$x01_4)
all_data$x01_5 <- factor(all_data$x01_5)
all_data$x01_6 <- factor(all_data$x01_6)

impute_arg <- aregImpute(~ gender +
                           age +
                           ethnic_group +
                           social_class +
                           religion +
                           marital_status +
                           income +
                           education +
                           party_id +
                           eu_vote +
                           x01_1 +
                           x01_2 +
                           x01_3 +
                           x01_4 +
                           x01_5 +
                           x01_6,
                         data = all_data, n.impute = 5, tlinear = FALSE)
impute_arg
imp_data <- as.data.frame(impute.transcan(impute_arg, imputation=1, data=all_data, list.out=TRUE,pr=FALSE, check=FALSE)) 
head(imp_data)

compl_data <- cbind(imp_data, all_data$wt_vote)
colnames(compl_data)[17] <- "wt_vote"

write_csv(compl_data, "data/bes_preprocessed.csv")
