library(haven)
library(dplyr)
library(tidyr)
mpps_2009 <- read_dta("G:/My Drive/0. study abroad/academic/11. 2024 Winter/3. SurvMeth 721 Total Survey Error and Quality II/2. Assignments/MPPS Data/mpps_data/2009spring_data_final_20130128.dta")
str(mpps_2009)
mpps_2017 <- read_dta("G:/My Drive/0. study abroad/academic/11. 2024 Winter/3. SurvMeth 721 Total Survey Error and Quality II/2. Assignments/MPPS Data/mpps_data/2017spring_data_final_respondents_20171023.dta")
str(mpps_2017)
mpps_2018 <- read_dta("G:/My Drive/0. study abroad/academic/11. 2024 Winter/3. SurvMeth 721 Total Survey Error and Quality II/2. Assignments/MPPS Data/mpps_data/2018spring_data_final_respondents_20181011.dta")
str(mpps_2018)

mpps_2009_res <- mpps_2009 %>% 
  filter(idcompleted != 0) %>% 
  select(1:8, 60:69, 97:130, 139:149, 165:170)
mpps_2009_res_wide <- mpps_2009_res %>% 
  select(1, 3, 9:69) %>% 
  gather(key = "question", value = response, -respondent_id, -source)

mpps_2017_res <- mpps_2017 %>% 
  filter(idcompleted != 0) %>% 
  select(1:10, 98:103, 118:124, 127:134)
mpps_2017_res_wide <- mpps_2017_res %>% 
  select(1, 5, 11:31) %>% 
  gather(key = "question", value = response, -respondent_id, -source)

mpps_2018_res <- mpps_2018 %>% 
  filter(idcompleted != 0) %>% 
  select(1:8, 94:101, 110:118, 120:124)
mpps_2018_res_wide <- mpps_2018_res %>% 
  select(1, 3, 9:30) %>% 
  gather(key = "question", value = response, -respondent_id, -source)

## 2009
### Q7
## needs revise
mpps_2009_res_7 <- mpps_2009es %>% 
  select(1:8, 17:25) %>% 
  mutate(nu_character_prompt_29a = 41) %>% 
  mutate(nu_character_prompt_29b = 60) %>% 
  mutate(nu_character_prompt_29c = 48) %>%
  mutate(nu_character_prompt_29d = 31) %>%
  mutate(nu_character_prompt_29e = 60) %>%
  mutate(nu_character_prompt_29f = 68) %>%
  mutate(nu_character_prompt_29g = 24) %>% 
  mutate(nu_character_prompt_29h = 37) %>% 
  mutate(nu_character_prompt_29i = 10) %>% 
  mutate(professional_terms_29a = 0) %>%
  mutate(professional_terms_29b = 0) %>%
  mutate(professional_terms_29c = 0) %>%
  mutate(professional_terms_29d = 0) %>%
  mutate(professional_terms_29e = 0) %>%
  mutate(professional_terms_29f = 1) %>%
  mutate(professional_terms_29g = 0) %>%
  mutate(professional_terms_29h = 0) %>%
  mutate(professional_terms_29i = 0) %>%
  mutate(nu_option_29 = 9) %>%
  mutate(order_prompt_29a = 1) %>%
  mutate(order_prompt_29b = 2) %>%
  mutate(order_prompt_29c = 3) %>%
  mutate(order_prompt_29d = 4) %>%
  mutate(order_prompt_29e = 5) %>%
  mutate(order_prompt_29f = 6) %>%
  mutate(order_prompt_29g = 7) %>%
  mutate(order_prompt_29h = 8) %>%
  mutate(order_prompt_29i = 9) %>%
  mutate(order_survey_29 = 29) %>%
  mutate(other_29a = 0) %>%
  mutate(other_29b = 0) %>%
  mutate(other_29c = 0) %>%
  mutate(other_29d = 0) %>%
  mutate(other_29e = 0) %>%
  mutate(other_29f = 0) %>%
  mutate(other_29g = 0) %>%
  mutate(other_29h = 0) %>%
  mutate(other_29i = 0) %>%
  mutate(none_29a = 0) %>%
  mutate(none_29b = 0) %>%
  mutate(none_29c = 0) %>%
  mutate(none_29d = 0) %>%
  mutate(none_29e = 0) %>%
  mutate(none_29f = 0) %>%
  mutate(none_29g = 0) %>%
  mutate(none_29h = 0) %>%
  mutate(none_29i = 0) %>%
  mutate(dk_29a = 0) %>%
  mutate(dk_29b = 0) %>%
  mutate(dk_29c = 0) %>%
  mutate(dk_29d = 0) %>%
  mutate(dk_29e = 0) %>%
  mutate(dk_29f = 0) %>% 
  mutate(dk_29g = 0) %>% 
  mutate(dk_29h = 0) %>% 
  mutate(dk_29i = 1)

### Q10

### Q11

### Q15

### Q21

## 2017
### Q18
mpps_2017_res_wide_1 <- mpps_2017_res_wide %>% 
  mutate(nu_character_prompt = 
           ifelse(question == "q18a", 49,
                  ifelse(question == "q18b", 9,
                         ifelse(question == "q18c", 9,
                                ifelse(question == "q18d", 8,
                                       ifelse(question == "q18e", 19,
                                              ifelse(question == "q18f", 10, 
                                                     ifelse(question == "q23a", 29,
                                                            ifelse(question == "q23b", 58,
                                                                   ifelse(question == "q23c", 70,
                                                                          ifelse(question == "q23d", 14,
                                                                                 ifelse(question == "q23e", 5, 
                                                                                        ifelse(question == "q23f", 10,
                                                                                               ifelse(question == "q26a", 32,
                                                                                                      ifelse(question == "q26b", 46,
                                                                                                             ifelse(question == "q26c", 39,
                                                                                                                    ifelse(question == "q26d", 33,
                                                                                                                           ifelse(question == "q26e", 36,
                                                                                                                                  ifelse(question == "q26f", 5, 10)))))))))))))))))))
                                                                           
  mutate(professional_terms_18a = 0) %>%
  mutate(professional_terms_18b = 0) %>%
  mutate(professional_terms_18c = 0) %>%
  mutate(professional_terms_18d = 0) %>%
  mutate(professional_terms_18e = 0) %>%
  mutate(professional_terms_18f = 0) %>%
  mutate(nu_option_18 = 6) %>%
  mutate(order_prompt_18a = 1) %>%
  mutate(order_prompt_18b = 2) %>%
  mutate(order_prompt_18c = 3) %>%
  mutate(order_prompt_18d = 4) %>%
  mutate(order_prompt_18e = 5) %>%
  mutate(order_prompt_18f = 6) %>%
  mutate(order_survey_18 = 18) %>%
  mutate(other_18a = 0) %>%
  mutate(other_18b = 0) %>%
  mutate(other_18c = 0) %>%
  mutate(other_18d = 0) %>%
  mutate(other_18e = 0) %>%
  mutate(other_18f = 0) %>%
  mutate(none_18a = 1) %>%
  mutate(none_18b = 0) %>%
  mutate(none_18c = 0) %>%
  mutate(none_18d = 0) %>%
  mutate(none_18e = 0) %>%
  mutate(none_18f = 0) %>%
  mutate(dk_18a = 0) %>%
  mutate(dk_18b = 0) %>%
  mutate(dk_18c = 0) %>%
  mutate(dk_18d = 0) %>%
  mutate(dk_18e = 0) %>%
  mutate(dk_18f = 1)
### Q23
mpps_2017_res_23 <- mpps_2017_res %>% 
  select(1:10, 17:23) %>% 
  mutate(nu_character_prompt_23a = 29) %>% 
  mutate(nu_character_prompt_23b = 58) %>% 
  mutate(nu_character_prompt_23c = 70) %>%
  mutate(nu_character_prompt_23d = 14) %>%
  mutate(nu_character_prompt_23e = 5) %>%
  mutate(nu_character_prompt_23f = 10) %>% 
  mutate(professional_terms_23a = 0) %>%
  mutate(professional_terms_23b = 1) %>%
  mutate(professional_terms_23c = 1) %>%
  mutate(professional_terms_23d = 0) %>%
  mutate(professional_terms_23e = 0) %>%
  mutate(professional_terms_23f = 0) %>%
  mutate(nu_option_23 = 6) %>%
  mutate(order_prompt_23a = 1) %>%
  mutate(order_prompt_23b = 2) %>%
  mutate(order_prompt_23c = 3) %>%
  mutate(order_prompt_23d = 4) %>%
  mutate(order_prompt_23e = 5) %>%
  mutate(order_prompt_23f = 6) %>%
  mutate(order_survey_23 = 23) %>%
  mutate(other_23a = 0) %>%
  mutate(other_23b = 0) %>%
  mutate(other_23c = 0) %>%
  mutate(other_23d = 0) %>%
  mutate(other_23e = 1) %>%
  mutate(other_23f = 0) %>%
  mutate(none_23a = 0) %>%
  mutate(none_23b = 0) %>%
  mutate(none_23c = 0) %>%
  mutate(none_23d = 0) %>%
  mutate(none_23e = 0) %>%
  mutate(none_23f = 0) %>%
  mutate(dk_23a = 0) %>%
  mutate(dk_23b = 0) %>%
  mutate(dk_23c = 0) %>%
  mutate(dk_23d = 0) %>%
  mutate(dk_23e = 0) %>%
  mutate(dk_23f = 1)
### Q26
mpps_2017_res_26 <- mpps_2017_res %>% 
  select(1:10, 24:31) %>% 
  mutate(nu_character_prompt_26a = 32) %>% 
  mutate(nu_character_prompt_26b = 46) %>% 
  mutate(nu_character_prompt_26c = 39) %>%
  mutate(nu_character_prompt_26d = 33) %>%
  mutate(nu_character_prompt_26e = 36) %>%
  mutate(nu_character_prompt_26f = 5) %>%
  mutate(nu_character_prompt_26g = 10) %>% 
  mutate(professional_terms_26a = 0) %>%
  mutate(professional_terms_26b = 0) %>%
  mutate(professional_terms_26c = 0) %>%
  mutate(professional_terms_26d = 0) %>%
  mutate(professional_terms_26e = 0) %>%
  mutate(professional_terms_26f = 0) %>%
  mutate(professional_terms_26g = 0) %>%
  mutate(nu_option_26 = 7) %>%
  mutate(order_prompt_26a = 1) %>%
  mutate(order_prompt_26b = 2) %>%
  mutate(order_prompt_26c = 3) %>%
  mutate(order_prompt_26d = 4) %>%
  mutate(order_prompt_26e = 5) %>%
  mutate(order_prompt_26f = 6) %>%
  mutate(order_prompt_26g = 7) %>%
  mutate(order_survey_26 = 26) %>%
  mutate(other_26a = 0) %>%
  mutate(other_26b = 0) %>%
  mutate(other_26c = 0) %>%
  mutate(other_26d = 0) %>%
  mutate(other_26e = 0) %>%
  mutate(other_26f = 1) %>%
  mutate(other_26g = 0) %>%
  mutate(none_26a = 0) %>%
  mutate(none_26b = 0) %>%
  mutate(none_26c = 0) %>%
  mutate(none_26d = 0) %>%
  mutate(none_26e = 0) %>%
  mutate(none_26f = 0) %>%
  mutate(none_26g = 0) %>%
  mutate(dk_26a = 0) %>%
  mutate(dk_26b = 0) %>%
  mutate(dk_26c = 0) %>%
  mutate(dk_26d = 0) %>%
  mutate(dk_26e = 0) %>%
  mutate(dk_26f = 0) %>% 
  mutate(dk_26g = 1)

## 2018
### Q22
mpps_2018_res_22 <- mpps_2018_res %>% 
  select(1:16) %>% 
  mutate(nu_character_prompt_22a = 17) %>% 
  mutate(nu_character_prompt_22b = 79) %>% 
  mutate(nu_character_prompt_22c = 45) %>%
  mutate(nu_character_prompt_22d = 44) %>%
  mutate(nu_character_prompt_22e = 72) %>%
  mutate(nu_character_prompt_22f = 35) %>%
  mutate(nu_character_prompt_22g = 32) %>% 
  mutate(nu_character_prompt_22h = 21) %>% 
  mutate(professional_terms_22a = 0) %>%
  mutate(professional_terms_22b = 1) %>%
  mutate(professional_terms_22c = 1) %>%
  mutate(professional_terms_22d = 1) %>%
  mutate(professional_terms_22e = 1) %>%
  mutate(professional_terms_22f = 0) %>%
  mutate(professional_terms_22g = 0) %>%
  mutate(professional_terms_22h = 0) %>%
  mutate(nu_option_22 = 8) %>%
  mutate(order_prompt_22a = 1) %>%
  mutate(order_prompt_22b = 2) %>%
  mutate(order_prompt_22c = 3) %>%
  mutate(order_prompt_22d = 4) %>%
  mutate(order_prompt_22e = 5) %>%
  mutate(order_prompt_22f = 6) %>%
  mutate(order_prompt_22g = 7) %>%
  mutate(order_prompt_22h = 8) %>%
  mutate(order_survey_22 = 22) %>%
  mutate(other_22a = 0) %>%
  mutate(other_22b = 0) %>%
  mutate(other_22c = 0) %>%
  mutate(other_22d = 0) %>%
  mutate(other_22e = 0) %>%
  mutate(other_22f = 0) %>%
  mutate(other_22g = 0) %>%
  mutate(other_22h = 0) %>%
  mutate(none_22a = 0) %>%
  mutate(none_22b = 0) %>%
  mutate(none_22c = 0) %>%
  mutate(none_22d = 0) %>%
  mutate(none_22e = 0) %>%
  mutate(none_22f = 0) %>%
  mutate(none_22g = 0) %>%
  mutate(none_22h = 0) %>%
  mutate(dk_22a = 0) %>%
  mutate(dk_22b = 0) %>%
  mutate(dk_22c = 0) %>%
  mutate(dk_22d = 0) %>%
  mutate(dk_22e = 0) %>%
  mutate(dk_22f = 0) %>% 
  mutate(dk_22g = 0) %>% 
  mutate(dk_22h = 0)
### Q29
mpps_2018_res_29 <- mpps_2018_res %>% 
  select(1:8, 17:25) %>% 
  mutate(nu_character_prompt_29a = 41) %>% 
  mutate(nu_character_prompt_29b = 60) %>% 
  mutate(nu_character_prompt_29c = 48) %>%
  mutate(nu_character_prompt_29d = 31) %>%
  mutate(nu_character_prompt_29e = 60) %>%
  mutate(nu_character_prompt_29f = 68) %>%
  mutate(nu_character_prompt_29g = 24) %>% 
  mutate(nu_character_prompt_29h = 37) %>% 
  mutate(nu_character_prompt_29i = 10) %>% 
  mutate(professional_terms_29a = 0) %>%
  mutate(professional_terms_29b = 0) %>%
  mutate(professional_terms_29c = 0) %>%
  mutate(professional_terms_29d = 0) %>%
  mutate(professional_terms_29e = 0) %>%
  mutate(professional_terms_29f = 1) %>%
  mutate(professional_terms_29g = 0) %>%
  mutate(professional_terms_29h = 0) %>%
  mutate(professional_terms_29i = 0) %>%
  mutate(nu_option_29 = 9) %>%
  mutate(order_prompt_29a = 1) %>%
  mutate(order_prompt_29b = 2) %>%
  mutate(order_prompt_29c = 3) %>%
  mutate(order_prompt_29d = 4) %>%
  mutate(order_prompt_29e = 5) %>%
  mutate(order_prompt_29f = 6) %>%
  mutate(order_prompt_29g = 7) %>%
  mutate(order_prompt_29h = 8) %>%
  mutate(order_prompt_29i = 9) %>%
  mutate(order_survey_29 = 29) %>%
  mutate(other_29a = 0) %>%
  mutate(other_29b = 0) %>%
  mutate(other_29c = 0) %>%
  mutate(other_29d = 0) %>%
  mutate(other_29e = 0) %>%
  mutate(other_29f = 0) %>%
  mutate(other_29g = 0) %>%
  mutate(other_29h = 0) %>%
  mutate(other_29i = 0) %>%
  mutate(none_29a = 0) %>%
  mutate(none_29b = 0) %>%
  mutate(none_29c = 0) %>%
  mutate(none_29d = 0) %>%
  mutate(none_29e = 0) %>%
  mutate(none_29f = 0) %>%
  mutate(none_29g = 0) %>%
  mutate(none_29h = 0) %>%
  mutate(none_29i = 0) %>%
  mutate(dk_29a = 0) %>%
  mutate(dk_29b = 0) %>%
  mutate(dk_29c = 0) %>%
  mutate(dk_29d = 0) %>%
  mutate(dk_29e = 0) %>%
  mutate(dk_29f = 0) %>% 
  mutate(dk_29g = 0) %>% 
  mutate(dk_29h = 0) %>% 
  mutate(dk_29i = 1)
### Q31
mpps_2018_res_31 <- mpps_2018_res %>% 
  select(1:8, 26:30) %>% 
  mutate(nu_character_prompt_31a = 61) %>% 
  mutate(nu_character_prompt_31b = 54) %>% 
  mutate(nu_character_prompt_31c = 45) %>%
  mutate(nu_character_prompt_31d = 36) %>%
  mutate(nu_character_prompt_31e = 10) %>%
  mutate(professional_terms_31a = 0) %>%
  mutate(professional_terms_31b = 0) %>%
  mutate(professional_terms_31c = 0) %>%
  mutate(professional_terms_31d = 0) %>%
  mutate(professional_terms_31e = 0) %>%
  mutate(nu_option_31 = 5) %>%
  mutate(order_prompt_31a = 1) %>%
  mutate(order_prompt_31b = 2) %>%
  mutate(order_prompt_31c = 3) %>%
  mutate(order_prompt_31d = 4) %>%
  mutate(order_prompt_31e = 5) %>%
  mutate(order_survey_31 = 31) %>%
  mutate(other_31a = 0) %>%
  mutate(other_31b = 0) %>%
  mutate(other_31c = 0) %>%
  mutate(other_31d = 0) %>%
  mutate(other_31e = 0) %>%
  mutate(none_31a = 0) %>%
  mutate(none_31b = 0) %>%
  mutate(none_31c = 0) %>%
  mutate(none_31d = 0) %>%
  mutate(none_31e = 0) %>%
  mutate(dk_31a = 0) %>%
  mutate(dk_31b = 0) %>%
  mutate(dk_31c = 0) %>%
  mutate(dk_31d = 0) %>%
  mutate(dk_31e = 1)


## building models
model_2017 <- glm(response ~ nu_character_prompt, data = mpps_2017_res_wide_1, family = binomial, na.action = na.exclude)
summary(model_2017)
