library(haven)
library(SteinIV)
library(estimatr)
library(dplyr)
library(tidyverse)
library(ivpack)
library(stargazer)
library(lmtest)
library(lfe)


data1 <- read_dta("D:/Replication_Exercises/Mixtape_IV_JudgeFE_JIVE/R/judge_fe.dta")

data1$bailDate1 <- as.numeric(data1$bailDate) 

## OLS with normal standard errors

model_ols_min <- lm(guilt ~ jail3 + day + day2 + day3 + bailDate1 + t1 + t2 + t3 + t4 + t5, data = data1)
model_ols_max <- lm(guilt ~ jail3 + day + day2 + day3 + bailDate1 + t1 + t2 + t3 + t4 + t5 + fel + mis + sum + F1 + F2 + F3 + F + M1 + M2 + M3 + M + age + black + male + white + priorCases + priorWI5 + prior_felChar + prior_guilt + onePrior + threePriors + possess + drugSell + aggAss + DUI1st + robbery, data = data1)                         

stargazer(model_ols_min, model_ols_max,type = "text", keep = "jail3", keep.stat = c("n","rsq","ser"))

## IV Regression with normal standard errors 
## Codes of this section also available on causal inference mixtape online 
## Visit the link: https://mixtape.scunning.com/instrumental-variables.html?panelset1=r-code2#judge-fixed-effects

judge_pre <- data1 %>% select(starts_with("judge_")) %>% colnames() %>% subset(., . != "judge_pre_8") %>% paste(., collapse = "+")

controls2 <- data1 %>% select(day, day2, bailDate1, t1, t2, t3, t4, t5) %>% colnames() %>% paste(., collapse = "+")

off <- data1 %>% select(fel, mis, sum, F1, F2, F3, M1, M2, M3, M) %>% colnames() %>% paste(., collapse = "+")

demo <- data1 %>% select(age, male, black, white) %>% colnames() %>% paste(., collapse = "+")

prior <- data1 %>% select(priorCases, prior_felChar, prior_guilt, priorWI5, onePrior, threePriors) %>% colnames() %>% paste(., collapse = "+")

min_iv_formula <- as.formula(paste("guilt ~ ", controls2, "| 0 | (jail3 ~ 0 +", judge_pre,")"))

max_iv_formula <- as.formula(paste("guilt ~", demo,  "+ possess +", off, "+ robbery +", prior, "+ aggAss +", controls2, "+ drugSell + DUI1st  | 0 | (jail3 ~ 0 +", judge_pre,")" ))

min_iv_formula
max_iv_formula

iv_min <-  felm(min_iv_formula, data = data1)
summary(iv_min)

iv_max <- felm(max_iv_formula, data = data1)
summary(iv_max)


## JIVE estimation 

X_min <- data1 %>% select(jail3, day, day2, t1, t2, t3, t4, t5, bailDate1 ) %>% model.matrix(data = ., ~.)
Z_min <- data1 %>%  select(-judge_pre_8) %>% select(starts_with("judge_"), day, day2, t1, t2, t3, t4, t5, bailDate1 ) %>% model.matrix(data = ., ~.)

min_jive <- jive.est(data1$guilt, X_min, Z_min)
min_jive$est



X_max <- data1 %>% select(jail3,day, day2, t1, t2, t3, t4, t5, bailDate1, fel, mis, sum, F1, F2, F3, M1, M2, M3, M, age, male, black, white, priorCases, prior_felChar, prior_guilt, priorWI5, onePrior, threePriors, possess, robbery, aggAss, DUI1st, drugSell) %>% model.matrix(data = ., ~.)
Z_max <- data1 %>% select(-judge_pre_8) %>% select(starts_with("judge_pre"),day, day2, t1, t2, t3, t4, t5, bailDate1, fel, mis, sum, F1, F2, F3, M1, M2, M3, M, age, male, black, white, priorCases, prior_felChar, prior_guilt, priorWI5, onePrior, threePriors, possess, robbery, aggAss, DUI1st, drugSell ) %>% model.matrix(data = ., ~.) 

max_jive <- jive.est(data1$guilt, X_max, Z_max)
max_jive$est









q()
y
