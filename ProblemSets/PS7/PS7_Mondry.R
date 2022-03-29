library(tidyverse)
library(magrittr)
library(mice)
library(modelsummary)
options('modelsummary_format_numeric_latex' = 'plain') # disabling special LaTeX coding of numeric entries

# reading in data
wages <- read.csv('wages.csv')

# dropping obs with missing HGC or tenure
wages %<>% filter(!(is.na(hgc) | is.na(tenure)))

# summary table for numeric features
wages %>% datasummary(formula = logwage + hgc + tenure + age ~ NUnique + PercentMissing + Mean + SD + Min + Median + Max, 
                      output = 'latex')        
# frequency tables for character features
wages %>% datasummary(formula = college + married ~ N,
                      output = 'latex')

### looking for correlations with missing values

# college factor frequencies
wages %>% 
  filter(is.na(wages)) %>%
  `[`(,'college') %>%
  table %>%
  `/`(table(wages$college)) %>%
  round(3)

# married factor frequencies
wages %>% 
  filter(is.na(wages)) %>%
  `[`(,'married') %>%
  table %>%
  `/`(table(wages$college)) %>%
  round(3)

# hgc mean
c(wages %>%
    filter(is.na(wages)) %>%
    `[`(,'hgc') %>%
    mean,
  wages %>%
    filter(!is.na(wages)) %>%
    `[`(,'hgc') %>%
    mean)

# tenure
c(wages %>%
    filter(is.na(wages)) %>%
    `[`(,'tenure') %>%
    mean,
  wages %>%
    filter(!is.na(wages)) %>%
    `[`(,'tenure') %>%
    mean)

# age
c(wages %>%
    filter(is.na(wages)) %>%
    `[`(,'age') %>%
    mean,
  wages %>%
    filter(!is.na(wages)) %>%
    `[`(,'age') %>%
    mean)

### imputation and regression

# complete cases
lm1 <- wages %>%
  filter(!is.na(logwage)) %>%
  lm(formula = logwage ~ hgc + college + tenure + I(tenure^2) + age + married)

# mean imputation
wages.mean <- wages
wages.mean[is.na(wages$logwage),'logwage'] <- 
  wages[!is.na(wages$logwage),'logwage'] %>%
  mean
lm2 <- wages.mean %>%
  lm(formula = logwage ~ hgc + college + tenure + I(tenure^2) + age + married)

# imputation with OLS
pred.lm1 <- predict(lm1, newdata = wages)
lm3 <- wages %>%
  lm(formula = ifelse(is.na(logwage),
                      yes = pred.lm1,
                      no  = logwage) ~ 
       hgc + college + tenure + I(tenure^2) + age + married)

# multiple imputation
wages.mice <- mice(wages, printFlag = FALSE)
lm4 <- with(wages.mice, lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married))

### model summary table
modelsummary(list('Complete cases' = lm1,
                  'Mean imputation' = lm2,
                  'OLS imputation' = lm3,
                  'Multiple imputation (m=5)' = lm4),
             output = 'latex')
