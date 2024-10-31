# HW6
Study group: Karolina Mullokand

**Creating usual R environment**

```
library(ggplot2)
library(tidyverse)
library(haven)

setwd("/Users/karolinamullokand/Desktop/ECO_B2000")
load("ACS_2021_couples.RData")
```

**Running the code from the Lab6**

```
acs2021_couples$age_diff <- acs2021_couples$AGE - acs2021_couples$h_age
View(acs2021_couples$h_age)
summary(acs2021_couples)
summary(acs2021_couples$age_diff[(acs2021_couples$SEX == "Female")&(acs2021_couples$h_sex == "Male")])
summary(acs2021_couples$age_diff[(acs2021_couples$SEX == "Male")&(acs2021_couples$h_sex == "Female")])
summary(acs2021_couples$age_diff[(acs2021_couples$SEX == "Male")&(acs2021_couples$h_sex == "Male")])
summary(acs2021_couples$age_diff[(acs2021_couples$SEX == "Female")&(acs2021_couples$h_sex == "Female")])

summary(acs2021_couples$AGE[(acs2021_couples$SEX == "Female")&(acs2021_couples$h_sex == "Male")])
summary(acs2021_couples$h_age[(acs2021_couples$SEX == "Female")&(acs2021_couples$h_sex == "Male")])

acs2021_couples$educ_numeric <- fct_recode(acs2021_couples$EDUC,
                                           "0" = "N/A or no schooling",
                                           "2" = "Nursery school to grade 4",
                                           "6.5" = "Grade 5, 6, 7, or 8",
                                           "9" = "Grade 9",
                                           "10" = "Grade 10",
                                           "11" = "Grade 11",
                                           "12" = "Grade 12",
                                           "13" = "1 year of college",
                                           "14" = "2 years of college",
                                           "15" = "3 years of college",
                                           "16" = "4 years of college",
                                           "17" = "5+ years of college")

acs2021_couples$educ_numeric <- as.numeric(levels(acs2021_couples$educ_numeric))[acs2021_couples$educ_numeric]

acs2021_couples$h_educ_numeric <- fct_recode(acs2021_couples$h_educ,
                                             "0" = "N/A or no schooling",
                                             "2" = "Nursery school to grade 4",
                                             "6.5" = "Grade 5, 6, 7, or 8",
                                             "9" = "Grade 9",
                                             "10" = "Grade 10",
                                             "11" = "Grade 11",
                                             "12" = "Grade 12",
                                             "13" = "1 year of college",
                                             "14" = "2 years of college",
                                             "15" = "3 years of college",
                                             "16" = "4 years of college",
                                             "17" = "5+ years of college")

acs2021_couples$h_educ_numeric <- as.numeric(levels(acs2021_couples$h_educ_numeric))[acs2021_couples$h_educ_numeric]

acs2021_couples$educ_diff <- acs2021_couples$educ_numeric - acs2021_couples$h_educ_numeric

acs_subgroup <- acs2021_couples %>% filter((AGE >= 25) & (AGE <= 55) & 
                                             (LABFORCE == 2) & (WKSWORK2 > 4) & (UHRSWORK >= 35) )
```

**Building regression model**

```
summary(acs_subgroup)

model_1 <- lm(educ_numeric ~ AGE + h_educ_numeric, data = acs_subgroup) #First attempt, didn't get it right, reread question in Lab6
model_asked <- lm(age_diff ~ educ_diff + educ_numeric + h_educ_numeric + poly(AGE,2) + poly(h_age,2), data = acs_subgroup)
names(acs_subgroup) #checked spelling of variables in dataset, becausw of occuring error
summary(model_asked) # (proud of myself)
```

**Creating a visual representation of creted model**

```
a1 <- ggplot(acs_subgroup, aes(x = educ_diff, y = age_diff)) +
  geom_point(alpha =0.3, color = "blue") +
  geom_smooth(method = "lm",color = "red") +
  labs(title = "Age Difference vs Education difference",
       x = "Educ Difference(years)",
       y = "Age Difference(Years)") +
  theme_minimal()

install.packages("plotly")
library(plotly)

fun <- ggplotly(a1)

fun
```

**Hypothesis test time**

```
#extracting data from the dataset
total_observations <- nrow(acs_subgroup)
significant_age_diff <- sum(abs(acs_subgroup$age_diff) > 5)  # Assuming age difference > 5 years is significant

#proportions
prop_significant <- significant_age_diff / total_observations

#hypothesis test
H0 <- "The proportion of couples with significant age difference is 0.5"
HA <- "The proportion of couples with significant age difference is not 0.5"

#performing proportion test
prop_test <- prop.test(significant_age_diff, total_observations, p = 0.5, alternative = "two.sided", conf.level = 0.95)

#extracting key statistics
estimate <- prop_test$estimate
standard_error <- sqrt(estimate * (1 - estimate) / total_observations)
z_stat <- (estimate - 0.5) / standard_error
p_value <- prop_test$p.value

#results
print(paste("Proportion of significant age differences:", estimate))
print(paste("Standard Error:",standard_error ))
print(paste("Z-statistic:", z_stat))
print(paste("p-value:", p_value))

#interpretation
if(p_value < 0.05) {
  print("Result: Reject null hypothesis")
  print("The proportion of couples with significant age difference is not 0.5")
} else {
  print("Result: Fail to reject null hypothesis")
  print("There is not enough evidence to conclude that the proportion of couples with significant age difference differs from 0.5")
}

#confidence interval
conf_int <- prop_test$conf.int
print(paste("95% Confidence Interval:", round(conf_int[1], 4), "to", round(conf_int[2], 4)))
```
