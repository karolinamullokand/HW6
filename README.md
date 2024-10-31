# HW6
Study group: Karolina Mullokand

# 1

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
# 2
**Topic #1**

```
The biglasso Package: A Memory- and Computation-Efficient Solver for Lasso Model Fitting with Big Data in R
by Yaohui Zeng and Patrick Breheny

https://web.p.ebscohost.com/ehost/pdfviewer/pdfviewer?vid=1&sid=9f766dba-9a4a-40ee-b5c1-068c54b75692%40redis
-OR-
https://journal.r-project.org/archive/2021/RJ-2021-001/RJ-2021-001.pdf
```

*The *biglasso* article introduces a powerful R package designed for handling very large datasets, which is perfect for my project on household financial health. In this study, I’ll explore how credit card debt, student loans, and other debts impact household stability and wealth, using *biglasso* to manage and analyze the data efficiently. The package’s ability to handle high-dimensional data by loading only necessary parts into memory means I can work with comprehensive datasets like the Survey of Consumer Finances, which has many variables. With econometric techniques like lasso and elastic net regression, *biglasso* will help me pinpoint the most significant factors affecting household finances, providing a clearer picture of how different types of debt influence wealth outcomes.*

 **Topic 2**

 ```
Outstanding Debt and the Household Portfolio
Author(s): Thomas A. Becker and Reza Shabani
Source:
The Review of Financial Studies , July 2010, Vol. 23, No. 7 (July 2010), pp. 2900-
2934
Published by: Oxford University Press. Sponsor: The Society for Financial Studies.
Stable URL: https://www.jstor.org/stable/40782970
```

*The article *"Outstanding Debt and the Household Portfolio"* uses data from the Survey of Consumer Finances (SCF), a detailed, publicly accessible dataset collected by the Federal Reserve, which includes comprehensive information on U.S. household assets, liabilities, and demographics. This study explores how mortgage debt impacts households' investment choices, specifically their likelihood to own stocks or bonds, by integrating debt into a standard portfolio choice model. Econometric techniques used include logistic regression for predicting market participation and Tobit regression for portfolio share analysis, which effectively handle cases where certain assets (like stocks) may not be held at all by some households. The key questions addressed are how mortgage debt influences financial risk-taking (e.g., stock and bond ownership) and how debt repayment priorities affect asset allocation within household portfolios.*
