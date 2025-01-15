library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)


# Reading updated_loan_data.csv
loan_data <- read_csv("updated_loan_data.csv")
loan_data

# Factoring the loan_status so that when visualizations are made, the category
# clearly states Approved and Denied, instead of 1 and 0
loan_data$loan_status <- factor(loan_data$loan_status,
                                levels = c(0,1),
                                labels = c("Denied","Approved"))
View(loan_data)
is.data.frame(loan_data)
str(loan_data)


## person_gender and loan_status -> Chi-Square Test

# Assumptions for Chi-Square Test

# Independence of data points, satisfied by between-group design
# Sample size is not too small (45000 rows of data)
# The shape of the contingency table is 2x2
# Each combination in contingency table has atleast 4000 data points, which is
# sufficient

contingency_table <- table(loan_data$person_gender, loan_data$loan_status)
contingency_table

# Running chi-square test
chi_square_test <- chisq.test(contingency_table)
chi_square_test

# The p-value resulting from chi-square test is 0.9143. Considering alpha level
# of 0.05, 0.9143 is greater than 0.05. Therefore, the p-value is
# non-significant and there is no statistically significant association between 
# loan_status and person_gender.


## person_education and loan_status -> Chi-Square Test

# Assumptions for Chi-Square Test

# Independence of data points, satisfied by between-group design
# Sample size is not too small (45000 rows of data)
# The shape of the contingency table is 2x5
# Each combination in contingency table has atleast 100 data points, which is
# sufficient

contingency_table <- table(loan_data$person_education, loan_data$loan_status)
contingency_table

# Running chi-square test
chi_square_test <- chisq.test(contingency_table)
chi_square_test

# The p-value resulting from chi-square test is 0.7328. Considering alpha level
# of 0.05, 0.7328 is greater than 0.05. Therefore, the p-value is
# non-significant and there is no statistically significant association between 
# loan_status and person_education.


## loan_int_rate and loan_status -> Independent Sample t Test

# checking assumptions

# independence assumption is satisfied, as each case appears only once in the 
# dataset and does not influence other data points

## checking for outliers
loan_data %>%
  group_by(loan_status) %>%
  identify_outliers(loan_int_rate) %>%
  print(n = Inf)
  
# 70 outliers were identified, all in the denied loan status category. 
# But none of them are extreme outliers, so assumption is met.

# extract a subset of random sample data from each loan status 
set.seed(123)
int_rate_sample <- loan_data %>% sample_n_by(loan_status, size = 5000)
int_rate_sample

# checking normality using the Shapiro Wilk test
int_rate_sample %>%
  group_by(loan_status) %>%
  shapiro_test(loan_int_rate)

# p value of the test is smaller than 0.05 for both loan_status conditions. The 
# data is not normally distributed. 

# Since the data points are not close to the trend line in qq plot, the data not
# normally distributed, so assumption is not met
ggqqplot(int_rate_sample, x = "loan_int_rate", facet.by = "loan_status")

# Non-parametric test is a better choice for this dataset

# Mann-Whitney U Test

# checking assumptions

# independence assumption is satisfied, as each case appears only once in the 
# dataset and does not influence other data points
# The dependent variable (loan_int_rate) is continuous

# Checking shape of group distributions
# Creates box plot for loan interest rate by loan status
ggboxplot(loan_data,
          x="loan_status", 
          y="loan_int_rate",
          title="Loan Interest Rate by Loan Status",
          xlab="Loan Status",
          ylab="Loan Interest Rate")

# Although the size of the box are the same, the positioning is different and
# the shape is different. However, Mann-Whitney will compare medians rather than
# overall distributions.

# Mann-Whitney U Test run on entire loan_data
mwu <- wilcox.test(loan_int_rate ~ loan_status, data = loan_data, exact = FALSE)
mwu

# the p-value is less than 2.2e-16, which is less than the alpha level of 0.05,
# therefore, there is significant association between loan_int_rate and 
# loan_status.


## loan_percent_income and loan_status -> Independent Sample t Test

# checking assumptions

## checking for outliers
extreme_outliers <- loan_data %>%
                      group_by(loan_status) %>%
                      identify_outliers(loan_percent_income) %>%
                      filter(is.extreme == TRUE) %>%
                      print(n = Inf)
# 44 extreme outliers were identified, all in the denied loan status category. 
# The resulting dataframe was saved to extreme_outliers so that these rows can
# be removed from the loan_data dataframe.

# resulting dataframe with extreme outliers
extreme_outliers
is.data.frame(extreme_outliers)

# Removing all rows from loan_data that match rows in extreme_outliers
loan_data <- loan_data %>%
  anti_join(extreme_outliers, by = c("loan_status", "person_gender",
                                     "person_education", "loan_int_rate",
                                     "loan_percent_income", "credit_score"))

# outliers checked again
loan_data %>%
  group_by(loan_status) %>%
  identify_outliers(loan_percent_income) %>%
  filter(is.extreme == TRUE) %>%
  print(n = Inf)

# No more extreme outliers were found, assumption now met.

# extract a subset of random sample data from each loan status 
set.seed(123)
perc_inc_sample <- loan_data %>% sample_n_by(loan_status, size = 5000)
perc_inc_sample

# checking normality using the Shapiro Wilk test
perc_inc_sample %>%
  group_by(loan_status) %>%
  shapiro_test(loan_percent_income)

# p value of the test is smaller than 0.05 for both loan_status conditions. The 
# data is not normally distributed. 

# Since the data points are not close to the trend line in qq plot, the data not
# normally distributed, so assumption is not met
ggqqplot(perc_inc_sample, x = "loan_percent_income", facet.by = "loan_status")

# Non-parametric test is a better choice for this dataset

# Mann-Whitney U Test

# checking assumptions

# independence assumption is satisfied, as each case appears only once in the 
# dataset and does not influence other data points
# The dependent variable (loan_percent_income) is continuous

# Checking shape of group distributions
# Creates box plot for loan amount as a proportion of annual income by loan
# status
ggboxplot(loan_data,
          x="loan_status", 
          y="loan_percent_income",
          title="Loan/Income Ratio by Loan Status",
          xlab="Loan Status",
          ylab="Loan/Income Ratio")

# The size of the boxes are different, the positioning is different and
# the shape is different. However, Mann-Whitney will compare medians rather than
# overall distributions.

# Mann-Whitney U Test run on entire loan_data
mwu <- wilcox.test(loan_percent_income ~ loan_status, data = loan_data, 
                   exact = FALSE)
mwu

# the p-value is less than 2.2e-16, which is less than the alpha level of 0.05,
# therefore, there is significant association between loan_percent_income and 
# loan_status.


## credit_score and loan_status -> Independent Sample t Test

# checking assumptions

## checking for outliers
extreme_outliers <- loan_data %>%
  group_by(loan_status) %>%
  identify_outliers(credit_score) %>%
  filter(is.extreme == TRUE) %>%
  print(n = Inf)

# 1 extreme outlier was identified in the denied loan status category. 
# The resulting dataframe was saved to extreme_outliers so that these rows can
# be removed from the loan_data dataframe.

# resulting dataframe with extreme outliers
extreme_outliers
is.data.frame(extreme_outliers)

# Removing all rows from loan_data that match rows in extreme_outliers
loan_data <- loan_data %>%
  anti_join(extreme_outliers, by = c("loan_status", "person_gender",
                                     "person_education", "loan_int_rate",
                                     "loan_percent_income", "credit_score"))

# outliers checked again
loan_data %>%
  group_by(loan_status) %>%
  identify_outliers(credit_score) %>%
  filter(is.extreme == TRUE) %>%
  print(n = Inf)

# No more extreme outliers were found, assumption now met.

# extract a subset of random sample data from each loan status 
set.seed(123)
cred_sc_sample <- loan_data %>% sample_n_by(loan_status, size = 5000)
cred_sc_sample

# checking normality using the Shapiro Wilk test
cred_sc_sample %>%
  group_by(loan_status) %>%
  shapiro_test(credit_score)

# p value of the test is smaller than 0.05 for both loan_status conditions. The 
# data is not normally distributed. 

# Since the data points are not close to the trend line in qq plot, the data not
# normally distributed, so assumption is not met
ggqqplot(cred_sc_sample, x = "credit_score", facet.by = "loan_status")

# Non-parametric test is a better choice for this dataset

# Mann-Whitney U Test

# checking assumptions

# independence assumption is satisfied, as each case appears only once in the 
# dataset and does not influence other data points
# The dependent variable (loan_percent_income) is continuous

# Checking shape of group distributions
# Creates box plot for applicant's credit score by loan status
ggboxplot(loan_data,
          x="loan_status", 
          y="credit_score",
          title="Credit Score by Loan Status",
          xlab="Loan Status",
          ylab="Credit Score")

# The size, shape, and positioning of the boxplots are very similar, assumption
# satisfied.

# Mann-Whitney U Test run on entire loan_data
mwu <- wilcox.test(credit_score ~ loan_status, data = loan_data, 
                   exact = FALSE)
mwu

# the p-value is 0.07313, which is greater than the alpha level of 0.05,
# therefore, there is no significant association between credit_score and 
# loan_status.