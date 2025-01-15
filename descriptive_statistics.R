# Project Deliverable 2

library(tidyverse)
library(rstatix)
library(ggplot2)
library(ggpubr)

# Importing and exploring dataset

# Reading loan_data.csv
loan_data <- read_csv("loan_data.csv")
loan_data
View(loan_data)
is.data.frame(loan_data)
str(loan_data)

## Data Cleaning

# Trying to find if there are any missing values in loan_data
which(is.na(loan_data))
# There were no missing values found in loan_data

# Trying to find if there are any rows 
loan_data[duplicated(loan_data),]
# There were no duplicate rows found in loan_data

## Data Manipulation

# relevant variables

# person_gender (male or female) - column 2
# person_education (high school, associate, bachelor, master, doctorate) - 
# column 3
# loan_int_rate - column 9
# loan_percent_income - column 10
# credit_score - column 12
# loan_status (0 for denied, 1 for approved) - column 14

# Keeping only columns of variables that are relevant to the research questions
loan_data <- loan_data[,c(2,3,9,10,12,14)]
view(loan_data)

# Exports the updated loan_data dataframe as updated_loan_data.csv file
write.csv(loan_data,"updated_loan_data.csv",row.names=F)

## Descriptive Statistics

# Gets summary statistics for the updated loan_data
summary(loan_data)

# Descriptive statistics for loan status by applicant's gender. Even though
# loan status is a category, the fact that it is an integer and it is 1 if
# approved and 0 if denied, getting a mean will provide proportion of loans
# approved for each gender category. 
loan_data %>%
  group_by(person_gender) %>%
  get_summary_stats(loan_status, type="mean_sd")
# The mean loan status (loan approval proportion) for both genders (male and 
# female) is 0.222. There does not seem to be much difference between loan
# approval rate based on gender.

# Descriptive statistics for loan status by applicant's education. Even though
# loan status is a category, the fact that it is an integer and it is 1 if
# approved and 0 if denied, getting a mean will provide proportion of loans
# approved for each education category. 
loan_data %>%
  group_by(person_education) %>%
  get_summary_stats(loan_status, type="mean_sd")
# The highest mean for loan status (loan approval proportion) is for doctorate
# degrees (0.229). They are followed by bachelor degrees (0.225), high 
# school (0.223), associate (0.22), and lastly, master degrees (0.218).

# Descriptive statistics for interest rate for loan by 
# loan status category (1 for approved, 0 for denied)
loan_data %>%
  group_by(loan_status) %>%
  get_summary_stats(loan_int_rate, type="mean_sd")
# The mean interest rate for loans was higher for cases where loan was
# approved (12.9%) when compared to cases where loan was denied (10.5%). This
# might suggest that loans with a higher interest rate have a higher chance
# of being approved.

# Descriptive statistics for loan amount as a proportion of annual income by 
# loan status category (1 for approved, 0 for denied)
loan_data %>%
  group_by(loan_status) %>%
  get_summary_stats(loan_percent_income, type="mean_sd")
# The mean loan amount as a proportion of annual income was higher for cases
# where loan was approved (0.203 or 20.3%) than cases where loan was denied 
# (0.122 or 12.2%). This might suggest that bigger loans compared to one's 
# income have a higher likelihood of being approved than smaller loans.

# Descriptive statistics for applicant's credit score  by 
# loan status category (1 for approved, 0 for denied)
loan_data %>%
  group_by(loan_status) %>%
  get_summary_stats(credit_score, type="mean_sd")
# The mean credit score of individuals who were denied the loan (633) is 1 
# point higher than those with loans approved (632). There might be some
# association between lower credit scores and a higher loan approval rate.

## Data Visualizations

# Factoring the loan_status so that when visualizations are made, the category
# clearly states Approved and Denied, instead of 1 and 0
loan_data$loan_status <- factor(loan_data$loan_status,
                                levels = c(0,1),
                                labels = c("Denied","Approved"))
View(loan_data)

# Creates a grouped bar chart with loan status on x axis and counts of each of
# loan status cases grouped by gender
ggplot(loan_data, aes(x = loan_status, fill = person_gender)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("male" = "lightblue", "female" = "pink")) +
  labs(title = "Loan Status by Gender", x = "Loan Status", y = "Cases")
# We can learn that males were both approved more loans and denied more loans
# than females. This corroborates the fact that the mean of the loan status
# (which is the proportion of loan approved due to 1 being approved and 0 being
# denied) is the same across both genders, since males were both approved more
# loans but also denied more loans. This is likely due to the fact that in the
# descriptive statistics, it was shown that there were more loan applications
# for males than females in the dataset.

# Creates a grouped bar chart with loan status on x axis and counts of each of
# loan status cases grouped by gender
ggplot(loan_data, aes(x = person_education, fill = loan_status)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Denied" = "indianred1", "Approved" = "seagreen3")) +
  labs(title = "Applicants Education by Loan Status", x = "Applicant Education", y = "Cases")
# We learn that for each education, most of the loans were denied. Furthermore,
# most loans were applied by people with bachelor degree and they also had 
# most loans approved. People with high school diploma and people with associate
# degree were second, as their denied and approved cases were approximately the 
# same. These were followed by people with master degree and finally, the lowest
# amount of applicants had a doctorate degree. However, it is interesting to 
# note that in descriptive statistics, doctorate degree had the highest 
# proportion of approval rate (the mean). After that, the mean basically 
# followed the order of the number of applications above: bachelors, high
# school, associate, and master. The fact that bachelor, high school, and 
# associate were above master in mean might be explained by the number of loans 
# applied by these applicants. This suggests that people usually apply for loans
# at this time, specially for academic reasons, which might be why their 
# previously calculated proportions of approval were so high. 

# Creates box plot for loan interest rate by loan status
ggboxplot(loan_data,
          x="loan_status", 
          y="loan_int_rate",
          title="Loan Interest Rate by Loan Status",
          xlab="Loan Status",
          ylab="Loan Interest Rate")
# We can learn that the interest rate median, lower quartile, upper quartile, 
# and upper whisker were higher for loans that were approved than those that 
# were not. However, the lower whisker for approved loans' interest rate is 
# approximately as low as those with denied loans, which means some lower 
# interest rate loans were also approved. However, for the reasons above and the
# fact that the few high interest rates for loans denied appear as outliers, we
# can say that higher interest rate loans tend to get higher rate of approval in
# this dataset.


# Creates box plot for loan amount as a proportion of annual income by loan
# status
ggboxplot(loan_data,
          x="loan_status", 
          y="loan_percent_income",
          title="Loan/Income Ratio by Loan Status",
          xlab="Loan Status",
          ylab="Loan/Income Ratio")
# We can learn that the loan/income ratio median, lower quartile, upper 
# quartile, and upper whisker were higher for loans that were approved than 
# those that were not. However, the lower whisker for approved loans' 
# loan/income ratio is approximately as low as those with denied loans, which 
# means some lower loan/income ratio loans were also approved. This seems to
# suggest that loans that are higher compared to one's income have a higher
# chance of approval. This is further corroborated by the fact that many values
# above about 0.3 loan/income ratio (loan is 30% of income) are considered
# outliers in the denied loan status. It is also interesting to note that the
# range between lower and upper quartile for approved loans was higher compared
# to denied laons. For denied laons, the ratio was more concentrated around the
# median. This also suggests that even though the loan/income ratio tends to be
# higher for approved loans, the range of the middle 50% of ratios is also
# larger.

# Creates box plot for applicant's credit score by loan status
ggboxplot(loan_data,
          x="loan_status", 
          y="credit_score",
          title="Credit Score by Loan Status",
          xlab="Loan Status",
          ylab="Credit Score")
# It is quite interesting to note that the credit score median, lower quartile, 
# upper quartile, lower whisker, and upper whisker are all approximately the
# same across both approved and denied loans. Furthermore, both categories of
# loan status also tends to have credit scores under about 500 identified as 
# outliers. However, the denied loan status category also tends to have outliers
# with credit scores above about 775. This at first glance can suggest that it
# is a rare case of high credit score getting denied a loan, however, this could
# be affected by another variable. Therefore, this graph suggests minimal 
# association between credit score and loan getting approved (1) or denied (0).
