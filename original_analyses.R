library('dplyr')
library('hash')

#read file
data <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/parkinsons/telemonitoring/parkinsons_updrs.data"))

# sort data so that time points and subjects are ordered from lowest to highest
arr_data <- arrange(data, subject., test_time)


# ideas:

# 1. select time points to analyze Total_UPDRS score correlation 
# a) between the 42 patients 
# b) between the two genders of patients 
# c) in conjunction with age

# 2. Use correlation data to suggest a hypothesis regarding the relationship between these three variables
# a) 2-sample t-test test: is avg change in Total_UPDRS over time different between males/females?
# b) 2-sample t-test test: is avg change in Total_UPDRS over time different between age groups?
    # age = <=65 (# = 2893) / >65 (# = 2982)


# attempting to organize a mess of data
subjects <- hash()
stimes <- c(10, 30, 50, 70, )

for (i in 1:42) {
  updrs <- data$total_UPDRS[which((data$subject. == i)) & (data$test_time )]
  subjects[[toString(i)]] <- updrs
}
