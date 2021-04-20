library('dplyr')
library('hash')

#read file
data <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/parkinsons/telemonitoring/parkinsons_updrs.data"))

# sort data so that time points and subjects are ordered from lowest to highest
arr_data <- arrange(data, subject., test_time)

times <- hash()
wtimes <- c()
for (i in 1:6) {
  
}

subjects <- hash()
stimes <- c(10, 30, 50, 70, )
for (i in 1:42) {
  updrs <- data$total_UPDRS[which((data$subject. == i)) & (data$test_time )]
  subjects[[toString(i)]] <- updrs
}
