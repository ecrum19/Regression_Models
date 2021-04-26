library(hash)
library(ggplot2)

data <- read.csv('/Users/eliascrum/Desktop/2021-Spring/STAT_437/Group Project/ObesityDataSet_fixed.csv')
columns <- colnames(data)
cond <- as.numeric(data$condition)
shapiro.test(cond)    # data is not normally distributed

cors_v <- rep(NA, 16)
cors_p <- rep(NA, 16)

for (i in 1:length(columns)) {
  if (i != 17) {
    c <- cor.test(data$condition, as.numeric(data[,i]))
    print(c)
    cors_v[i] <- c$estimate
    cors_p[i] <- c$p.value
  }
}

sig_cors <- matrix(NA, nrow = 16, ncol = 2, byrow = TRUE) 
for (j in 1:length(cors_p)) {
  if (cors_p[j] < 0.05) {
    sig_cors[j,] <- c(columns[j], cors_v[j])
  }
}

sig_cors <- sig_cors[-which(is.na(sig_cors)),]
sig_cors_data <- data.frame(variables = sig_cors[,1], correlations = sapply(sig_cors[,2], as.numeric))
ggplot(sig_cors_data, aes(sig_cors_data$variables, sig_cors_data$correlations, color = sig_cors_data$correlations)) + geom_point() + 
  geom_hline(yintercept = 0.4, color = 'red')
