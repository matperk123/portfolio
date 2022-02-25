rm(list=ls()) #removes all variables stored previously
library(Hmisc)

data <- read.csv("C:/Users/12092/Desktop/Covid_R Project/COVID19_line_list_data.csv")
describe(data) # Hmisc Command

# cleaned up death column
data$death_dummy <- as.integer(data$death != 0)

# death rate
sum(data$death_dummy) / nrow(data)

# Age
# Claim: People who die are substantially older than those that have lives
dead = subset(data, death_dummy == 1)
alive = subset(data, death_dummy == 0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)
# Are these results significant?
t.test(alive$age, dead$age, alternative = "two.sided", conf.level = 0.99)

# normally, if p-value < 0.05, we reject null hypothesis
# here, p-value ~0 so we reject the null hypothesis and conclude that this is statistically significant

#Gender:
# claim: gender has no effect
men = subset(data, gender == "male")
women = subset(data, gender == "female")
mean(men$death_dummy, na.rm = TRUE)
mean(women$death_dummy, na.rm = TRUE)
# Are these results significant?
t.test(men$death_dummy, women$death_dummy, alternative = "two.sided", conf.level = 0.99)
#99% confidence: men have from 0.8% to 8.8% higher change of dying that a woman
# p-value = 0.002 < 0.05, so this is statistically significant
