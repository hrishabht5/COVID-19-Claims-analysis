rm(list=ls()) # removes all variables stored previously
library(Hmisc) # import

data <- read.csv("C:/Users/hrish/OneDrive/Desktop/COVID_R/COVID19_line_list_data.csv")
describe(data) # Hmisc command

# cleaned up death column
data$death_dummy <- as.integer(data$death != 0)
data$death_dummy

# Death Rate 

Death_Rate <- sum(data$death_dummy) / nrow(data)
Death_Rate

# Age

# Claim : people who die are older than the people who survived

dead = subset(data, death_dummy == 1)
alive = subset(data, death_dummy == 0)

mean_age$dead <- mean(dead$age, na.rm = TRUE)
mean_age$dead

mean_age$alive <- mean(alive$age, na.rm = TRUE)
mean_age$alive

# is this statistically significant?

t.test(alive$age, dead$age, alternative = "two.sided", conf.level = 0.99)

# normally, if p-value < 0.05, we reject null hypothesis
# here, p-value ~ 0, so we reject the null hypothesis and 
# conclude that this is statistically significant

Claim = mean_age$dead > mean_age$alive
Claim

# Gender

# Claim2 : gender has no effect on death

men <- subset(data, death_dummy = "men")
women <- subset(data, death_dummy = "women")

mean_death_men <- mean(men$death_dummy, na.rm = TRUE)
mean_death_men

mean_death_women <- mean(women$death_dummy, na.rm = TRUE)
mean_death_women

# is this statistically significant?
t.test(men$death_dummy, women$death_dummy, alternative="two.sided", conf.level = 0.99)

# p-value = 1 > 0.05, so this is not statistically
# significant

Claim2 = mean_death_men > mean_death_women
Claim2
