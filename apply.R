# apply intro

## generate data for medical example
medical.example <-
data.frame(patient = 1:100,
           age = rnorm(100, mean = 60, sd = 12),
           treatment = gl(2, 50, labels = c("Treatment", "Control")))

summary(medical.example)
head(medical.example)


baseball.example <-
  data.frame(team = gl(5, 5,
                       labels = paste("Team", LETTERS[1:5])),
             player = sample(letters, 25),
             batting.average = runif(25, .200, .400))

summary(baseball.example)


## Generic Example
## tapply(Summary Variable, Group Variable, Function)
 
## Medical Example
tapply(medical.example$age, medical.example$treatment, mean)
 
## Baseball Example
tapply(baseball.example$batting.average, baseball.example$team, max)
