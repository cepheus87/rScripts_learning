# apply intro

# https://nsaunders.wordpress.com/2010/08/20/a-brief-introduction-to-apply-in-r/



# tapply ------------------------------------------------------------------

## generate data for medical example
medical.example <-
data.frame(patient = 1:100,
           age = rnorm(100, mean = 60, sd = 12),
           treatment = gl(2, 50, labels = c("Treatment", "Control")))


medical.example$group <-rep(LETTERS[1:4], 25)

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
tapply(medical.example$age, INDEX = medical.example$treatment, mean)
tapply(medical.example$age, INDEX = list(medical.example$treatment), mean)
tapply(medical.example$age, INDEX = list(medical.example$treatment, medical.example$group), mean)

 
## Baseball Example
tapply(baseball.example$batting.average, baseball.example$team, max)
