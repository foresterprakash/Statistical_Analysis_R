## Statistical Test in R

## Inference statistics

## Sample statistics defines population parameter.

## One sampled T-test.

tips <- read.csv("E:/broadwaylearning/R-Training/tips.csv")
head(tips)
str(tips)

library(dplyr)
library(ggplot2)

H0 <- "The true value of average tip is $2.50"
H1 <- "The true value of average tip differs from $2.50"

test_result <- t.test(tips$tip, alternative = "two.sided", mu = 2.50)
test_result

p_value <- test_result$p.value
p_value
# How to make a decision using p value 

# If the calculated t value is greater than the tabulated t value, then we reject the null hypothesis.

# But we will use p-value to make a decision.

# The rule for decision making using p-value is that, if p_value is less
# Than 0.05 (alpha) then we reject null hypothesis other wise 
# We fail to reject null hypothesis.

ifelse(p_value < 0.05,H1,H0)

## Two sample t-test

library(ggplot2)
ggplot(data = tips,
       mapping = aes(x = tip, fill = sex)) +
  geom_histogram()


aggregate(tip ~ sex, data = tips, var)


# Check if the data is normal

norm_tip <- shapiro.test(tips$tip)
norm_tip

ifelse(norm_tip$p.value < 0.05, "Alternative Hypothesis", "Null hypothesis")

norm_male <- shapiro.test(tips$tip[tips$sex == "Male"])
norm_male


norm_female <- shapiro.test(tips$tip[tips$sex == "Female"])
norm_female

# The data is deviated from normality so that we cannot use parametric methods of 
# Variance test i.e., levene test, so that we have to use ansari-bradley test which is
#non parametric method of variance test.

var_test <- ansari.test(tip~sex, data = tips)
var_test


test_2 <- t.test(data = tips, tip~sex, var.equal = TRUE)
test_2

## Paired sample t-test
#install.packages("UsingR")
library(UsingR)

head(father.son)

p_test <- t.test(father.son$fheight, father.son$sheight, paired = TRUE)
p_test

H0 <- "There is no significant difference in fathers and sons height."
H1 <- "There is a significant difference in fathers and sons height."

# Decision:

p_test$p.value
ifelse(p_test$p.value < 0.05, H1, H0)


## Linear Regression

### Simple linear Regression (only one independent variable)

### Multiple linear regression (multiple independent variable)

# Load data set for simple linear regression
income <- read.csv("E:/broadwaylearning/R-Training/Statistical Analysis in R/income.data.csv")

summary(income)

# Correlation test
cor.test(income$income,income$happiness)

#install.packages("broom")
#install.packages("ggpubr")

pacman::p_load(ggplot2,broom,dplyr,ggpubr)

## Check the assumptions

# Independence of observation

# Normality of dependent variable

hist(income$happiness)

# It seems the kde plot will generate a roughly bell shaped curve 
#so that our data follows normality

# Linearity

plot(happiness ~ income, data = income)

# the plot shows the linearity

# Homoscedaticity will be checked after model formulation

# Now perform th linear regression

income_happiness <-  lm(happiness ~ income, data = income)
summary(income_happiness)

## Check Homoscedasticity

par(mfrow = c(2,2))
plot(income_happiness)
par(mfrow = c(1,1))

## Model performance
library(easystats)

performance(income_happiness)

report(income_happiness)

## Visualize in Graph

ggplot(data = income,
       mapping = aes(x = income, y = happiness)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red", se = FALSE) +
  stat_regline_equation(label.x = 3, label.y = 7) +
  theme_bw() +
  labs(title = "Report of income and happiness",
       x = "Income(in 10000 $)",
       y = "Happiness in scale 1 to 10")


