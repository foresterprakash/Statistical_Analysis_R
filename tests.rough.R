## ANOVA

# Analysis of Variance

crop_data <- read.csv("E:/broadwaylearning/R-Training/Statistical Analysis in R/crop.data.csv")
summary(crop_data)
str(crop_data)

# changing the data structure as characters
crop_data$density <- as.character(crop_data$density)
crop_data$block <- as.character(crop_data$block)
crop_data$fertilizer <- as.character(crop_data$fertilizer)

str(crop_data)

# Understanding the character data 

unique(crop_data$density)
unique(crop_data$block)
unique(crop_data$fertilizer)
summary(crop_data$yield)

# Required package 
library(ggplot2)
library(dplyr)
library(ggpubr)
library(broom)
library(AICcmodavg)

## One way ANOVA
# lets have a look at the effect of fertilizer on crop yield

H0 <- "there is no significant effect in yield from different fertilizers level"
H1 <- "there is significant effect in yield from different fertilizers level"


# Perform one Way ANOVA

one_way <- aov(formula = yield ~ fertilizer, data = crop_data)
summary_anova <- summary(one_way)
summary_anova

# Extract p_value
p_value <- summary_anova[[1]]$`Pr(>F)`[1]
p_value

# Decision
ifelse(p_value < 0.05,H1,H0)

## Two way anova
# set up hypotheis for fertilizers
H0_fert <- "there is no significant effect in yield from different fertilizers level"
H1_fert <- "there is significant effect in yield from different fertilizers level"

# set up hypotheis for density
H0_dens <- "there is no significant effect in yield from different density level"
H1_dens <- "there is significant effect in yield from different density level"

## Perform two way anova

two_way <- aov(formula =  yield ~ fertilizer + density, data = crop_data)
summary_two_way <- summary(two_way)
summary_two_way

# Extract p value 
p_value_fertilizer <- summary_two_way[[1]]$`Pr(>F)`[1]
p_value_density <- summary_two_way[[1]]$`Pr(>F)`[2]

# Decision

ifelse(p_value_fertilizer < 0.05, H1_fert, H0_fert)
ifelse(p_value_density < 0.05, H1_dens, H0_dens)

## add one more variable on it .
three_var <- aov(formula = yield ~ fertilizer + density+ block, data = crop_data)
summary_three_var <- summary(three_var)
summary_three_var

## Lets see interaction effect also

interaction <- aov(formula = yield ~ fertilizer * density, data = crop_data)
summary_interaction <- summary(interaction)
p_value_int <- summary_interaction[[1]]$`Pr(>F)`[3]
p_value_int

## We have run 4 models here i.e, oneway, two way , three var and interaction.
## Lets see which combination of variable is best suited

library(AICcmodavg)
model_set <- list(one_way, two_way, interaction, three_var)
model_names <- c("One way ANOVA", "Two way ANOVA", "Interaction Effect Model",
                 "Modelling with Blocking")

aictab(model_set, modnames = model_names)

## Post hoc analysis

## Famous used post hoc analysis for anova is TukeyHSD analysis.
##(Honesty Significance Difference)

tukey.two_way <- TukeyHSD(two_way)
tukey.two_way

## Non-parametric test 

# use Plantgrowth 

data <- PlantGrowth
head(data)
unique(data$group)

result <- kruskal.test(weight ~ group, data = data)
result

H0 <- "No significant effect"
H1 <- "Significant effect"

ifelse(result$p.value< 0.05,H1,H0)

# Post hoc analysis

# Dunn test

library(dunn.test)
post_hoc <- dunn.test(data$weight, data$group, method = "bonferroni")

income


## Chi-squared test

library(MASS) # for data set

dat <- survey

str(dat)

## Create a data frame from the main data set.

stu_data <- data.frame(dat$Smoke, dat$Exer)

## Cretae a contigency table for data preparation

stu_data1 <- table(dat$Smoke, dat$Exer)
stu_data1

## Setting up hypothesis

null_hypo <- "The smoking levels and excercise levels are independent to each other"
alter_hypo <- "There is a significant relationship between smoking levels and excercise level"

## Perform chi-square test

chi_result <- chisq.test(stu_data1)
chi_result

## P_value extraction

p_value <- chi_result$p.value
p_value

## Decision

ifelse(p_value < 0.05, alter_hypo, null_hypo)

# Another factors sex and smoke

studata2 <- table(dat$Sex, dat$Smoke)
studata2

chisq.test(studata2)

studata3 <- table(dat$Sex,dat$Exer)
studata3

chisq.test(studata3)


## Correlation 

# The relationship between two or more continuous variable.

# 1. Pearsons - (data normal chha bhane or data has no outliers)
# 2. Spearmanns rank coefficient (if data has outliers and not distributed normally)
# 3. Kundell

cor.mat <- mtcars %>%
  select(mpg, disp, hp, drat, wt, qsec) %>%
  cor_mat()

# Visualize correlation matrix
#::::::::::::::::::::::::::::::::::::::::::
# Full correlation matrix,
# insignificant correlations are marked by crosses
cor.mat %>% cor_plot()
