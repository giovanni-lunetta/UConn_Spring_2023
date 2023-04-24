# Load the required packages
library(stats)
library(sandwich)
library(lmtest)
library(haven)
library(ggplot2)
library(outliers)
library(dplyr)

# QUESTION 1
# 1. Complete parts a and b of Q2 on pp 459-60 in the textbook

# read the .dta file
survey_data <- read_dta("Ch12_Exercise2_Global_warming.dta")

# -------------------------------------------------------------------------------
# 1.(a)(i) Looking at the estimated coefficients, the variable that appears to have 
# the most important influence on the opinion that global warming is real and 
# caused by humans is partisan identification (Party7). This is indicated by the 
# relatively large coefficient of 0.4404 and the very low p-value (<2e-16), 
# which suggests strong statistical evidence that the variable is associated 
# with the outcome.

# Estimate logistic regression model
lpm <- glm(humancause ~ male + white + educ + incomecat + age + party7, data = survey_data)

# Print summary of regression results
summary(lpm)

# -------------------------------------------------------------------------------
# 1.(a)(ii) The minimum fitted value from the model is -0.1965759, and the maximum fitted 
# value is 0.7463568 These values represent the range of probabilities for the 
# dependent variable (i.e., the probability of saying that global warming is 
# real and caused by humans) based on the values of the independent variables 
# included in the model. These values suggest that there is a wide range of 
# probabilities of support for human-caused climate change among the individuals 
# in the dataset, with some having very low probabilities and others having very
# high probabilities. But, because the probability cannot technically be less
# than 0 we can treat the minimum value (which is less than 0) as 0.

# Get predicted probabilities for each observation
probs <- predict(lpm, type = "response")

# Calculate minimum and maximum fitted values
min_fitted <- min(probs)
max_fitted <- max(probs)

min_fitted
max_fitted

# -------------------------------------------------------------------------------
# 1.(b)(i) To compare the statistical significance of the independent variables in 
# the probit model with those in the LPM model, we can look at the p-values for 
# each coefficient. In the LPM model, the variables "male," "white," 
# "incomecat," and "agesq" are not statistically significant at the 0.05 level, 
# while the variables "educ" and "party7" are statistically significant at the 
# 0.001 level. In the probit model, the variables "male," "white," and 
# "incomecat" are not statistically significant at the 0.05 level, while the 
# variables "educ," "age," "agesq," and "party7" are statistically significant 
# at the 0.05 level or better. Overall, the results suggest that the probit 
# model may be a better fit for this particular dataset, as it includes 
# additional statistically significant variables compared to the LPM model.

# Fit a probit model
probit <- glm(humancause ~ male + white + educ + incomecat + age + agesq + party7,
                          data = survey_data, family = binomial(link = "probit"))

# Print the summary of the model
summary(probit)

# -------------------------------------------------------------------------------
# 1.(b)(ii) The minimum fitted value from this model is 0.021, which represents the 
# lowest predicted probability of the outcome of interest (in this case, support
# for human-caused climate change). The maximum fitted value is 0.797, which 
# represents the highest predicted probability of the outcome of interest. 
# These values suggest that there is a wide range of probabilities of support 
# for human-caused climate change among the individuals in the dataset, with 
# some having very low probabilities and others having very high probabilities.

# Get predicted probabilities for each observation
probs <- predict(probit, type = "response")

# Calculate minimum and maximum fitted values
min_fitted <- min(probs)
max_fitted <- max(probs)

min_fitted
max_fitted

# -------------------------------------------------------------------------------
# 1.(b)(iii) Comparing the three approaches the the effect of an increase of one 
# unit on the seven point scale we have as follows:
# LPM: 0.0867771
# Probit: 0.08673408
# Marginal Effects: 8.3421e-02 or 0.083421
# As we can see, the difference in these three approaches are minuscule which
# makes sense as all of the approaches should have similar results because regardless
# of the way in which the effect is calculated, the value should be almost the
# same.

#LPM model
# coefficient for party7 = 0.0867771

#probit model
male <- survey_data$male
white <- survey_data$white
educ <- survey_data$educ
incomecat <- survey_data$incomecat
age <- survey_data$age
agesq <- survey_data$agesq
party7 <- survey_data$party7

P1_party7 <- pnorm(probit$coef[1] + probit$coef[2]*male + probit$coef[3]*white + probit$coef[4]*educ + probit$coef[5]*incomecat + probit$coef[6]*age + probit$coef[7]*agesq + probit$coef[8]*party7)

party7_plus <- party7 + 1

P2_party7 <- pnorm(probit$coef[1] + probit$coef[2]*male + probit$coef[3]*white + probit$coef[4]*educ + probit$coef[5]*incomecat + probit$coef[6]*age + probit$coef[7]*agesq + probit$coef[8]*party7_plus)

mean(P2_party7-P1_party7, na.rm=TRUE)

# marginal effects
library(mfx)
probitmfx(probit, data = survey_data, atmean=FALSE)

# -------------------------------------------------------------------------------
# 1.(b)(iv) Comparing the three approaches the the effect of being a male on the 
# probability of saying global warming is real and caused by humans we have as follows:
# LPM: 0.0206509
# Probit: 0.01976387
# Marginal Effects: 1.9757e-02 or 0.019757
# As we can see, the difference in these three approaches are minuscule which
# makes sense as all of the approaches should have similar results because regardless
# of the way in which the effect is calculated, the value should be almost the
# same.

#LPM model
# coefficient for male = 0.0206509

# probit
P0_male <- pnorm(probit$coef[1] + probit$coef[2]*0 + probit$coef[3]*white + probit$coef[4]*educ + probit$coef[5]*incomecat + probit$coef[6]*age + probit$coef[7]*agesq + probit$coef[8]*party7)

P1_male <- pnorm(probit$coef[1] + probit$coef[2]*1 + probit$coef[3]*white + probit$coef[4]*educ + probit$coef[5]*incomecat + probit$coef[6]*age + probit$coef[7]*agesq + probit$coef[8]*party7)

mean(P1_male-P0_male, na.rm=TRUE)

# marginal effects
library(mfx)
probitmfx(probit, data = survey_data, atmean=FALSE)

# -------------------------------------------------------------------------------
# QUESTION 2
# 2(party7) Comparing the two approaches the effect of an increase of one 
# unit on the seven point scale we have as follows:
# Probit: 0.08673408
# Logit: 0.08672648
# Marginal Effects Probit: 8.3421e-02 or 0.083421
# Marginal Effects Logit: 8.3421e-02 or 0.083421
# As we can see, the difference in these approaches are minuscule or not different at all
# which makes sense as all of the approaches should have similar results because regardless
# of the way in which the effect is calculated, the value should be almost the
# same. It is important to note that even though the coefficients for the probit
# and logit models are very different, the effects are essentially the same.

# Fit a logit model
logit <- glm(humancause ~ male + white + educ + incomecat + age + agesq + party7,
              data = survey_data, family = binomial(link = "logit"))

# Print the summary of the model
summary(logit)

# calculate difference
party7_plus <- party7 + 1

num_1 <- exp(-3.6605596 + 0.1139617*male + 0.1871482*white + 0.1446450*educ + 0.0152539*incomecat -0.0483237*age + 0.0004373*agesq + 0.4431849*party7)
den_1 <- 1 + exp(-3.6605596 + 0.1139617*male + 0.1871482*white + 0.1446450*educ + 0.0152539*incomecat -0.0483237*age + 0.0004373*agesq + 0.4431849*party7)
val_1 <- num_1 / den_1

num_2 <- exp(-3.6605596 + 0.1139617*male + 0.1871482*white + 0.1446450*educ + 0.0152539*incomecat -0.0483237*age + 0.0004373*agesq + 0.4431849*party7_plus)
den_2 <- 1 + exp(-3.6605596 + 0.1139617*male + 0.1871482*white + 0.1446450*educ + 0.0152539*incomecat -0.0483237*age + 0.0004373*agesq + 0.4431849*party7_plus)
val_2 <- num_2 / den_2

mean(val_2 - val_1)

# marginal effects
library(mfx)
probitmfx(logit, data = survey_data, atmean=FALSE)

# -------------------------------------------------------------------------------
# QUESTION 2
# 2(male) Comparing the two approaches the effect of being a male on the 
# probability of saying global warming is real and caused by humans we have as follows:
# Probit: 0.01976387
# Logit: 0.02135673
# Marginal Effects Probit: 1.9757e-02 or 0.019757
# Marginal Effects Logit: 1.9757e-02 or 0.019757
# As we can see, the difference in these approaches are minuscule (but greater than the effect of party7) 
# or not different at all which makes sense as all of the approaches should have 
# similar results because regardless of the way in which the effect is calculated, 
# the value should be almost the same. It is important to note that even though 
# the coefficients for the probit and logit models are very different, the effects 
# are essentially the same.

num_1_m <- exp(-3.6605596 + 0.1139617*0 + 0.1871482*white + 0.1446450*educ + 0.0152539*incomecat -0.0483237*age + 0.0004373*agesq + 0.4431849*party7)
den_1_m <- 1 + exp(-3.6605596 + 0.1139617*0 + 0.1871482*white + 0.1446450*educ + 0.0152539*incomecat -0.0483237*age + 0.0004373*agesq + 0.4431849*party7)
val_1_m <- num_1_m / den_1_m

num_2_m <- exp(-3.6605596 + 0.1139617*1 + 0.1871482*white + 0.1446450*educ + 0.0152539*incomecat -0.0483237*age + 0.0004373*agesq + 0.4431849*party7)
den_2_m <- 1 + exp(-3.6605596 + 0.1139617*1 + 0.1871482*white + 0.1446450*educ + 0.0152539*incomecat -0.0483237*age + 0.0004373*agesq + 0.4431849*party7)
val_2_m <- num_2_m / den_2_m

mean(val_2_m - val_1_m)

# marginal effects
library(mfx)
probitmfx(logit, data = survey_data, atmean=FALSE)

# -------------------------------------------------------------------------------
# QUESTION 3

# read the .dta file
uk_climate_data <- read_dta("ukclimate(1).dta")

# -------------------------------------------------------------------------------
# 3.1 Is overall belief in climate change higher or lower in the UK or US? 
# The overall belief in climate change is much higher in the UK than the US. The
# 56% of people in the UK believe in climate change while only 33% in the US believe
# (based on the respective samples taken)
mean_humancause <-  sum(survey_data$humancause, na.rm = TRUE) / nrow(survey_data)
mean_cchuman <- sum(uk_climate_data$cchuman, na.rm = TRUE) / nrow(uk_climate_data)

mean_humancause
mean_cchuman

# -------------------------------------------------------------------------------
# 3.2 Repeat the exercise from Q1 and 2 using this dataset, estimating an LPM, a probit, and a logit model. 






# -------------------------------------------------------------------------------
# 3.2.(a)(i) Looking at the estimated coefficients, the variable that appears to have 
# the most important influence on the opinion that global warming is real and 
# caused by humans is partisan identification (lrscale). This is indicated by the 
# relatively large coefficient (compared to the others) of -0.0527507 and the very low p-value (1.67e-14), 
# which suggests strong statistical evidence that the variable is associated 
# with the outcome.

# Estimate logistic regression model
lpm_uk <- glm(cchuman ~ male + white + edu_deg + hinctnta + age + lrscale, data = uk_climate_data)

# Print summary of regression results
summary(lpm_uk)

# -------------------------------------------------------------------------------
# 3.2.(a)(ii) The minimum fitted value from the model is 0.1889625, and the maximum fitted 
# value is 0.9620034 These values represent the range of probabilities for the 
# dependent variable (i.e., the probability of saying that global warming is 
# real and caused by humans) based on the values of the independent variables 
# included in the model. These values suggest that there is a wide range (but not as wide as the US data) of 
# probabilities of support for human-caused climate change among the individuals 
# in the dataset, with some having very low probabilities and others having very
# high probabilities.

# Get predicted probabilities for each observation
probs_uk <- predict(lpm_uk, type = "response")

# Calculate minimum and maximum fitted values
min_fitted_uk <- min(probs_uk)
max_fitted_uk <- max(probs_uk)

min_fitted_uk
max_fitted_uk

# -------------------------------------------------------------------------------
# 3.2.(b)(i) To compare the statistical significance of the independent variables in 
# the probit model with those in the LPM model, we can look at the p-values for 
# each coefficient. In the LPM model, the variables "male," "white," and
# "hinctnta" are not statistically significant at the 0.05 level, 
# while the variables "edu_deg," "age," and "lrscale" are statistically significant at the 
# 0.05 level. In the probit model, the same independent variables are significant and not significant. 
# Overall, neither model seems to be signifantly better than the other.

# Fit a probit model
probit_uk <- glm(cchuman ~ male + white + edu_deg + hinctnta + age + lrscale, data = uk_climate_data, family = binomial(link = "probit"))

# Print the summary of the model
summary(probit_uk)

# -------------------------------------------------------------------------------
# 3.2.(b)(ii) The minimum fitted value from this model is 0.1961146, which represents the 
# lowest predicted probability of the outcome of interest (in this case, support
# for human-caused climate change). The maximum fitted value is 0.8971762, which 
# represents the highest predicted probability of the outcome of interest. 
# These values suggest that there is a wide range of probabilities of support 
# for human-caused climate change among the individuals in the dataset 
# (although less of a range than the US probit model), with 
# some having very low probabilities and others having very high probabilities.

# Get predicted probabilities for each observation
probs_uk <- predict(probit_uk, type = "response")

# Calculate minimum and maximum fitted values
min_fitted_uk <- min(probs_uk)
max_fitted_uk <- max(probs_uk)

min_fitted_uk
max_fitted_uk

# -------------------------------------------------------------------------------
# 3.2.(b)(iii) Comparing the three approaches the the effect of an increase of one 
# unit on the seven point scale we have as follows:
# LPM: -0.0527507
# Probit: 6.492935e-05 or 0.0000649
# Marginal Effects: -0.05353380
# As we can see, the difference in these three approaches are minuscule but there is a
# clear difference in the probit model. Although the probit model is basically 0,
# the LPM model and the marginal effects approach are negative. With that being said
# the difference between 0 and -0.05 is very small.

#LPM model
# coefficient for lrscale = -0.0527507

#probit model
male <- uk_climate_data$male
white <- uk_climate_data$white
edu_deg <- uk_climate_data$edu_deg
hinctnta <- uk_climate_data$hinctnta
age <- uk_climate_data$age
lrscale <- uk_climate_data$lrscale

P1_lrscale <- pnorm(probit_uk$coef[1] + probit_uk$coef[2]*male + probit_uk$coef[3]*white + probit_uk$coef[4]*edu_deg + probit_uk$coef[5]*hinctnta + probit_uk$coef[6]*age + probit$coef[7]*lrscale)

lrscale_plus <- lrscale + 1

P2_lrscale <- pnorm(probit_uk$coef[1] + probit_uk$coef[2]*male + probit_uk$coef[3]*white + probit_uk$coef[4]*edu_deg + probit_uk$coef[5]*hinctnta + probit_uk$coef[6]*age + probit$coef[7]*lrscale_plus)

mean(P2_lrscale - P1_lrscale, na.rm=TRUE)

# marginal effects
library(mfx)
probitmfx(probit_uk, data = uk_climate_data, atmean=FALSE)

# -------------------------------------------------------------------------------
# 3.2.(b)(iv) Comparing the three approaches the the effect of being a male on the 
# probability of saying global warming is real and caused by humans we have as follows:
# LPM: 0.0175316
# Probit: 0.01224184
# Marginal Effects: 0.01783862
# As we can see, the difference in these three approaches are minuscule which
# makes sense as all of the approaches should have similar results because regardless
# of the way in which the effect is calculated, the value should be almost the
# same.

#LPM model
# coefficient for male = 0.0175316

# probit
P0_male_uk <- pnorm(probit_uk$coef[1] + probit_uk$coef[2]*0 + probit_uk$coef[3]*white + probit_uk$coef[4]*edu_deg + probit_uk$coef[5]*hinctnta + probit_uk$coef[6]*age + probit$coef[7]*lrscale)

P1_male_uk <- pnorm(probit_uk$coef[1] + probit_uk$coef[2]*1 + probit_uk$coef[3]*white + probit_uk$coef[4]*edu_deg + probit_uk$coef[5]*hinctnta + probit_uk$coef[6]*age + probit$coef[7]*lrscale)

mean(P1_male_uk - P0_male_uk, na.rm=TRUE)

# marginal effects
library(mfx)
probitmfx(probit_uk, data = uk_climate_data, atmean=FALSE)

# -------------------------------------------------------------------------------
# 3.2.2(lrscale) Comparing the two approaches the effect of an increase of one 
# unit on the seven point scale we have as follows:
# Probit: 6.492935e-05 or 0.0000649
# Logit: -0.0544852
# Marginal Effects Probit: -0.05353380
# Marginal Effects Logit: -0.05353380
# As we can see, the difference in these approaches are minuscule or not different at all
# which makes sense as all of the approaches should have similar results because regardless
# of the way in which the effect is calculated, the value should be almost the
# same. It is important to note that even though the coefficients for the probit
# and logit models are very different, the effects are essentially the same.

# Fit a logit model
logit_uk <- glm(cchuman ~ male + white + edu_deg + hinctnta + age + lrscale, data = uk_climate_data, family = binomial(link = "logit"))

# Print the summary of the model
summary(logit_uk)

# calculate difference
lrscale_plus <- lrscale + 1

num_1 <- exp(1.299080 + 0.081391*male + 0.030560*white + 0.190558*edu_deg + 0.026656*hinctnta -0.006932*age + -0.237155*lrscale)
den_1 <- 1 + exp(1.299080 + 0.081391*male + 0.030560*white + 0.190558*edu_deg + 0.026656*hinctnta -0.006932*age + -0.237155*lrscale)
val_1 <- num_1 / den_1

num_2 <- exp(1.299080 + 0.081391*male + 0.030560*white + 0.190558*edu_deg + 0.026656*hinctnta -0.006932*age + -0.237155*lrscale_plus)
den_2 <- 1 + exp(1.299080 + 0.081391*male + 0.030560*white + 0.190558*edu_deg + 0.026656*hinctnta -0.006932*age + -0.237155*lrscale_plus)
val_2 <- num_2 / den_2

mean(val_2 - val_1, na.rm=TRUE)

# marginal effects
library(mfx)
probitmfx(logit_uk, data = uk_climate_data, atmean=FALSE)

# -------------------------------------------------------------------------------
# 3.2.2(male) Comparing the two approaches the effect of being a male on the 
# probability of saying global warming is real and caused by humans we have as follows:
# Probit: 0.01224184
# Logit: 0.01840566
# Marginal Effects Probit: 0.01783862
# Marginal Effects Logit: 0.01783862
# As we can see, the difference in these approaches are minuscule (but greater than the effect of party7) 
# or not different at all which makes sense as all of the approaches should have 
# similar results because regardless of the way in which the effect is calculated, 
# the value should be almost the same. It is important to note that even though 
# the coefficients for the probit and logit models are very different, the effects 
# are essentially the same.

num_1 <- exp(1.299080 + 0.081391*0 + 0.030560*white + 0.190558*edu_deg + 0.026656*hinctnta -0.006932*age + -0.237155*lrscale)
den_1 <- 1 + exp(1.299080 + 0.081391*0 + 0.030560*white + 0.190558*edu_deg + 0.026656*hinctnta -0.006932*age + -0.237155*lrscale)
val_1 <- num_1 / den_1

num_2 <- exp(1.299080 + 0.081391*1 + 0.030560*white + 0.190558*edu_deg + 0.026656*hinctnta -0.006932*age + -0.237155*lrscale)
den_2 <- 1 + exp(1.299080 + 0.081391*1 + 0.030560*white + 0.190558*edu_deg + 0.026656*hinctnta -0.006932*age + -0.237155*lrscale)
val_2 <- num_2 / den_2

mean(val_2 - val_1, na.rm=TRUE)

# marginal effects
library(mfx)
probitmfx(logit_uk, data = uk_climate_data, atmean=FALSE)

