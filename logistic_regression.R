# Read in libraries
library(caret)
library(aod)
library(ggplot2)
library(pscl)

# Read in datasource
mydata <- read.csv("transmission_df.csv")
head(mydata)

# summary statistics for temperature
summary(mydata$temperature_c_index)

# summary statistics for age contact_pred
summary(mydata$age_contact_pred)

# summary statistics for age contact_pred
summary(mydata$age_index)



# create boxplots to identify possible confounders
boxplot(age_index~secondary_transmission_case, data=mydata, ylab ="age_index", xlab = "secondary transmission")
boxplot(age_contact_pred~secondary_transmission_case, data=mydata, ylab ="age_contact_pred", xlab = "secondary transmission")
boxplot(household_size~secondary_transmission_case, data=mydata, ylab ="household_size", xlab = "secondary transmission")
boxplot(total_household_adults~secondary_transmission_case, data=mydata, ylab ="total_household_adults", xlab = "secondary transmission")
boxplot(total_household_children~secondary_transmission_case, data=mydata, ylab ="total_household_children", xlab = "secondary transmission")
boxplot(temperature_c_index~secondary_transmission_case, data=mydata, ylab ="temperature_c_index", xlab = "secondary transmission")
boxplot(agegroup_index~secondary_transmission_case, data=mydata, ylab ="age group index", xlab = "secondary transmission")
boxplot(state_encode~secondary_transmission_case, data=mydata, ylab ="state encoded", xlab = "secondary transmission")

# examine stratification for age prediction for secondary transmission/outcome
table(mydata$secondary_transmission_case[mydata$agegroup_contact_pred==1])/length(mydata$secondary_transmission_case[mydata$agegroup_contact_pred==1])
table(mydata$secondary_transmission_case[mydata$agegroup_contact_pred==2])/length(mydata$secondary_transmission_case[mydata$agegroup_contact_pred==2])
table(mydata$secondary_transmission_case[mydata$agegroup_contact_pred==3])/length(mydata$secondary_transmission_case[mydata$agegroup_contact_pred==3])

#calculate point-biserial correlation
cor.test(mydata$age_contact_pred, mydata$secondary_transmission_case)
cor.test(mydata$age_contact_pred, mydata$temperature_c_index)

#make this example reproducible
set.seed(1)

#creating indices to train and test
trainIndex <- createDataPartition(mydata$secondary_transmission_case,p=0.75,list=FALSE)
#splitting data into training/testing data using the trainIndex object
train <- mydata[trainIndex,] #training data (75% of data)

test <- mydata[-trainIndex,] #testing data (25% of data)

# raw model with just secondary transmission and temperature
raw_model <- glm(secondary_transmission_case ~ temperature_c_index, data = train, family = 'binomial'(link="logit"))
summary(raw_model)

exp(.13611)

exp(summary(raw_model)$coefficients["temperature_c_index",1] + 
      qnorm(c(0.025,0.5,0.975)) * summary(mylogit)$coefficients["temperature_c_index",2])

#full unadjusted model with all variables included
unadjustedModel <- glm(secondary_transmission_case ~ household_size + total_household_children + total_household_adults + age_index + age_contact_pred + week_num + state_encode + temperature_c_index, data = train, family = 'binomial'(link="logit"))
summary(unadjustedModel)

# 
pscl::pR2(unadjustedModel)["McFadden"]
caret::varImp(unadjustedModel)
exp(.1776)

# get 95% confidence interval using qnorm
exp(summary(unadjustedModel)$coefficients["temperature_c_index",1] + 
          qnorm(c(0.025,0.5,0.975)) * summary(mylogit)$coefficients["temperature_c_index",2])

# 
age_model <- glm(secondary_transmission_case ~ age_index + temperature_c_index, data = train, family = 'binomial'(link="logit"))
summary(age_model)
exp(.1415793)

exp(summary(age_model)$coefficients["temperature_c_index",1] + 
      qnorm(c(0.025,0.5,0.975)) * summary(age_model)$coefficients["temperature_c_index",2])

# model with possible confounder introduced age_contact_pred
age_contact_model <- glm(secondary_transmission_case ~ age_contact_pred + temperature_c_index, data = train, family = 'binomial'(link="logit"))
summary(age_contact_model)
exp(.1726260)

exp(summary(age_contact_model)$coefficients["temperature_c_index",1] + 
      qnorm(c(0.025,0.5,0.975)) * summary(age_contact_model)$coefficients["temperature_c_index",2])

ggplot(mydata, aes(x=temperature_c_index, y=secondary_transmission_case)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial))
