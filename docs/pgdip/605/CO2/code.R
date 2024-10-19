setwd("C:\\Users\\Makhate\\deepdives\\datascience\\docs\\pgdip\\605\\CO2\\info")
getwd()
CO2data <- read.csv("CO2EmissionsData.csv")

str(CO2data)
summary(CO2data)

#####Cleaning#######
clean_CO2data <- na.omit(CO2data)
?na.omit()
####
# the clean dataframe now has 7354 entries/observations
# 31 Obs have been ommited
###

####Checking Categorical Variables#######
print(sapply(clean_CO2data,class))
########################################

#####setting Categorical Variables#######
factors_clean_CO2data <- clean_CO2data

factors_clean_CO2data$Make <- as.factor(factors_clean_CO2data$Make)
factors_clean_CO2data$Model <- as.factor(factors_clean_CO2data$Model)
factors_clean_CO2data$Vehicle.Class <- as.factor(factors_clean_CO2data$Vehicle.Class)
factors_clean_CO2data$Engine.Size.L. <- as.factor(factors_clean_CO2data$Engine.Size.L.)
factors_clean_CO2data$Cylinders <- as.factor(factors_clean_CO2data$Cylinders)
factors_clean_CO2data$Transmission <- as.factor(factors_clean_CO2data$Transmission)
factors_clean_CO2data$Fuel.Type <- as.factor(factors_clean_CO2data$Fuel.Type)

summary_factors_clean_CO2data <-as.data.frame(summary(factors_clean_CO2data))
##############################################################################

##############Deduplicating##################
library(dplyr)
deduplicated_CO2<-factors_clean_CO2data %>% distinct()
View(deduplicated_CO2)

##############Stats##################
stats_on_deduplicated_data <- as.data.frame(summary(deduplicated_CO2))
View(stats_on_deduplicated_data[,"Freq"])