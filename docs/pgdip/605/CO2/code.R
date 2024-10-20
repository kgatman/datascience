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
install.packages('writexl')
library('writexl')
write_xlsx(stats_on_deduplicated_data,"stats_on_deduplicated_data.xlsx")
########################################

##########Visualising The Data################
library(ggplot2)
##########Emissions By Engine Size############
ggplot(data=deduplicated_CO2, 
       aes(x = CO2.Emissions.g.km., y = Engine.Size.L., color=Engine.Size.L.)) +
  geom_point() +
  labs(title = "CO2 Emissions By Engine Size",
       x = "CO2 Emissions",
       y = "Engine Size")

##########Emissions By Fuel Type############
ggplot(data=deduplicated_CO2, 
       aes(x = CO2.Emissions.g.km., y = Fuel.Type, color=Fuel.Type)) +
  geom_point() +
  labs(title = "CO2 Emissions By Fuel Type",
       x = "CO2 Emissions",
       y = "Fuel Type")

##########Emissions By Cylinders############
ggplot(data=deduplicated_CO2, 
       aes(x = CO2.Emissions.g.km., y = Cylinders, color=Cylinders)) +
  geom_point() +
  labs(title = "CO2 Emissions By Cylinders",
       x = "CO2 Emissions",
       y = "Cylinders")

##########Emissions By Transmission############
ggplot(data=deduplicated_CO2, 
       aes(x = CO2.Emissions.g.km., y = Transmission, color=Transmission)) +
  geom_point() +
  labs(title = "CO2 Emissions By Transmission",
       x = "CO2 Emissions",
       y = "Transmission")

##########Emissions By Fuel Consumption############
ggplot(data=deduplicated_CO2, 
       aes(x = CO2.Emissions.g.km., y = Fuel.Consumption.Comb..L.100.km., color=Fuel.Consumption.Comb..L.100.km.)) +
  geom_point() +
  labs(title = "CO2 Emissions By Fuel Consumption",
       x = "CO2 Emissions",
       y = "Consumption")
