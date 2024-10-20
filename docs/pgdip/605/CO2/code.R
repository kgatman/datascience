setwd("C:\\Users\\Makhate\\deepdives\\datascience\\docs\\pgdip\\605\\CO2\\info")
getwd()
CO2data <- read.csv("CO2EmissionsData.csv")

str(CO2data)
summary(CO2data)

#####Cleaning#######
clean_CO2data <- na.omit(CO2data)
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

#### Reducing Many Factors ####
deduplicated_CO2$Vehicle.Class[deduplicated_CO2$Vehicle.Class %in% c('SUV - SMALL', 'SUV - STANDARD', 'MINIVAN')] <- 'SUV'
deduplicated_CO2$Vehicle.Class[deduplicated_CO2$Vehicle.Class %in% c('MID-SIZE', 'TWO-SEATER', 'FULL-SIZE', 'STATION WAGON - SMALL', 'STATION WAGON - MID-SIZE')] <- 'Sedan'
deduplicated_CO2$Vehicle.Class[deduplicated_CO2$Vehicle.Class %in% c('VAN - CARGO', 'VAN - PASSENGER', 'PICKUP TRUCK - STANDARD', 'SPECIAL PURPOSE VEHICLE', 'PICKUP TRUCK - SMALL')] <- 'Bakkie'
deduplicated_CO2$Vehicle.Class[deduplicated_CO2$Vehicle.Class %in% c('COMPACT', 'MINICOMPACT', 'SUBCOMPACT')] <- 'Town_Runners'

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
       aes(x =Fuel.Type , y = CO2.Emissions.g.km., color=Fuel.Type)) +
  geom_boxplot() +
  labs(title = "CO2 Emissions By Fuel Type",
       x = "Fuel Type",
       y = "CO2 Emissions")

##########Emissions By Cylinders############
ggplot(data=deduplicated_CO2, 
       aes(x = Cylinders, y = CO2.Emissions.g.km., color=Cylinders)) +
  geom_point() +
  labs(title = "CO2 Emissions By Cylinders",
       x = "Cylinders",
       y = "CO2 Emissions")

##########Emissions By Vehicle TYpe############
ggplot(data=deduplicated_CO2, 
       aes(x = Vehicle.Class, y = CO2.Emissions.g.km., color=Vehicle.Class)) +
  geom_boxplot() +
  labs(title = "CO2 Emissions By Vehicle Type",
       x = "CO2 Emissions",
       y = "Vehicle Type")


##########Emissions By Fuel Consumption############
ggplot(data=deduplicated_CO2, 
       aes(x = CO2.Emissions.g.km., y = Fuel.Consumption.Comb..L.100.km., color=Fuel.Consumption.Comb..L.100.km.)) +
  geom_point() +
  labs(title = "CO2 Emissions By Fuel Consumption",
       x = "CO2 Emissions",
       y = "Consumption")
