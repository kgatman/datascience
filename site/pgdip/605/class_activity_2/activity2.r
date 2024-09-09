getwd()
setwd("C:\\Users\\Makhate\\deepdives\\datascience\\docs\\pgdip")read.csv()

#reading data
housing_price <- read.csv("605\\class_activity_2\\ParisHousingPriceData.csv")

#a
is.data.frame(housing_price) #TRUE

#b
str(housing_price)
summary(housing_price)

#c
clean_housing_price <- na.omit(housing_price)
#new dataset now has 9903 from 10K obs

#d
class(clean_housing_price$SQM)
class(clean_housing_price$Bedrooms)
#tocheck the class of ALL variables
print(sapply(clean_housing_price,class))

factors_clean_housing_price <- data.frame(
  clean_housing_price$SQM,
  clean_housing_price$Bedrooms,
  clean_housing_price$HasYard,
  clean_housing_price$HasPool,
  clean_housing_price$Floors,
  clean_housing_price$SuburbType,
  clean_housing_price$NumPrevOwners,
  clean_housing_price$Made,
  clean_housing_price$HasStormProtector,
  clean_housing_price$Basement,
  clean_housing_price$Attic,
  clean_housing_price$Garages,
  clean_housing_price$HasStorageRoom,
  clean_housing_price$LivingRooms,
  clean_housing_price$Price,
  stringsAsFactors = T
)
str(factors_clean_housing_price)
