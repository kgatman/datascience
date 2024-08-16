#Data
revenue <- c(14574.49, 7606.46, 8611.41, 9175.41, 8058.65, 8105.44, 11496.28, 9766.09, 10305.32, 14379.96, 10713.97, 15433.50)
expenses <- c(12051.82, 5695.07, 12319.20, 12089.72, 8658.57, 840.20, 3285.73, 5821.12, 6976.93, 16618.61, 10054.37, 3803.96)

# round()
# mean()
# max()
# min()

profit <- revenue - expenses

profit_after_tax <- profit - profit*0.3

profit_margin_for_each_month <- profit_after_tax/revenue

bad_months <- profit_after_tax > mean(revenue)

good_months <- profit_after_tax < mean(revenue)

best_months <- profit_after_tax == max(profit)

#matrices
n<-10
a <- rep("a",n)
b <- rep("b",n)
c <- rep("c",n)
mymatrix <- rbind(a,b,c)
?rep()
matrix2 <- cbind(a,b,c)
#selecting the file manually
lipalo <- read.csv(file.choose())
#another method
getwd()
setwd("C:\\Users\\Makhate\\deepdives\\datascience")

#data exploration
#### nrow, ncol
### head() -  top 6 rows
### str(), summary()

str(lipalo)
rm(lipalo)

lipalo <- read.csv("refresheR\\P2-Demographic-Data.csv",stringsAsFactors = T)

lipalo$Internet.users[1]
lipalo[,1,drop=F] #to help main the object as a data frame and not a vector
is.data.frame(lipalo[,1,drop=F]) #yep, it's still a data frame

# behaves just like vectors, i love it. Makes life simple😍
lipalo$myNewColumn <- lipalo$Birth.rate + lipalo$Internet.users

#filtering data frames
filter_IUunder2 <- lipalo$Internet.users < 2
lipalo[filter,]
filter_BR0ver40 <- lipalo$Birth.rate > 40
liapalo_tse_ling <- lipalo[filter_BR0ver40 & filter_IUunder2,]
str(liapalo_tse_ling)
levels(lipalo$Income.Group) #checking levels of categorical data

#Malta
Malta <- lipalo$Country.Name == "Malta"
lipalo[Malta,]

#qplot
library("ggplot2")
?qplot
qplot(data = lipalo,x=Internet.users)
qplot(data = lipalo,x=Income.Group, y=Birth.rate, color=I("blue"), geom = "boxplot")
