#Importing the Data
Math.Students <- read.csv("~/STAT606_Assignment/Math-Students.csv", header=TRUE)
#Summary Statistics
summary(Math.Students)
####Target Variable
library(h2o)
library(ggplot2)
# Start H2O
h2o.init()
h20_MathStudents <- h20.importFile(Math.Students)

install.packages("ggplot2")
# Create box plot
library(ggplot2)
ggplot(Math.Students, aes(x = Age, y = G1)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Sepal Width by Species", x = "G3", y = "Age") +
  theme_minimal()
