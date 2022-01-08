library(ggplot2) # allows charts drawing and data visualization
library(dplyr) #data manipulation - deleting columns - selecting specific columns
library(broom) # tides up the messy outputs of builtin functions
library(ggpubr)
library(caTools)

dataset = read.csv('G:\\gam3a\\Advanced Statistics\\Project\\income.data.csv') #the dataset

sum(is.na(dataset)) #  checking if there is null values in the dataset
summary(dataset) #summary about the data ,, min/ max / mean/ median ..etc

dataset <- na.omit(dataset)

split = sample.split(dataset$happiness, SplitRatio = 0.8) # split into test and train 
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
training_set = scale(training_set) #scalling the values for accurecy
test_set = scale(test_set)


plot(happiness ~ income, data = dataset) # checking linearity  

#To perform a simple linear regression analysis and check the results
income.happiness.lm <- lm(happiness ~ income, data = dataset)#fitting values in linear model
summary(income.happiness.lm)
#

par(mfrow=c(2,2))
plot(income.happiness.lm)## plotting residuals vs fitted values to check error and make sure its countable // trustworthy result
par(mfrow=c(1,1))


income.graph<-ggplot(dataset, aes(x=income, y=happiness))+
  geom_point()#plot the data and the regression line from our linear regression model

income.graph <- income.graph + geom_smooth(method="lm", col="black")#add the line of the linear regression as well as the standard error


income.graph <- income.graph +
  stat_regline_equation(label.x = 3, label.y = 7)#the equation for the regression line.



income.graph <- income.graph +# finished graph
  theme_bw() +
  labs(title = "Reported happiness as a function of income",
       x = "Income (x$10,000)",
       y = "Happiness score (0 to 10)")

plot(income.graph)