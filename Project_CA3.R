data = read.csv(file.choose())

#Exploratory data analysis
View(data)
summary(data)

#removing two redundant columns (5,15)
data = data[,-c(5,15)]

#cleaning the bmi column
data$bmi = as.numeric(gsub(",",".", data$bmi))
#There are no null values

ncol(data)
nrow(data)

library(ggplot2)
ggplot(data , aes(x = diabetes))+geom_bar(fill = c('red','green'))+
  labs(title = "Frequency distribution of the diagnosis", x = 'diabates',y = 'count')+
  geom_text(stat = 'count', aes(label = ..count..),  vjust = -0.5)



#frequency distribution of cholesterol column normal dist
ggplot(data, aes(x = cholesterol)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Values",
       x = "Value",
       y = "Frequency")+labs(title = "Frequency distribution of Cholesterol")

#frequency distribution of glucose column left skewed
ggplot(data, aes(x = glucose)) +
  geom_histogram(binwidth = 5, fill = "green", color = "black") +
  labs(title = "Histogram of Values",
       x = "Value",
       y = "Frequency")+labs(title = "Frequency distribution of Glucose")


#frequency distribution of hdl_chol column left skewed
ggplot(data, aes(x = hdl_chol)) +
  geom_histogram(binwidth = 5, fill = "pink", color = "black") +
  labs(title = "Histogram of Values",
       x = "Value",
       y = "Frequency")+labs(title = "Frequency distribution of hdl_chol")


#frequency distribution of age column unifrorm
ggplot(data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "red", color = "black") +
  labs(title = "Histogram of Values",
       x = "Value",
       y = "Frequency")+labs(title = "Frequency distribution of age")

#frequency distribution of gender column unifrorm
ggplot(data , aes(x = gender))+geom_bar(fill = c('red','green'))+
  labs(title = "Frequency distribution of the gender", x = 'gender',y = 'count')+
  geom_text(stat = 'count', aes(label = ..count..),  vjust = -0.5)


#frequency distribution of height column normal
ggplot(data, aes(x = height)) +
  geom_histogram(binwidth = 5, fill = "orange", color = "black") +
  labs(title = "Histogram of Values",
       x = "Value",
       y = "Frequency")+labs(title = "Frequency distribution of height")


#frequency distribution of weight column normal
ggplot(data, aes(x = weight)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "black") +
  labs(title = "Histogram of Values",
       x = "Value",
       y = "Frequency")+labs(title = "Frequency distribution of weight")

#frequency distribution of bmi column left skewed
ggplot(data, aes(x = bmi)) +
  geom_histogram(binwidth = 5, fill = "brown", color = "black") +
  labs(title = "Histogram of Values",
       x = "Value",
       y = "Frequency")+labs(title = "Frequency distribution of bmi")


#frequency distribution of systolic_bp column left skewed
ggplot(data, aes(x = systolic_bp)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Histogram of Values",
       x = "Value",
       y = "Frequency")+labs(title = "Frequency distribution of systolic_bp")


#frequency distribution of diastolic_bp column normal
ggplot(data, aes(x = diastolic_bp)) +
  geom_histogram(binwidth = 5, fill = "yellow", color = "black") +
  labs(title = "Histogram of Values",
       x = "Value",
       y = "Frequency")+labs(title = "Frequency distribution of diastolic_bp")


#frequency distribution of waist column normal
ggplot(data, aes(x = waist)) +
  geom_histogram(binwidth = 5, fill = "maroon", color = "black") +
  labs(title = "Histogram of Values",
       x = "Value",
       y = "Frequency")+labs(title = "Frequency distribution of waist")


#frequency distribution of hip column left skewed
ggplot(data, aes(x = hip)) +
  geom_histogram(binwidth = 5, fill = "cyan", color = "black") +
  labs(title = "Histogram of Values",
       x = "Value",
       y = "Frequency")+labs(title = "Frequency distribution of hip")


#visualising the outliers using boxplot
ggplot(data , aes( y = cholesterol)) + geom_boxplot(outlier.colour = 'red', outlier.size = 5)
ggplot(data , aes( y = glucose)) + geom_boxplot(outlier.colour = 'blue', outlier.size = 5)
ggplot(data , aes( y = hdl_chol)) + geom_boxplot(outlier.colour = 'green', outlier.size = 5)
ggplot(data , aes( y = age)) + geom_boxplot(outlier.colour = 'yellow', outlier.size = 5)
ggplot(data , aes( y = height)) + geom_boxplot(outlier.colour = 'pink', outlier.size = 5)
ggplot(data , aes( y = weight)) + geom_boxplot(outlier.colour = 'maroon', outlier.size = 5)
ggplot(data , aes( y = bmi)) + geom_boxplot(outlier.colour = 'lightblue', outlier.size = 5)
ggplot(data , aes( y = systolic_bp)) + geom_boxplot(outlier.colour = 'brown', outlier.size = 5)
ggplot(data , aes( y = diastolic_bp)) + geom_boxplot(outlier.colour = 'cyan', outlier.size = 5)
ggplot(data , aes( y = waist)) + geom_boxplot(outlier.colour = 'purple', outlier.size = 5)
ggplot(data , aes( y = hip)) + geom_boxplot(outlier.colour = 'orange', outlier.size = 5)

