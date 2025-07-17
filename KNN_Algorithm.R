#Normalisation

#Encoding the gender variable
data$gender
Gender_Encoded = (factor(data$gender , labels = c(0,1), levels = c("male","female")))
Gender_Encoded


nor <- function(x){ (x -min(x))/(max(x)-min(x)) }
data_nor <- as.data.frame(lapply(data[,-c(1,6,14)], nor))
data_nor

#adding the encoded column gender
data_nor["gender"] = Gender_Encoded
#Splitting the data
#set.seed(123)
random = sample(1:nrow(data),nrow(data)*0.9)
random
x_train = data_nor[random,]
x_test = data_nor[-random,]

y_train = data[random,14]
y_actual = data[-random,14]

nrow(x_train)
nrow(x_test)

#KNN algorithm
library(class)
model_knn = knn(x_train, x_test, k = 18, cl = y_train)
y_predicted_KNN = model_knn

confusion_matrix_KNN = table(y_actual , y_predicted_KNN)
confusion_matrix_KNN


accuracy_KNN = ((sum(diag(confusion_matrix_KNN))) / (sum(confusion_matrix_KNN)))*100
paste("Accuracy of K Nearest Neighbours classifier is : ", accuracy_KNN,"%")



#Decision Trees
library(rpart)
library(rpart.plot)
library(caret)

data = data[-1]
model_decision_tree = rpart(diabetes~. , data = data[random,], method = "class")
rpart.plot(model_decision_tree)
y_predicted_DecisionTree = predict(model_decision_tree, data[-random,], type = 'class')
y_predicted_DecisionTree
confusion_matrix_DecsionTree = table(y_actual, y_predicted_DecisionTree)
confusion_matrix_DecsionTree
accuracy_DecisionTree = ((sum(diag(confusion_matrix_DecsionTree))) / (sum(confusion_matrix_DecsionTree)))*100
paste("Accuracy of Decision Trees is : ", accuracy_DecisionTree,"%")



#Support Vector Machine (SVM)
library(e1071)
data$diabetes = factor(data$diabetes)
model_SVM=svm(formula=diabetes ~ .,data=data[random,],type='C-classification',kernel='polynomial', degree = 3)

ggplot(data=data[random,], aes(y=glucose,
                      x=cholesterol,col = diabetes))+geom_point()

y_predicted_SVM = predict(model_SVM, data[-random,], type = 'class')
y_predicted_SVM
confusion_matrix_SVM = table(y_actual, y_predicted_SVM)
confusion_matrix_SVM
accuracy_SVM = ((sum(diag(confusion_matrix_SVM))) / (sum(confusion_matrix_SVM)))*100
paste("Accuracy of Support Vector Machine is : ", accuracy_SVM,"%")



#Artificial Neural Networks (ANN)
str(data)
library("neuralnet")
data_ANN = data
Gender_Encoded1 = as.integer(factor(data$gender , labels = c(0,1), levels = c('male','female')))
typeof(Gender_Encoded1)
data_ANN$gender = Gender_Encoded1
model_ANN  <- neuralnet(
    formula = diabetes~.,
    data = data_ANN[random,],
    hidden = 5,
    threshold = 0.01,
    linear.output = F  
  )

y_predicted_ANN = predict(model_ANN, data_ANN[-random,])
y_predicted_ANN_classes <- ifelse(y_predicted_ANN[,1] > 0.5, 'Diabetes', 'No diabetes')
class(y_predicted_ANN_classes)
plot(model_ANN)
confusion_matrix_ANN = table(y_actual, y_predicted_ANN_classes)
confusion_matrix_ANN
accuracy_ANN = ((sum(diag(confusion_matrix_ANN))) / (sum(confusion_matrix_ANN)))*100
paste("Accuracy of ANN is : ", accuracy_ANN,"%")



#Random Forest
install.packages("randomForest")
library(randomForest)
model_Random_Forest = randomForest(diabetes ~ .,data=data[random,],ntree=100)
y_predicted_Random_Forest = predict(model_Random_Forest, data[-random,], type = 'class')
y_predicted_Random_Forest
confusion_matrix_Random_Forest = table(y_actual, y_predicted_Random_Forest)
confusion_matrix_Random_Forest
accuracy_Random_Forest = ((sum(diag(confusion_matrix_Random_Forest))) / (sum(confusion_matrix_Random_Forest)))*100
paste("Accuracy of Random_Forest is : ", accuracy_Random_Forest,"%")
varImpPlot(model_Random_Forest,
           main = "random Forest",col=c("lightblue","lightgreen"),
           pch=16,cex=1.5)




#Model Accuracy Comparison
model_accuracies<-data.frame(Model=c("KNN","Decision Tree","SVM","ANN",
                                     "Random Forest"),
                             Accuracy=c(accuracy_KNN,accuracy_DecisionTree,
                            accuracy_SVM,accuracy_ANN,accuracy_Random_Forest))
model_accuracies
barplot(model_accuracies$Accuracy,names.arg=model_accuracies$Model,
        col="lightblue",main="Model Accuracy Comparison",xlab="Model",
        ylab="Accuracy (%)",ylim=c(0,100))
text(x=seq_along(model_accuracies$Model),y=model_accuracies$Accuracy,
     labels=round(model_accuracies$Accuracy,1),pos=3,cex=0.8,col="black")




