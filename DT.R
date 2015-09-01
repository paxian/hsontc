


#########################################
# Recursive Paritiioning
#########################################
library(rpart)

#install.packages("rattle", repos="http://rattle.togaware.com", type="source") 
library(rattle)

#install.packages("rpart.plot")
library(rpart.plot)

#install.packages("RColorBrewer")
library(RColorBrewer)

my_tree <- rpart(Mark~Attempt+Semester_en+EarnedECP+FY+SY+TY+EX+EXT,
                 data=train)

#fancyRpartPlot(my_tree)
treePrediction <- predict(my_tree, test1)

tree_sol <- data.frame(StudentID=test1$StudentId, Pass=treePrediction)
#write.csv(tree_sol, file = "tree_sol.csv")
#########################################


#########################################
# Decision Trees
#########################################
#install.packages("party")
library("party")

dtree <- ctree(Mark~Attempt+Semester_en+EarnedECP+FY+SY+TY+EX+EXT,
                 data=train)

#plot(dtree)
dtree_Prediction <- predict(dtree, test1)

dtree_sol <- data.frame(StudentID=test1$StudentId, Pass=treePrediction)
#plot(dtree_sol)
#write.csv(dtree_sol, file = "dtree_sol.csv")
#########################################





#########################################
# Random Forest
#########################################

#install.packages("randomForest")
library(randomForest)

set.seed(111)

my_forest <- randomForest(as.factor(Mark)~Attempt+Semester_en+EarnedECP+FY+SY+TY+EX+EXT,
                          data=train, importance=TRUE)

#print(my_forest)
#importance(my_forest)
#plot(my_forest)
#plot( importance(my_forest), lty=2, pch=16)
#lines(importance(my_forest))

forest_prediction <- predict(my_forest, test1)

forest_solution <- data.frame(StudentID=test1$StudentId, Pass=forest_prediction)
#plot(forest_solution)
#write.csv(forest_solution, file = "forest_solution.csv")
#########################################


#########################################
# Using Naive Bayes
#########################################

library('e1071')

nB_model <- naiveBayes(Mark~Attempt+Semester_en+EarnedECP+FY+SY+TY+EX+EXT,
                       data=train)

nB_prediction <- predict(nB_model, test1)
nB_prediction2 <- predict(nB_model, test2)

nB_solution <- data.frame(StudentID=test1$StudentId, Grade=nB_prediction)
nB_solution2 <- data.frame(StudentID=test2$StudentId, Grade=nB_prediction2)

write.csv(nB_solution, file = "nB_solution.csv")
#write.csv(nB_solution2, file = "nB_solution2.csv")

#Confusion Matrix
#install.packages("caret")
library(caret)
table(nB_prediction, test1$Mark)
table(nB_prediction2, test2$Mark)

confusionMatrix(nB_prediction, test1$Mark)
confusionMatrix(nB_prediction2, test2$Mark)
#########################################


#########################################
# SVM
#########################################

#install.packages('e1071')
# library(e1071)
# 
# svm_model <- svm(Mark~Attempt+EarnedECP,
#                  data=train, type='one', kernel='sigmoid')
# 
# svm_prediction <- predict(svm_model, test1)
# 
# svm_solution <- data.frame(StudentID=test1$StudentId, Pass=svm_prediction)
#write.csv(svm_solution, file = "svm_solution.csv")
#########################################


