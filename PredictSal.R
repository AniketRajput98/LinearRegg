#install.packages("caTools")
#install.packages("ggplot2")
#install.packages("gridExtra")
library(ggplot2)
library(caTools)
library(gridExtra)

#Importing Data from Local storage
dataset<-read.csv("Predicting_Salaries.csv",header = T)

#setting seed to get same split on different execution
set.seed(123)

#splitting in 3/4 ratio.
sample<-sample.split(dataset$AnnualSalary,SplitRatio = .75)
data_train<-subset(dataset,sample == T)
data_test<-subset(dataset,sample == F)

#AnnualSalary -> Dependent variable 
#Years of exp. -> Independent variable
linearRegg<-lm(AnnualSalary~YearsOfExperience,data = data_train)

#Predicting with test data
test_predict<-predict(linearRegg,newdata=data_test)



Train.plot<-ggplot()+
              geom_point(aes(x=data_train$YearsOfExperience,y=data_train$AnnualSalary),colour="red")+
              geom_line(aes(x=data_train$YearsOfExperience,y=predict(linearRegg,newdata = data_train)),colour="blue")+
              ggtitle("Annual sal vs Experience(Trained)")+
              xlab("Years of experience")+
              ylab("Annual salary")+
              scale_x_continuous(limits = c(0,12))+
              scale_y_continuous(limits = c(0,150000))

Test.plot<- ggplot()+
              geom_point(aes(x=data_test$YearsOfExperience,y=data_test$AnnualSalary),colour="red")+
              geom_line(aes(x=data_train$YearsOfExperience,y=predict(linearRegg,newdata = data_train)),colour="blue")+
              ggtitle("Annual sal vs Experience(Testing)")+
              xlab("Years of experience")+
              ylab("Annual salary")+
              scale_x_continuous(limits = c(0,12))+
              scale_y_continuous(limits = c(0,150000))

#To dispay both plots
gridExtra::grid.arrange(Train.plot,Test.plot,ncol=2)
