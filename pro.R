#importing the datasets
datasets = read.csv('Crimes_-_2001_to_Present.csv',nrows = 100000)


#the shape of the datasets
dim(datasets)

#Understand data

#columns names
colnames(datasets)

#summary the datasets
summary(datasets)


#drop columns have no effect
################################################################################


drops <-  c("ID","Case.Number","Date","Year","Updated.On","Location","Description","IUCR","FBI.Code","Block")

datasets = datasets[ , !(names(datasets) %in% drops)]
View(datasets)






#Handling missing values
################################################################################
#for the ward column
datasets$Ward =ifelse(is.na(datasets$Ward),
                      ave(datasets$Ward, FUN=function(x) mean(x,na.rm=TRUE)),
                      datasets$Ward)
#for the Community Area column
datasets$Community.Area =ifelse(is.na(datasets$Community.Area),
                                ave(datasets$Community.Area, FUN=function(x) mean(x,na.rm=TRUE)),
                                datasets$Community.Area)
#for the X.Coordinate column
datasets$X.Coordinate =ifelse(is.na(datasets$X.Coordinate),
                              ave(datasets$X.Coordinate, FUN=function(x) mean(x,na.rm=TRUE)),
                              datasets$X.Coordinate)
#for the Y.coordinate column
datasets$Y.Coordinate =ifelse(is.na(datasets$Y.Coordinate),
                              ave(datasets$Y.Coordinate, FUN=function(x) mean(x,na.rm=TRUE)),
                              datasets$Y.Coordinate)
#for the Latitude column
datasets$Latitude =ifelse(is.na(datasets$Latitude),
                          ave(datasets$Latitude, FUN=function(x) mean(x,na.rm=TRUE)),
                          datasets$Latitude)
#for the Longitude column
datasets$Longitude =ifelse(is.na(datasets$Longitude),
                           ave(datasets$Longitude, FUN=function(x) mean(x,na.rm=TRUE)),
                           datasets$Longitude)







#ENCODING Categorical values
################################################################################

#Encoding the target column(Arrest)
datasets$Arrest = factor(datasets$Arrest,
                         levels = c("true","false"),
                         labels = c(1,0))
#Encoding the Domestic column
datasets$Domestic = factor(datasets$Domestic,
                           levels = c("true","false"),
                           labels = c(1,0))


#primary=c(unique(datasets$Primary.Type))
#length(primary)

#Encoding the primary type column
#datasets$Primary.Type = factor(datasets$Primary.Type,
#                               levels = primary,
#                               labels = c(1:23))
#primary[3]

#locde=c(unique(datasets$Location.Description))
#length(locde)
#Encoding the Location.Description column
#datasets$Location.Description = factor(datasets$Location.Description,
#                                       levels = locde,
#                                       labels = c(1:55))

#iucr_vec=c(unique(datasets$IUCR))
#length(iucr_vec)
#Encoding the IUCR column
#datasets$IUCR = factor(datasets$IUCR,levels = locde,labels = c(1:361))



#block_vec=c(unique(datasets$Block))
#length(block_vec)
#Encoding the Block column
#datasets$Block = factor(datasets$Block,levels = block_vec,labels = c(1:913))


is.na(datasets)

#splitting data into train and test
################################################################################

# install.packages('caTools')
library(caTools)

set.seed(123)
split = sample.split(datasets$Arrest,SplitRatio = 0.8)

training_set=subset(datasets,split==TRUE)
test_set=subset(datasets,split==FALSE)

#training_set visualization
################################################################################
library(ggplot2)
ggplot(data = training_set[1:100,],
       mapping = aes(x = Latitude, 
                     y = Longitude,
                     color = Arrest)) +
  geom_point(alpha = .6,
             size = 1) +
  geom_smooth(method = "lm",
              se = F, 
              size = 1)



ggplot(data =  training_set[1:100,],
       mapping = aes(x = X.Coordinate, 
                     y = Y.Coordinate,
                     color = Arrest)) +
  geom_point(alpha = .7,
             size = 1) +
  geom_smooth(method = "lm",
              se = FALSE, 
              size = 1)

ggplot(data= training_set[1:100,], aes(x=Location.Description, Arrest)) +
  #  geom_point() +
  geom_bar(stat="identity")


#test_set visualization
################################################################################
ggplot(data = test_set[1:100,],
       mapping = aes(x = X.Coordinate, 
                     y = Y.Coordinate,
                     color = Arrest)) +
  geom_point(alpha = .6,
             size = 1) +
  geom_smooth(method = "lm",
              se = F, 
              size = 1)


ggplot(data= test_set[1:100,], aes(x=Location.Description, Arrest)) +
  #  geom_point() +
  geom_bar(stat="identity")




#Build the classifier algorethm
#decision tree
################################################################################
library('rpart')
classifier=rpart(formula = Arrest ~ .,
                 data=training_set)

y_pred = predict(classifier,newdata = test_set[-14],type = 'class')



#naive bayes Algo
################################################################################
#install.packages('e1071')
library(e1071)
model1<- naiveBayes(Arrest ~.,training_set)
test_pred <- predict(model1, newdata = test_set)
training_pred <- predict(model1, newdata = training_set)
cm <- table(training_set$Arrest, test_pred)
#accuracy of train
#install.packages('caret')
library(caret)
training_Accuracy <- confusionMatrix(data=training_pred, reference = training_set$Arrest)
#accuracy of test
test_Accuracy <- confusionMatrix(data=test_pred, reference = tes_set$Arrest)
training_Accuracy
test_Accuracy


# applying k-fold Cross Validation

folds=createFolds(training_set$Arrest,k=10)
cv=lapply(folds, function(x){
  
  training_fold=training_set[-x,]
  test_fold=training_set[x,]
  classifier<- naiveBayes(Arrest ~.,training_fold)
  y_pred = predict(classifier, newdata = test_fold[-14])
  cm = table(test_fold$Arrest,y_pred)
  accuracy=(cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[2,1]+cm[2,1])
  return(accuracy) 
})
mean(as.numeric(cv))

#grid search

#nbDiscrete

classifier = train(form = Arrest ~ ., data = training_set, method = 'nb') 
classifire
