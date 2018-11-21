
# Read the data:   
data = read.table("titanic.csv",sep=",",header = TRUE)
#View data
View(data)
lapply(data, class)
View(data)

#removing the columns with non numeric value
data_numeric <- data[,sapply(data, is.numeric)]
names(data_numeric)
#adding the partition column back
dtf<- cbind(data_numeric,partition=data$partition)
names(dtf)
View(dtf)

# did passengers who paid more survived more/?
ggplot(data,aes(x=sex,y=fare))+geom_boxplot()+scale_y_log10()+facet_wrap(~survived)
# demographics of children who survived more male child or female child ?
ggplot(data,aes(x=sex,y=age))+geom_violin()+facet_wrap(~survived)
#did females in 3rd class died the most
ggplot(data,aes(x=sex,y=pclass))+geom_violin()+facet_wrap(~survived)


#calculate target incidence in the full data set
table(dtf$survived)

nrow(dtf)


# Split the data in train and test data
library(dplyr)
train = filter(dtf, partition== 'train')
test  = filter(dtf, partition == 'test')

names(test)
#confirmation for your filtering
table(test$partition)
View(test)
names(train)

#model performance evaluattion 
auc = function(trueval, predval){
  df = as.data.frame(cbind(trueval,predval))
  names(df) = c("trueval","predval")
  auc = roc(trueval~predval,data=df)$auc
  return(auc)
}

#model building
names(dtf)
variables= names(dtf[c(-1,-3,-8)]) # getting rid of other non relevant columns and target columns
variables
#initializing a list of predicted test and train AUC values
aucl=data.frame(variable= variables,pred_test=double(length(variables)),pred_train=double(length(variables)))
View(auck)
for(v in variables){
mdl=glm(paste("survived ~", v),data=train ,family="binomial")
pred_train= predict(mdl,newdata = train,type="response")
pred_test= predict(mdl,newdata = test,type="response")
aucl[aucl$variable==v,2]=auc(test$survived,pred_test)
aucl[aucl$variable==v,3]=auc(train$survived,pred_train)
}
View(aucl)

# passenger class and fare are the most relevanct features for predicting 
# in if the passenger survived or not