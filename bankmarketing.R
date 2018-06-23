# Importing the dataset
mydata<-read.csv('C:/Users/gagan/Downloads/banking_data.csv',header=T)

#Understanding the data
View(mydata)
str(mydata)
summary(mydata)

# Dropping Top non important variables from data
mydata$poutcome <- NULL

head(mydata)

# Encoding Categorical Variables
mydata$default <- factor(mydata$default)
mydata$housing <- factor(mydata$housing)
mydata$marital <- factor(mydata$marital)
mydata$contact <- factor(mydata$contact)
mydata$y <- factor(mydata$y)
mydata$education <- factor(mydata$education)
mydata$day <- factor(mydata$day)

#Descriptive analysis
var_Summ=function(x){
  if(class(x)=="numeric"){
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    mean<-mean(x,na.rm=T)
    std<-sd(x,na.rm=T)
    var<-var(x,na.rm=T)
    min<-min(x,na.rm=T)
    p1<-quantile(x,0.01,na.rm=T)
    p5<-quantile(x,0.05,na.rm=T)
    p10<-quantile(x,0.1,na.rm=T)
    q1<-quantile(x,0.25,na.rm=T)
    q2<-quantile(x,0.5,na.rm=T)
    q3<-quantile(x,0.75,na.rm=T)
    p90<-quantile(x,0.9,na.rm=T)
    p95<-quantile(x,0.95,na.rm=T)
    p99<-quantile(x,0.99,na.rm=T)
    max<-max(x,na.rm=T)
    UC1=mean(x,na.rm=T)+3*sd(x,na.rm=T)
    LC1=mean(x,na.rm=T)-3*sd(x,na.rm=T)
    UC2=quantile(x,0.99,na.rm=T)
    LC2=quantile(x,0.01,na.rm=T)
    iqr=IQR(x,na.rm=T)
    UC3=q3+1.5*iqr
    LC3=q1-1.5*iqr
    ot1<-max>UC1 | min<LC1 
    ot2<-max>UC2 | min<LC2 
    ot3<-max>UC3 | min<LC3
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,mean=mean,std=std,var=var,min=min,p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max,ot_m1=ot1,ot_m2=ot2,ot_m2=ot3))
  }
  else{
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    fre<-table(x)
    prop<-prop.table(table(x))
    #x[is.na(x)]<-x[which.max(prop.table(table(x)))]
    
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,freq=fre,proportion=prop))
  }
}

#Vector of numaerical variables
num_var= sapply(mydata,is.numeric)
Other_var= !sapply(mydata,is.numeric)

#Applying above defined function on numerical variables
my_num_data<-t(data.frame(apply(mydata['age'], 2, var_Summ)))
my_num_data<-t(data.frame(apply(mydata['occupation'], 2, var_Summ)))
my_num_data<-t(data.frame(apply(mydata['quartor'], 2, var_Summ)))
my_num_data<-t(data.frame(apply(mydata['duration'], 2, var_Summ)))
my_num_data<-t(data.frame(apply(mydata['campaign'], 2, var_Summ)))
my_num_data<-t(data.frame(apply(mydata['pdays'], 2, var_Summ)))
my_num_data<-t(data.frame(apply(mydata['previous'], 2, var_Summ)))
my_cat_data<-data.frame(t(apply(mydata[Other_var], 2, var_Summ)))
View(my_num_data)
View(my_cat_data)


# Removing missing values 
mydata1=na.omit(mydata)

# Logistic Regression Model
#Splitting data into Training, Validaton and Testing Dataset
train_ind <- sample(1:nrow(mydata1), size = floor(0.70 * nrow(mydata1)))

training<-mydata1[train_ind,]
testing<-mydata1[-train_ind,]

#Building Model for training dataset

#3
# prediction of term deposit with respect to education as predictor variable 
fit<-glm(y~education,data = training,
         family = binomial(logit))

#Output of Logistic Regression
summary(fit)
ls(fit)
fit$model

#Odd ratio for education as a predictor variable 
exp(coef(fit))

#From result we can see education1 and education2 will bring positive results on term deposit subscription.
#They should be considered wisely in the marketing campaign.So these are significant association.


# with respect to campaign
fit<-glm(education~campaign,data = training,
         family = binomial(logit))


#Probabilities
probabilities <- predict.glm(fit,mydata['education'],type='response')
predictions <- cut(probabilities,c(-Inf,0.5,Inf),labels=c('no','yes'))

table(predictions,mydata$y)

confusion.matrix <- prop.table(table(predictions, mydata$y))

confusion.matrix



#4.

fit<-glm(y~day,data = training,
         family = binomial(logit))

#Probabilities
probabilities <- predict.glm(fit,mydata['day'],type='response')
predictions <- cut(probabilities,c(-Inf,0.5,Inf),labels=c('no','yes'))

probabilities

probs <- exp(probabilities)/(1+exp(probabilities))

probs

#Confusion matrix
confusion.matrix <- prop.table(table(predictions, mydata$y))

confusion.matrix


#5

fit<-glm(y~.,data = training,
         family = binomial(logit))

#Probabilities
probabilities <- predict.glm(fit,mydata['day'],type='response')
predictions <- cut(probabilities,c(-Inf,0.5,Inf),labels=c('no','yes'))

#Confusion matrix
confusion.matrix <- prop.table(table(predictions, mydata$y))

confusion.matrix

accuracy <- confusion.matrix[1,1] + confusion.matrix[2,2] 


#6

optimalCutoff(mydata$y, prob)[1] 
optCutOff <- optimalCutoff(mydata$y, prob)[1] 
misClassError(mydata$y, prob, threshold = optCutOff)
#Decision Tree Classification
#Fit the tree model using the training data
data_training_tree <- tree(y~., mydata)
plot(data_training_tree)
text(data_training_tree, pretty = 0)

# Test this tree model using testing data
data_training_predict <- predict(data_training_tree, data_testing, type="class")
predictions <- cut(data_training_predict,c(-Inf,0.5,Inf),labels=c('no','yes'))

#Confusion matrix
confusion.matrix <- prop.table(table(predictions, mydata$y))

confusion.matrix

accuracy <- confusion.matrix[1,1] + confusion.matrix[2,2] 

# Misclassification Error
mean(data_training_predict != mydata$y) # 0.5502703 ~= 55%
