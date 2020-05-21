#Root insurance
install.packages('ROSE')
install.packages('party')
install.packages('randomForest')
install.packages('caret')
install.packages('UBL')
install.packages('rpart.plot')
install.packages('nnet')
install.packages('naivebayes')
install.packages('psych')
install.packages('cluster')
###Data fetching###########################
getwd()
Acme = read.csv("Acme.csv",header=T,na.strings="?")
fix(Acme)
Acme=na.omit(Acme)
dim(Acme)
summary(Acme)
colnames(Acme)
head(Acme)
length(Acme$policies.sold)
#############Convert into factors:Policies sold,Acme$click#################
str(Acme)

levels(Acme$policies.sold)[levels(Acme$policies.sold)==' -   '] = '0'
levels(Acme$policies.sold)[levels(Acme$policies.sold)==' 1 '] = '1'

levels(Acme$click)[levels(Acme$click)==' -   '] = '0'
levels(Acme$click)[levels(Acme$click)==' 1 '] = '1'

Acme.click =subset(Acme,select=-c(policies.sold,cost))
Acme.click1 = Acme.click[c(1,3,4,5,6,7,2)]

#Data visualising with only click
head(Acme.click1)
str(Acme.click1)
table(Acme.click1$rank)
table(Acme.click1$click,Acme.click1$rank)
#######################################
Temp = Acme.click1

Temp$number_of_vehicles = as.integer(Temp$number_of_vehicles)
Temp$number_of_drivers = as.integer(Temp$number_of_drivers)
Temp$rank = as.integer(Temp$rank)
str(Temp)
summary(Temp)

#Data Analysis of 4 variables
#try to find which type of people more likely to go under which type of ranking
#Comparing

#insured and Non-insured are apporx.same
ins =table(Temp$currently_insured)  
#3,4,5 rank proportions high for both insured and Non insured
#but Non-insurance proportion in higher rank > insurance proportion 
#non insurance propotion in lower rank < insurance proportion
#It indicates for cost$10, Acme earns high ranks(1,2) for non insurance people
prop.table(table(Temp$currently_insured,Temp$rank),1)   

table(Temp$currently_insured,Temp$click,Temp$rank)
barplot(table(Temp$currently_insured,Temp$rank),main="Insured-grey/noninsured-block")
#########################################################################################

table(Temp$number_of_vehicles)           #having 2 veh-customers are high
table(Temp$number_of_vehicles,Temp$rank) #Lower rank for 2 veh-customers/higher rak for1 vehicle cutomers
prop.table(table(Temp$number_of_vehicles,Temp$rank),1)
#It indicates for cost$10, Acme earns high ranks(1,2,3) for 1-vehicle people

table(Temp$number_of_vehicles,Temp$click,Temp$rank)
barplot(table(Temp$number_of_vehicles,Temp$rank),main="veh-2-gray/veh-1-block")
###########################################################################################

table(Temp$number_of_drivers)    #1driver cutomers are little high.
table(Temp$number_of_drivers,Temp$rank) #higher rank for 2 drivers customers/lower rank for 1 drivers customers
prop.table(table(Temp$number_of_drivers,Temp$rank),1)
#it indicates for cost$10,Acme earns higher ranks(1,2,3) for 2 drivers people

table(Temp$number_of_drivers,Temp$click,Temp$rank)
barplot(table(Temp$number_of_drivers,Temp$rank),main="drivers-1-block/drivers-2-gray")
##############################################################################################

table(Temp$marital_status) #married people will be high
table(Temp$marital_status,Temp$rank)#Majority of singles go under ranks 2,3/Majority of Married go under ranks 3.4,5
prop.table(table(Temp$marital_status,Temp$rank),1)
#it indicates for cost$10, Acme earns higher ranks for singles than married

table(Temp$marital_status,Temp$click,Temp$rank)
barplot(table(Temp$marital_status,Temp$rank),main="Married-block/unmarried-gray")

###############################################################################################

#Over all we get higher ranks(1,2,3) ads  for the below category people for cost 10
#Non-insured
#1-vehicle
#2drivers
#single

#Overall we get lower ranks(4,5) ads for the below category people for cost 10
#Insured
#2vehicles
#1driver
#Married

#Now, we ll deep further


#find the proportion of people clicking ad based on ranking

click.tempad <- 1*(levels(Temp$click)[Temp$click]=="1")
grouped.temprank =aggregate(as.numeric(click.tempad),by=list(Temp$rank),FUN=sum)
grouped.temprank

click.tempadn <- 1*(levels(Temp$click)[Temp$click]=="0")
grouped.temprankn =aggregate(as.numeric(click.tempadn),by=list(Temp$rank),FUN=sum)
grouped.temprankn
r1=r2=r3=r4=r5=0
prop.temprc1 =data.frame(r1,r2,r3,r4,r5)
prop.temprc1[1,1]= grouped.temprank[1,2]/(grouped.temprank[1,2] +grouped.temprankn[1,2])
prop.temprc1[1,2]= grouped.temprank[2,2]/(grouped.temprank[2,2] +grouped.temprankn[2,2])
prop.temprc1[1,3]= grouped.temprank[3,2]/(grouped.temprank[3,2] +grouped.temprankn[3,2])
prop.temprc1[1,4]= grouped.temprank[4,2]/(grouped.temprank[4,2] +grouped.temprankn[4,2])
prop.temprc1[1,5]= grouped.temprank[5,2]/(grouped.temprank[5,2] +grouped.temprankn[5,2])


prop.temprc1 = as.matrix(prop.temprc1)

#high ranks, high chance to click
prop.temprc1
barplot(prop.temprc1,main="The liklihood of person clicking ad based on ranking")

#find the relationship between people who clicks,and buys
table(Acme$click,Acme$policies.sold)

#It clearly indicates no ad clicks means no chance to buy,
#If you click the ad, 0.4 chance to buy
prop.table(table(Acme$click,Acme$policies.sold),1)

#Find the realtion between ranking and policy buys
table(Acme$policies.sold,Acme$rank)
prop.table(table(Acme$policies.sold,Acme$rank),1)

#We clearly learn that at rank1 - more likely to buy the policy, if rank 4 or 5-less likely to  buy
#but for rank 2 and 3-there is a big difference between people buy or not buy
####################################################
#class-imbalanced problem

prop.table(table(Temp$click))
barplot(prop.table(table(Temp$rank)),col=rainbow(5),main="click-distribution")



#Data Particion with Acme Data
set.seed(123)
indices = sample(nrow(Acme),0.6*nrow(Acme))
indices
train.Acme = Acme[indices,]
test.Acme = Acme[-indices,]
train.Acme$rank =as.factor(train.Acme$rank)
test.Acme$rank = as.factor(test.Acme$rank)

train.Acme$number_of_vehicles =as.factor(train.Acme$number_of_vehicles )
test.Acme$number_of_vehicles  = as.factor(test.Acme$number_of_vehicles )

train.Acme$number_of_drivers =as.factor(train.Acme$number_of_drivers )
test.Acme$number_of_drivers  = as.factor(test.Acme$number_of_drivers )

summary(train.Acme)
summary(test.Acme)

#Balancing data by rank using UBL library
library(UBL)
mysmote.Acme <- SmoteClassif(rank~.,train.Acme,C.perc = "balance",dist="HEOM")
summary(mysmote.Acme)
str(mysmote.Acme)
str(train.Acme)




#just try decission trees
library(party)
tree.Acme=ctree( rank~currently_insured+number_of_vehicles+number_of_drivers+marital_status+cost,
                 data=mysmote.Acme,controls =ctree_control(mincriterion = 0.99,minsplit=1000))
plot(tree.Acme)
tree.Acme



#Misclassification error for train data
tab = table(predict(tree.Acme),mysmote.Acme$rank)
error =1-sum(diag(tab))/sum(tab)   

#predict
test.pred=predict(tree.Acme,test.Acme,type="prob")
test.val=predict(tree.Acme,test.Acme)

#Misclassification error for test data
tab.test = table(test.val,test.Acme$rank)
error.test =1-sum(diag(tab.test))/sum(tab.test)   
tab.test
error.test
##################################
#################################
#plot with rpart
library(rpart)
mysmote.Acme1 =mysmote.Acme

tree.Acme2=rpart( rank~currently_insured+number_of_vehicles+number_of_drivers+marital_status,
                  data=mysmote.Acme1)
library(rpart.plot)
rpart.plot(tree.Acme2)
#predict
predict(tree.Acme2,test.Acme,type="prob")
##########################################

#random-forest
library(randomForest)
rftree=randomForest(rank~currently_insured+number_of_vehicles+number_of_drivers+marital_status+cost,
                    data=mysmote.Acme,ntree=300,importance=TRUE,proximity=TRUE)
print(rftree)
attributes(rftree)

#prediction-random forest
library(caret)
library(e1071)
rf.pred =predict(rftree,test.Acme)
confusionMatrix(rf.pred,test.Acme$rank)

plot(rftree)

#variable importance
varImpPlot(rftree)
importance(rftree)
varUsed(rftree)
################################Analysis##################################
#find the proportion of people clicking ad based on ranking

click.ad <- 1*(levels(train.Acme$click)[train.Acme$click]=="1")
grouped.rank =aggregate(as.numeric(click.ad),by=list(train.Acme$rank),FUN=sum)
grouped.rank

click.adn <- 1*(levels(train.Acme$click)[train.Acme$click]=="0")
grouped.rankn =aggregate(as.numeric(click.adn),by=list(train.Acme$rank),FUN=sum)
grouped.rankn
r1=r2=r3=r4=r5=0
prop.rc1 =data.frame(r1,r2,r3,r4,r5)
prop.rc1[1,1]= grouped.rank[1,2]/(grouped.rank[1,2] +grouped.rankn[1,2])
prop.rc1[1,2]= grouped.rank[2,2]/(grouped.rank[2,2] +grouped.rankn[2,2])
prop.rc1[1,3]= grouped.rank[3,2]/(grouped.rank[3,2] +grouped.rankn[3,2])
prop.rc1[1,4]= grouped.rank[4,2]/(grouped.rank[4,2] +grouped.rankn[4,2])
prop.rc1[1,5]= grouped.rank[5,2]/(grouped.rank[5,2] +grouped.rankn[5,2])

prop.rc1
prop.rc1 = as.matrix(prop.rc1)
barplot(prop.rc1)
##############################################################################
###########################################################################################################

#Part2
#Just analyse irrespective of ad ranking, the person is intersted to buy policies or not

Acme.Temp =subset(Acme,select=-c(cost))

head(Acme.Temp)
str(Acme.Temp)
table(Acme.Temp$rank)
table(Acme.Temp$click,Acme.Temp$rank)


click.buy = Acme.Temp[0,]
click.notbuy = Acme.Temp[0,]
notclick.notbuy = Acme.Temp[0,]
###########################################

#Try to find any relationship between charecterstic of the customers and policy sold with out ranking
for (i in 1:dim(Acme.Temp)[1]) {
  if(Acme.Temp$click[i] == '1'&& Acme.Temp$policies.sold[i] =='1'){
    click.buy[i,] =Acme.Temp[i,]
  }
  else if(Acme.Temp$click[i] == '1'&& Acme.Temp$policies.sold[i] =='0'){
    click.notbuy[i,] =Acme.Temp[i,]
  }
  else if(Acme.Temp$click[i] == '0'&& Acme.Temp$policies.sold[i] =='0'){
    notclick.notbuy[i,] =Acme.Temp[i,]
  }
  
}
click.buy=na.omit(click.buy)
click.notbuy=na.omit(click.notbuy)
notclick.notbuy=na.omit(notclick.notbuy)

#binding (click and buy) and (click not buy) data together.
click = rbind(click.buy,click.notbuy)
click1=subset(click,select=-c(click,rank))

library(UBL)
#Data Particion
set.seed(123)
indices = sample(nrow(click1),0.6*nrow(click1))
train.click = click1[indices,]
test.click   = click1[-indices,]
str(train.click)
head(train.click)
table(train.click$policies.sold)


#just try decission trees
library(party)
tree.click=ctree(policies.sold~currently_insured+number_of_vehicles+number_of_drivers+marital_status,
                 data=train.click,controls =ctree_control(mincriterion = 0.95,minsplit=1000))
plot(tree.click)


#plot with rpart
library(rpart)

#relationship between charecterstic of the customers and policy sold with out ranking
policy.Acme2=rpart( policies.sold~currently_insured+number_of_vehicles+number_of_drivers+marital_status,
                  data=train.click)
library(rpart.plot)
rpart.plot(policy.Acme2)



############################################################Not find exactly




####If the person is
#####################grouping########################
table(Acme.Temp$number_of_vehicles)
table(Acme.Temp$policies.sold,Acme.Temp$number_of_vehicles)

table(Acme.Temp$number_of_drivers)
table(Acme.Temp$policies.sold,Acme.Temp$number_of_drivers)

table(Acme.Temp$currently_insured)
table(Acme.Temp$policies.sold,Acme.Temp$currently_insured)

#####################################################################
####################Working with Logistic Regression#######
#seems that 'number of vehicles' variable  is not important in logistic regression to classify ranks?

Temp.log = train.Acme
mysmote.Acmelog <- SmoteClassif(rank~.,Temp.log,C.perc = "balance",dist="HEOM")
summary(mysmote.Acmelog)
str(mysmote.Acmelog)

library(nnet)
library(boot)
click.cv1=rep(0,5)
click.cv2=rep(0,5)
click.cv3=rep(0,5)
click.cv4=rep(0,5)
click.cv5=rep(0,5)
for(i in 1:5) {
  
  click.mod1 = glm(formula = rank~currently_insured +number_of_vehicles+number_of_drivers,family='binomial',
                   data=mysmote.Acmelog)
  click.cv1[1] =cv.glm(mysmote.Acmelog,click.mod1,K=5)$delta[1]
  
  click.mod2 = glm(formula = rank~currently_insured +number_of_vehicles+number_of_drivers+marital_status,
                   family='binomial',data=mysmote.Acmelog)
  click.cv2[i] =cv.glm(mysmote.Acmelog,click.mod2,K=5)$delta[1]
  
  click.mod3 = glm(formula = rank~currently_insured + number_of_vehicles + number_of_drivers + marital_status
                   +(currently_insured*number_of_vehicles),family='binomial',data=mysmote.Acmelog)
  click.cv3[i] =cv.glm(mysmote.Acmelog,click.mod3,K=5)$delta[1]
  
  click.mod4 = glm(formula = rank~currently_insured + number_of_vehicles + number_of_drivers + marital_status
                   +(currently_insured*number_of_drivers),family='binomial',data=mysmote.Acmelog)
  click.cv4[i] =cv.glm(mysmote.Acmelog,click.mod4,K=5)$delta[1]
  
  click.mod5 = glm(formula = rank~currently_insured + number_of_vehicles + number_of_drivers + marital_status
                   +(currently_insured*marital_status),family='binomial',data=mysmote.Acmelog)
  click.cv5[i] =cv.glm(mysmote.Acmelog,click.mod5,K=5)$delta[1]
  
}

MSE1 =mean(click.cv1)
MSE2 =mean(click.cv2)
MSE3 =mean(click.cv3)
MSE4 =mean(click.cv4)
MSE5 =mean(click.cv5)
summary(click.mod4)

click.modfin = multinom(formula = rank~currently_insured +  number_of_drivers +(currently_insured*number_of_drivers),
                   data=mysmote.Acmelog)


summary(click.modfin)
#Prediction

p1=predict(click.modfin,test.Acme)

#Misclassification error
tab.logtest = table(p1,test.Acme$rank)
error.logtest =1-sum(diag(tab.logtest))/sum(tab.logtest)   
tab.logtest
error.logtest                    # The error rate in logistic is higher than the tree model

#################Naive-Bayes
library(dplyr)
library(ggplot2)
library(psych)
library(naivebayes)

#Data visualisation
Temp1=Temp
Temp1$number_of_vehicles =as.factor(Temp$number_of_vehicles)
Temp1$number_of_drivers =as.factor(Temp$number_of_drivers)
str(Temp1)
Temp1 %>%
      ggplot(aes(x=rank,y=currently_insured,fill = rank)) +
      geom_boxplot() +
      ggtitle("Boxplot of currently insured")

Temp1 %>%
  ggplot(aes(x=rank,y=number_of_vehicles,fill = rank)) +
  geom_boxplot() +
  ggtitle("Boxplot of number of vehicles")

Temp1 %>%
  ggplot(aes(x=rank,y=number_of_drivers,fill = rank)) +
  geom_boxplot() +
  ggtitle("Boxplot of number of drivers")

Temp1 %>%
  ggplot(aes(x=rank,y=marital_status,fill = rank)) +
  geom_boxplot() +
  ggtitle("Boxplot of Married")

#Bayes Model
mysmote.Acme2 =mysmote.Acme
str(mysmote.Acme2)
Bayes.Acme2=naive_bayes( rank~currently_insured+number_of_vehicles+number_of_drivers+marital_status,
                  data=mysmote.Acme2)
plot(Bayes.Acme2)

#Prediction
pre.bay=predict(Bayes.Acme2,test.Acme)
tab.bay=table(pre.bay,test.Acme$rank)
1-sum(diag(tab.bay))/sum(tab.bay)
###################################################################
str(Acme)
Acme_analysis =subset(Acme,select=-c(rank,cost,click))
Acme_analysis$number_of_vehicles = as.factor(Acme_analysis$number_of_vehicles)
Acme_analysis$number_of_drivers = as.factor(Acme_analysis$number_of_drivers)
str(Acme_analysis)
#hirerarical clustering to learn about potential buyers
library(cluster)
head(Acme_analysis)
dis_Acme =daisy(Acme_analysis[,2:5],metric = c("gower"))
summary(dis_Acme)
#divisive clustering

divisive.clust = diana(as.matrix(dis_Acme),diss=TRUE,keep.diss =TRUE)
plot(divisive.clust)