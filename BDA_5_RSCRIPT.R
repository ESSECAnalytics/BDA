###########################################################
#                  Big Data Analytics                     #
#       Session 5 - Classification and Regressions        #
#                                                         #
#               Advanced Regressions                      #
#                 Decision Trees                          #  
#                                                         #
# Author: Nicolas Glady                                   #
###########################################################

###########################################################
# Disclaimer: this script is used to produce the examples #
#  presented during the course Big Data Analytics. The    #
#  author is not responsible in any way for any problem   #
#  encountered during this code execution.                #
###########################################################

#################################
#### Advanced Regressions    ####
#################################

# Simple regression
library(openxlsx)

rawdata <- read.xlsx("ABBELECTRIC.xlsx", sheet = "Univariate", startRow = 1, colNames = TRUE)

logreg<-glm(Choice ~ ., data = rawdata, family = "binomial") # Logistic regression
summary(logreg)

purchase<-logreg$fitted.values[rawdata$Choice==1]    # Select the fitted proba for Purchasers
nonpurchase<-logreg$fitted.values[rawdata$Choice==0] # Select the fitted proba for Non-Purchasers
  
plot(density(nonpurchase),col = "red", xlim =c(-.2,1),ylim=c(0,max(c(density(nonpurchase)$y,density(purchase)$y))),main = paste("Purchased vs. Non-Purchased", collapse = ''))
lines(density(purchase), col = "green") # plot the non-purchased in green

library(ROCR)
pred<-prediction(logreg$fitted.values,logreg$y) # Create a predictions object
perf <- performance(pred, "tpr", "fpr") # Assess performance: True Positives vs. True Negatives
plot(perf)

library(SDMTools)
confusion.matrix(logreg$y,logreg$fitted.values,threshold=0.25) # Create a confusion matrix

auc.tmp <- performance(pred,"auc"); auc <- as.numeric(auc.tmp@y.values)

# Fixed effect
rawfixdata <- read.xlsx("ABBELECTRIC.xlsx", sheet = "Fixeffect", startRow = 1, colNames = TRUE)
logregf<-glm(Choice ~ ., data = rawfixdata, family = "binomial") # Logistic regression

summary(logregf)
auc.tmp <- performance(prediction(logregf$fitted.values,logregf$y),"auc"); 
aucf <- as.numeric(auc.tmp@y.values)

# Multinomial regression

choicedata <- read.xlsx("ABBELECTRIC.xlsx", sheet = "Multivariate", startRow = 1, colNames = TRUE)
choicedata$Area<-factor(choicedata$Area)
xyplot(Turnover ~ Area,type="p",group=Choice,data=choicedata,
       auto.key=list(title="Choice", space = "right", cex=1.0, just = 0.95),
       par.settings = list(superpose.line=list(pch = 0:18, cex=1)),
       scales=list(x=list(at=seq(1, 3, length.out=3), labels=c("Area 1","Area 2","Area 3"))))

library(nnet)
choicedata$Choice<-factor(choicedata$Choice)
mnomchoice<-multinom(Choice ~ Area+Turnover, reflevel="ABB",data=choicedata)
summary(mnomchoice)

predchoice <- data.frame(fitted(mnomchoice),"Predicted"=predict(mnomchoice),"Actual"=choicedata$Choice)
write.xlsx(predchoice, "predchoice.xlsx") 

#################################
#### Decision trees          ####
#################################

# Decision trees:
# Let's understand what drives sales of Mars' brands
purchase <- read.table('purchasei_0.csv',header = TRUE,sep = ",")
purchase$time<-(purchase$yearweek %/% 100 - 2008)*52+ ((purchase$yearweek) %% 100)
purchase$month<-purchase$time%/%4 +1 # aggregate by month

temp <- read.table('Branddetails.csv',header = TRUE, sep = ",")
Branddetails<-subset(temp,Manufacturer=='Mars Inc.') # Select only Mars
purchasebrand<-merge(purchase,Branddetails,by='brand')
purchasemonth<-aggregate(value~household+brand+month,purchasebrand,FUN=sum)
temp<-expand.grid(household=1:5000,month = 1:79)
purchastot<-merge(temp,purchasemonth,by=c("month","household"),all.x=TRUE)
purchastot[is.na(purchastot)] <- 0
View(purchastot)
  
# Compute the exposures to the Mars brands by month
CampaignDetails <- read.table('CampaignDetails.csv',header = TRUE, sep = ",")
Brandtotal <- merge(Branddetails,CampaignDetails,by="Name")
contact <- read.table('contacts_0.csv',header = TRUE,sep = ",")
contact$time<-(contact$yearweek %/% 100 - 2008)*52+ ((contact$yearweek) %% 100)
contact$month<-contact$time%/%4 +1 # aggregate by month
contactbrand<-merge(contact,Brandtotal,by='copy')
aggbrandcontact <- aggregate(value ~ household+Type+month, data = contactbrand, FUN = sum)
# and reshape the rows in columns
library(reshape)
aggcontactbbrand<-cast(aggbrandcontact, household+month~Type, sum)
View(aggcontactbbrand)

# Merge everything 
mergetot<-merge(purchastot,aggcontactbbrand,by=c('household','month'),all.x=TRUE)
mergetot[is.na(mergetot)]<-0
mergetot$brand<-factor(mergetot$brand)
View(mergetot)

# So how can we know which brand one household will buy?
library(party)
estimbrand<-ctree(brand~.,data=mergetot[,c(3,5:8)]) #To estimate the tree
plot(estimbrand)  # To plot the tree
print(estimbrand)  # To have a text representation of the tree

prop.table(table(mergetot$brand)) # The "unconditional" proportions

plot(estimbrand,type="simple", # To have the proportion
     inner_panel=node_inner(estimbrand,pval = FALSE,# no p-values
                            id = FALSE),          # no id on nodes
     terminal_panel=node_terminal(estimbrand, id=FALSE,
                                  digits = 2,                   # few digits on numbers
                                  fill = c('white')))            # make box white not grey

# Decomposing the tree
mergetot$ismars<-mergetot$brand!='0' # Mars vs. Not-Mars purchase dummy variable
summary(lm(ismars~.,data=mergetot[,c(5:9)])) # which driver is the most significant?
estimbrandN1<-ctree(ismars~.,data=mergetot[,c(5:9)],controls=ctree_control(maxdepth=1))
# We use maxdepth = 1 to force the tree to be with only one split
plot(estimbrandN1,type='simple')

# Predictions: what drives our sales?
aggpurch <- aggregate(value ~ household+month, data = purchastot, FUN = sum)

# Let's add the sociodemographics and merge everything:
sociodemo <- read.table('sociodemo_0.csv',header = TRUE, sep = ",",fileEncoding="ISO-8859-7")
temp<-merge(sociodemo,aggpurch,by="household")
mergetot2<-merge(temp,aggcontactbbrand,by=c('household','month'),all.x=TRUE)
mergetot2[is.na(mergetot2)]<-0

#Estimate a regression tree:
estimVal<-ctree(value~.,data=mergetot2) 
plot(estimVal) # To plot the tree

# Comparing a pruned tree vs. non-pruned one on a training set vs. test set
trainset<-mergetot2[mergetot2$month<=12*4,] # Select the first 4 years
valset<-mergetot2[mergetot2$month>12*4,] # And use the remaining data for testing

library(rpart)
estimtrain_max<-rpart(value~.,data=trainset,control = rpart.control(cp = 0.000005)) # A very complex tree
plotcp(estimtrain_max)
estimtrain_pruned<-prune(estimtrain_max,cp=estimtrain_max$cptable[387]) 

# Use the function predict to predict new data from a model
predict_max<-predict(estimtrain_max,newdata=valset)
predict_pruned<-predict(estimtrain_pruned,newdata=valset)
# Use the function corr to compute the correlation between two vectors
cor(valset$value,predict_max)
cor(valset$value,predict_pruned)


