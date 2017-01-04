###########################################################
#                  Big Data Analytics                     #
#       Session 2 - Knowledge Discovery in Data           #
#                                                         #
#                   Introduction                          #
#                 Basic Regression                        #  
#               Visualizing Results                       #
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
#### Introduction            ####
#################################

# You can leave a comment after the sign #. It won't be interpreted by the computer 

# Open a text file into a data frame
Branddetails <- read.table("Branddetails.txt",header = TRUE, sep = "\t")
# <- means "assign", e.g. variable <- value
# Make sure you are in the right working directory (via Session->Set Working Directory)

# R is key sensitive, i.e. branddetails is different from Branddetails
# You can always have information about a function with ?function. Try ?read.table

# Open an xlsx file
# You need to call a library, e.g. "XLConnect" 
library(XLConnect)
# But make sure that the related package is installed first with: install.packages("XLConnect")
CampaignDetails<-readWorksheet(loadWorkbook("CampaignDetails.xlsx"),sheet=1)

Brandtotal <- merge(Branddetails,CampaignDetails,by="Name")

# The values in the "by" need to be exactly the same in the two data frames
# you can view the content of a dataframe by typing it in the console or via View(dataframe)
# e.g.:

View(Brandtotal)

purchase <- read.table("purchasei_0.csv",header = TRUE, sep = ",")
# Which(condition) selects the entries that is true for 'condition'
# datafames are the most used type of variable in R. It's basically a structured matrix
# dataframe[10,1:4] selects the line 10 and columns 1 to 4 of data.frame 
# adding a $ after a dataframe allows to select the column by its name
View(purchase[which(purchase$household==200),1:4])
# Note this is equivalent to View(subset(purchase[,1:4], household==200))

# Transforming yearweek into a time unit format:
purchase$time<-(purchase$yearweek %/% 100 - 2008)*52+ ((purchase$yearweek) %% 100)

# The data has been cleansed, so no missing value!
# Making same computations

summary(purchase$value) # Summary statistics of the value
sumvalue<-sum(purchase$value) # The sum of all the values
stdvalue<-sd(purchase$value) # The standard deviation
sumvalue5t10<-sum(purchase$value[which((purchase$time>=5)&(purchase$time<=10))]) # For time units 5 to 10
avgvalue7<-mean(purchase$value[which(purchase$time==7)]) # Average value at time point 7
# Note how you can "embed" commands, e.g. function(variable[function(variable)]) 

# Aggregating Values is important !
valuebhh<-aggregate(value ~ household, data=purchase, FUN=sum) # Aggregate values (sum) by household
valuebtime<-aggregate(value ~ time, data=purchase, FUN=sum) # Aggregate values (sum) by time point
countbhh<-aggregate(time ~ household, data=purchase, FUN=length) # Aggregate values (count) by time point

View(valuebhh)
View(valuebtime)

hist(valuebhh$value) # Produce a histogram of the values by household
hist(log(valuebhh$value)) # Histogram of the log-transform

# Aggregating by value and summary statistics

contacts <- read.table('contacts_0.csv',header = TRUE, sep = ",")
Contacttotal<-merge(contacts,Brandtotal,by="copy")

barplot(prop.table(table(Contacttotal$Type))) # compute the proportion of each type of media

# Let's compute the number of weeks with at least one exposure by type by household:

hhexpos<-table(household=Contacttotal$household,Type=Contacttotal$Type)

# Note that the actual number of exposures by type by household is given by:
hhexposseigui<-aggregate(value ~ household+Type,data=Contacttotal,sum)

# When you have special chars (here German), be aware of the file encoding:
sociodemo <- read.table('sociodemo_0.csv',header = TRUE, sep = ",",fileEncoding="ISO-8859-7")
MediaSocioDemo<-merge(hhexpos,sociodemo,by="household")
aggMedia<-aggregate(Freq ~ Work_Type+Type, data=MediaSocioDemo, FUN=sum)
library(reshape)
cast(aggMedia, Work_Type~Type, mean)

# Outliers:

truncval<-valuebhh$value # Create a new variable
truncval[which(valuebhh$value>(sd(valuebhh$value)*1.5))]<-sd(valuebhh$value)*1.5 # Truncation
hist(truncval)

#################################
#### Basic Regression        ####
#################################

rawregdata <- read.xlsx("TQ_BigDataAnalyt.xlsx", sheet = "BigDataAnalyt", startRow = 1, colNames = TRUE)

selregdb<-rawregdata[,c(10,11,16,17,18,19,20,21,22,23,24,25)] 
# c(a,b,c) creates a vector with values a, b and c.

# We need a category for the logit, so let's create it:
selregdb$StrongCode<-(selregdb[,4]=="Average")|(selregdb[,4]=="Above average") 

tempregdb<-selregdb[,c(1,2,3,5,6,13)]
logreg<-glm(StrongCode ~ ., data = tempregdb, family = "binomial") # Logistic regression
summary(logreg)

selregdb$agenumeric<-as.numeric(selregdb[,2]) # Transform age into a numeric variable

selregdb$LevelDB<-array(0,c(dim(selregdb)[1],1)) # Transform Database skills into a numeric variable
selregdb$LevelDB[(selregdb[,5]=="Excellent")]<-5
selregdb$LevelDB[(selregdb[,5]=="Above average")]<-4
selregdb$LevelDB[(selregdb[,5]=="Average")]<-3
selregdb$LevelDB[(selregdb[,5]=="Below average")]<-2
selregdb$LevelDB[(selregdb[,5]=="Poor")]<-1

selregdb$LevelWeb<-array(0,c(dim(selregdb)[1],1)) # Transform Web skills into a numeric variable
selregdb$LevelWeb[(selregdb[,6]=="Excellent")]<-5
selregdb$LevelWeb[(selregdb[,6]=="Above average")]<-4
selregdb$LevelWeb[(selregdb[,6]=="Average")]<-3
selregdb$LevelWeb[(selregdb[,6]=="Below average")]<-2
selregdb$LevelWeb[(selregdb[,6]=="Poor")]<-1

logreg<-glm(StrongCode ~ agenumeric+LevelDB+LevelWeb, data = selregdb, family = "binomial") # Logistic regression
summary(logreg)

selregdb$LevelCode<-array(0,c(dim(selregdb)[1],1)) # Transform Code skills into a numeric variable
selregdb$LevelCode[(selregdb[,4]=="Excellent")]<-5
selregdb$LevelCode[(selregdb[,4]=="Above average")]<-4
selregdb$LevelCode[(selregdb[,4]=="Average")]<-3
selregdb$LevelCode[(selregdb[,4]=="Below average")]<-2
selregdb$LevelCode[(selregdb[,4]=="Poor")]<-1

linreg<-glm(LevelCode ~ agenumeric+LevelDB+LevelWeb, data = selregdb) # Linear regression
summary(linreg)

#################################
#### Visualizing Results     ####
#################################

# Visualizing time series
plot(valuebtime) #plot value by time

library(lattice)
valuebtimeabrand<-aggregate(value ~ time+brand, data=purchase, FUN=sum) # Aggregate values by time and brand

# Produce the plots:
xyplot(value ~ time,type="p",group=brand,data=valuebtimeabrand,
       auto.key=list(title="Brands", space = "right", cex=1.0, just = 0.95),
       par.settings = list(superpose.line=list(pch = 0:18, cex=1))) 

xyplot(value ~ time,type="smooth",group=brand,data=valuebtimeabrand,
       auto.key=list(title="Brands", space = "right", cex=1.0, just = 0.95),
       par.settings = list(superpose.line=list(pch = 0:18, cex=1))) 

