###########################################################
#                  Big Data Analytics                     #
#         Session 3 - Exploratory Analytics               #
#                                                         #
#                   PCA & ANOVA                           #
#             Hierarchical Clustering                     #  
#               Take home exercise                        #
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
#### PCA & ANOVA             ####
#################################

library(openxlsx)

rawdata <- read.xlsx("BlackBerry Pearl Data.xlsx", sheet = "Positionning", startRow = 1, colNames = TRUE)
# Note that we can select only the numeric part of the data: rawdata[1:22,2:7]
Brands<-rawdata[1:22,2:7]
# To access specific rows or columns, use [] after the variable name

row.names(Brands)<-rawdata[1:22,1] # To name the rows properly

# To transpose a matrix, use the t() command
corBrands<-cor(t(Brands)) # Cor() computes a correlation matrix

# We can save this correlation matrix in xls:
write.xlsx(corBrands, "corBrands.xlsx") 

library(FactoMineR) # A package with PCA tools
pcabb<-PCA(t(Brands), scale.unit=TRUE, ncp=3, graph=T) 
summary(pcabb)

# Outliers and ANOVA

purchase <- read.table('purchasei_0.csv',header = TRUE, sep = ",")
valuebhhd<-aggregate(value ~ household, data=purchase, FUN=sum) # Aggregate values by household
boxplot(valuebhhd$value)

library(doBy) # allows to "summaryBy"
summaryBy(value ~ brand,data=valuebhhdabrand, FUN=c(mean, sd))

valuebhhdabrand<-aggregate(value ~ brand+household, data=purchase, FUN=sum) # Aggregate values by household and brand
boxplot(value ~ brand,data=valuebhhdabrand)

fit <- aov(value ~ brand,data=valuebhhdabrand) # ANOVA
summary(fit)

#################################
#### Hierarchical Clustering ####
#################################

# Merging the different files…

branddetails <- read.table('Branddetails.csv',header = TRUE, sep = ",")
branddetails<-branddetails[1:9,] # Remove the Market shares (useless)
campaigndetails <- read.table('CampaignDetails.csv',header = TRUE, sep = ",")
campaigninfo<-merge(campaigndetails,branddetails,by="Name")

contacts <- read.table("contacts_0.csv",header = TRUE, sep = ",")
contactbhhdbbcopy<-aggregate(value ~ household+copy, data=contacts, FUN=length) # Count contacts by household & copy
names(contactbhhdbbcopy)[names(contactbhhdbbcopy)=="value"] <- "count" # Rename column "value" in "name"

contactbrand<-merge(campaigninfo,contactbhhdbbcopy,by="copy",all = TRUE) # OUTER Join
contactbrand[is.na(contactbrand)] <- 0

contactvalue<-merge(contactbrand,valuebhhdabrand,by=c("household","brand"),all=TRUE)
contactvalue[is.na(contactvalue)] <- 0

plot(contactvalue$count,contactvalue$value)

hist(aggregate(value~household,valuebhhdabrand,FUN=sum)$value)
plot(rank(contactvalue$count),rank(contactvalue$value))

# Hierarchical Clustering

rawdata <- read.xlsx("BlackBerry Pearl Data.xlsx", sheet = "Preferences", startRow = 1, colNames = TRUE)
preferences<-rawdata[,2:ncol(rawdata)]

d <- dist(preferences, method = "euclidean") # distance matrix (Euclidian is ok since ratings)
hcward <- hclust(d, method="ward.D") # Hiearchical Clustering using Ward criterion

plot(hcward)
# draw dendogram with red borders around the 4 clusters
rect.hclust(hcward, k=4, border="red") 

preferences$groups<-cutree(hcward,k=4) #Create segments for k=4
aggregate(.~ groups, data=preferences, FUN=mean)

library(lattice)
preftemp<-preferences
preftemp$groups <- cutree(hcward, k=4) # cut tree into 4 clusters
xyplot(RIM.BlackBerry.Pearl ~ Sidekick3,type="p",group=groups,data=preftemp,
       auto.key=list(title="Group", space = "right", cex=1.0, just = 0.95),
       par.settings = list(superpose.line=list(pch = 0:18, cex=1)))

preferences<-rawdata[,2:ncol(rawdata)]
pcapref<-PCA(preferences, scale.unit=TRUE, ncp=2, graph=T) # Compute PCA

prefgrouppca<-data.frame(pcapref$ind$coord)
prefgrouppca$groups<-preftemp$groups
xyplot(Dim.2 ~ Dim.1,type="p",group=groups,data=prefgrouppca,
       auto.key=list(title="Group", space = "right", cex=1.0, just = 0.95),
       par.settings = list(superpose.line=list(pch = 0:18, cex=1)))

#################################
#### Take home exercise      ####
#################################

# Take home exercise: 1/ PCA on Media

contactbtype<-merge(contacts,campaigninfo,by='copy') # To have the campaigns info
aggcontact<-aggregate(value~household+Type,contactbtype,FUN=sum) # Contacts by Type by hhd
sociodemo <- read.table("sociodemo_0.csv",header = TRUE, sep = ",",fileEncoding="ISO-8859-7")
sociocopy<-merge(expand.grid(household=1:5000,Type = unique(campaigninfo$Type)),sociodemo,by='household',all.x=TRUE)
sociodbbrand<-merge(aggcontact,sociocopy,by=c('household','Type'),all.y=TRUE)
sociodbbrand[is.na(sociodbbrand)]<-0

meanexpos<-aggregate(value~Type,sociodbbrand,FUN=mean) # compute the mean exposure by Type
sociodbbrandMC<-merge(meanexpos,sociodbbrand,by='Type')
sociodbbrandMC$value<-sociodbbrandMC$value.y-sociodbbrandMC$value.x # Mean centering

# Create for each values of each type of socio-demo an aggregation on the number of contacts
hhcontNC<-aggregate(value~Type+Number_of_Children_18_years,sociodbbrandMC,FUN=mean)
colnames(hhcontNC)[2]<-'Variable' # Rename the colname of the variable of interest
hhcontWT<-aggregate(value~Type+employed,sociodbbrandMC,FUN=mean)
colnames(hhcontWT)[2]<-'Variable'
hhcontAG<-aggregate(value~Type+Age_Houshold_Leader,sociodbbrandMC,FUN=mean)
colnames(hhcontAG)[2]<-'Variable'
hhcontST<-aggregate(value~Type+German_States,sociodbbrandMC,FUN=mean)
colnames(hhcontST)[2]<-'Variable'
hhcontIN<-aggregate(value~Type+Household_Net_Income,sociodbbrandMC,FUN=mean)
colnames(hhcontIN)[2]<-'Variable'

hhcont<-rbind(hhcontNC,hhcontWT,hhcontAG,hhcontST,hhcontIN)
hhcontc<-cast(hhcont,Type~Variable,mean) # reshape rows as columns on Type
rownames(hhcontc)<-hhcontc$Type # rename the rownames using brand names
hhcontc$Type<-NULL #remove the now useless column

pcasociodem<-PCA(hhcontc,scale.unit=TRUE, ncp=3,graph=TRUE) 
plot(pcasociodem)
summary(pcasociodem)

# Take home exercise: 2/ Creating a segmentation of the consumers 
# The code below is just an indication to help you kick-starting your analysis:
# You'll have to adapt the code!!!

purchasebname<-merge(purchase,branddetails,by='brand',all.x=TRUE) # To have the brand names
purchasebname$Name<-factor(purchasebname$Name, levels=c(levels(purchasebname$Name), 'Private Label')) # need to add one type of factor for brand 10
purchasebname$Name[is.na(purchasebname$Name)]<-'Private Label'
aggpurchase<-aggregate(value~household+Name,purchasebname,FUN=sum) # Contacts by Name by hhd
sociocopy<-merge(expand.grid(household=1:5000,Name = unique(purchasebname$Name)),sociodemo,by='household',all.x=TRUE)
sociodbbrand<-merge(aggpurchase,sociocopy,by=c('household','Name'),all.y=TRUE)
sociodbbrand[is.na(sociodbbrand)]<-0

# Create for each values of each type of socio-demo an aggregation on the number of contacts

# ... try to adapt the code from the media example...

# Clustering
# varbyhouseholdbybrand is up to you to be created (adapt sociodbbrandMC for purchases)
uspurch<-cast(varbyhouseholdbybrand,household~Name,mean)

d <- dist(uspurch, method = "euclidean") # distance matrix (Euclidian is ok since ratings)
hcpurch <- hclust(d, method="ward.D") # Hiearchical Clustering using Ward criterion

plot(hcpurch)
# etc.
