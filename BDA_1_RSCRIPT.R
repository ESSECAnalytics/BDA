###########################################################
#                  Big Data Analytics                     #
#   Session 1 - Introduction to Big Data Analytics        #
#                                                         #
#                   Survey Statistics                     #
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
#### Survey Statistics       ####
#################################

# HAVE GENDER AS THE FIRST COLUMN AND AGE AS THE SECOND ONE!

library(XLConnect) # Make sure to "install.packages('XLConnect')" and also to have the right version of Java installed on your machine.
rawdata<-readWorksheet(loadWorkbook("TQ_BigDataAnalyt_2017T2.xlsx"),sheet="RespondentData.Text")

# 1 Who you are

slices<-prop.table(table(rawdata$X1...What.is.your.gender))
pct <- round(slices/sum(slices)*100)
lbls <- c('Female','Male')
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls))) 

hist(as.numeric(rawdata[,2]),col=rainbow(5)) 

library("wordcloud")
words=paste(unlist(rawdata[,3:5]),collapse='')
word.cloud <- wordcloud(words,scale=c(8,.2),min.freq=1,
                    max.words=Inf, random.order=FALSE, rot.per=.15, colors=brewer.pal(8,"Dark2"))

# 2 Where you come from & your skills

barplot(prop.table(table(rawdata[,7])), col=rainbow(length(unique(rawdata[,7]))))  # Zoom it to see the full labels!

rankedskills<-factor(rawdata[,8],levels=c('Poor','Below average','Average','Above average','Excellent'))
                      #,'Excellent'))
barplot(prop.table(table(rankedskills)),col=heat.colors(length(unique(rankedskills))))
title(main='General Programming Skills') 

rankedskills<-factor(rawdata[,9],levels=c('Poor','Below average','Average','Above average','Excellent'))
#,'Excellent'))
barplot(prop.table(table(rankedskills)),col=heat.colors(length(unique(rankedskills))))
title(main='Database Management Skills') 

# 3 What are your expectations

rankedskills<-factor(rawdata[,13],levels=c('Very Unimportant','Somewhat Unimportant','Neither Important nor Unimportant','Somewhat Important','Very Important'))
barplot(prop.table(table(rankedskills)),col=topo.colors(length(unique(rankedskills))))
title(main='Quantitative Methods and Statistics') 

rankedskills<-factor(rawdata[,14],levels=c('Very Unimportant','Somewhat Unimportant','Neither Important nor Unimportant','Somewhat Important','Very Important'))
barplot(prop.table(table(rankedskills)),col=topo.colors(length(unique(rankedskills))))
title(main='Programming and Coding') 

rankedskills<-factor(rawdata[,15],levels=c('Very Unimportant','Somewhat Unimportant','Neither Important nor Unimportant','Somewhat Important','Very Important'))
barplot(prop.table(table(rankedskills)),col=topo.colors(length(unique(rankedskills))))
title(main='General Business Understanding') 

rankedskills<-factor(rawdata[,16],levels=c('Very Unimportant','Somewhat Unimportant','Neither Important nor Unimportant','Somewhat Important','Very Important'))
barplot(prop.table(table(rankedskills)),col=topo.colors(length(unique(rankedskills))))
title(main='Being able to communicate my results') 

word.cloud <- wordcloud(words=paste(rawdata[,17],collapse=''),scale=c(8,.2),min.freq=1,
                        max.words=Inf, random.order=FALSE, rot.per=.15, colors=brewer.pal(8,"Dark2"))
