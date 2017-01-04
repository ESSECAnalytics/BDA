###########################################################
#                  Big Data Analytics                     #
#               Session 8 - Text mining                   #
#                                                         #
#               Student survey analysis                   #
#               Urban Outfitters tweets                   #  
#           Sentiment analysis across brands              #
#                                                         #
# Authors: Nicolas Glady and Michael Surlereaux           #
###########################################################

###########################################################
# Disclaimer: this script is used to produce the examples #
#  presented during the course Big Data Analytics. The    #
#  authors are not responsible in any way for any problem #
#  encountered during this code execution.                #
###########################################################

#################################
#### Student survey analysis ####
#################################

# 1 Data extraction
library(openxlsx)
rawSurveydata <- read.xlsx("TQ_BigDataAnalyt.xlsx", sheet = "BigDataAnalyt", startRow = 1, colNames = TRUE)

View(Survey[,c(12:15,26)]) # Show the "open entry" results only
Texttomine<-paste(Survey[,12],Survey[,13],Survey[,14],Survey[,15],Survey[,26],sep=" ") # All the columns are concatenated

# 2 Data Visualization with a Wordcloud

library(wordcloud)
word.cloud <- wordcloud(words=paste(Texttomine,collapse=''),scale=c(8,.2),min.freq=1,
                        max.words=Inf, random.order=FALSE, rot.per=.15, colors=brewer.pal(8,"Dark2"))

# 3.1 Data transformation - Use of the tm package
library(tm)
MyCorpus <- Corpus(VectorSource(Texttomine))
inspect(MyCorpus) # Contains all the different n data entries (here 34) in a "Corpus" (a structure)
tdm <- TermDocumentMatrix(MyCorpus) # A TermDocumentMatrix is more easy to work with
inspect(tdm) # A matrix of n x number of words (here 34 x number of words) with the number of times the words appears in each entry
findFreqTerms(tdm, lowfreq=10) # Words appearing more than 10 times

# 3.2 Remove irrelavant data (data cleaning)
MyCorpus<-tm_map(MyCorpus,stripWhitespace)
MyCorpus<-tm_map(MyCorpus, removePunctuation)
myStopWords <-c(stopwords('english'),'also') # all the words that are irrelevant
MyCorpus<-tm_map(MyCorpus, removeWords,myStopWords) # Remove irrelevant words like (about, etc.)
tdmclean <- TermDocumentMatrix(MyCorpus) 

# 4 Data analysis
findFreqTerms(tdmclean, lowfreq=10) # Words appearing more than 10 times
findAssocs(tdmclean,"data",0.5) # Words correlated (more than 50%) with data
findAssocs(tdmclean,"business",0.7) # Words correlated (more than 70%) with business

# 5 Hierarchical cluster analysis
# 5.1 Remove sparse terms
tdm2 <- removeSparseTerms(tdmclean, sparse=0.8) # Let's trim the unfrequent (sparse) words
# 5.2 Compute the distance matrix
tdm2 <- as.matrix(tdm2)
distMatrix <- dist(scale(tdm2))
# 5.3 Operate the Hierarchical clustering
Survey.cluster <- hclust(distMatrix, method="ward.D")
# 5.4 Dendogram
plot(Survey.cluster, cex=0.9, hang=-1, main="Word Cluster Dendogram")

#################################
#### Urban Outfitters tweets ####
#################################
#load("Session8.Rdata")
# 1. Data identification
# Visit http://www.twitter.com/urbanoutfitters 

# 2.1 Data extraction (connection)
library(devtools)
install_github("twitteR", username="geoffjentry") # We use a workaround for the connection
library(twitteR)

api_key = "TO BE REPLACED"
api_secret = "TO BE REPLACED"
access_token = "TO BE REPLACED"
access_token_secret = "TO BE REPLACED"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# 2.1 Data extraction (parsing)
# Extract 1000 tweets from the Urban Outfitters timelines
UO.tweets <- userTimeline("@UrbanOutfitters", n=1000)
# Transform tweets into a data frame
tweets.df <- twListToDF(UO.tweets)

#save.image(file="Session8.RData")

# 3. Data pre-treatment
# 3.1. Use of the tm package
#load("Session8.RData")

# 3.1.1. Transformation to make it readable
# convert string to vector of words
dattemp <- unlist(strsplit(tweets.df$text, split=", "))
# remove usernames
datatemp<-gsub("@[[:alnum:]]*","",dattemp)
# to ASCII
datatemp <- iconv(datatemp, "latin1", "ASCII", sub="")
datatemp <- str_replace_all(datatemp,"[^[:graph:]]", " ") 
# remove punctuation
datatemp<-gsub("[[:punct:]]", "", datatemp)
# remove htpp
datatemp<-gsub("http[[:alnum:]]*","",datatemp)
# remove numbers 
datatemp<-gsub("\\d", "",datatemp)
# remove "stop words"
myStopWords <-c(stopwords('english'))
datatemp<-removeWords(datatemp,myStopWords)
# Strip whitespace
datatemp<-stripWhitespace(datatemp)
# to lowercase
datatemp <-tolower(datatemp)
# 3.2. Use of the snowballC package - stemming of the strings (i.e. truncation to stem)
# Creation of a "dictionnary" from all the words
#myCorpusdict<-rownames(TermDocumentMatrix(Corpus(VectorSource(datatemp))))
datacomplete<-datatemp # Will be used to store the stemmed result
library(SnowballC)
k<-length(datatemp)
for (i in 1:k) {
  temp<-strsplit(datatemp[[i]]," ") # Selection of the tweet i
  temp<-stemDocument(temp[[1]]) # Stemming of the tweet i
  #temp<-stemCompletion(temp,dictionary=myCorpusdict)
  datacomplete[[i]]<-paste(temp,collapse=" ") # Form it as a sentence
}
myCorpusComplete<-Corpus(VectorSource(datacomplete))
#View(dattemp)
#View(datacomplete)

# 3.3. Transform the corpus into a Term-Document Matrix 
tweets.tm <- TermDocumentMatrix(myCorpusComplete)
#inspect(tweets.tm[1:10,1:10])

#4.0 Wordcloud associated to the UO tweets
word.cloud <- wordcloud(words=paste(datacomplete,collapse=''),scale=c(8,.2),max.words=100, random.order=FALSE, rot.per=.15, colors=brewer.pal(8,"Dark2"))

#4.1.1 Frequent terms analysis
findFreqTerms(tweets.tm, lowfreq=25)
#4.1.2 Association analysis
# Find the correlation around the term "omg"
findAssocs(tweets.tm, "omg", 0.2)
# Find the correlation around the term "love"
findAssocs(tweets.tm, "love", 0.2)
#4.2 Hierarchical cluster analysis preparation
# Remove sparse terms
tweets.tm2 <- removeSparseTerms(tweets.tm, sparse=0.98)
tweets.tm2 <- as.matrix(tweets.tm2)
# Compute the distance matrix
distMatrix <- dist(scale(tweets.tm2))
# Operate the Hierarchical clustering
UO.cluster <- hclust(distMatrix, method="ward.D")

#4.2 Hierarchical cluster analysis output
plot(UO.cluster, cex=0.9, hang=-1, main="Word Cluster Dendogram")
# Cut the tree into 5 clusters
rect.hclust(UO.cluster, k=5)

View(tweets.df[,c(3,5,12,15:16)]) 

#4.3. Temporal analysis
library(ggplot2)

#4.3.1 Plot the number of tweets created by day
ggplot(tweets.df,aes(x=created))+geom_bar(aes(y = (..count..)))

#4.3.2 Plot the number of retweets per day of the week
#First, label your tweets with hour and weekday number
#label a tweet with the hour
tweets.df$hour=sapply(tweets.df$created, function(x) {p=as.POSIXlt(x);p$hour})
#label a tweet with a number corresponding to the day of the week
tweets.df$wday=sapply(tweets.df$created, function(x) {p=as.POSIXlt(x);p$wday})

ggplot(tweets.df,aes(x=wday))+geom_bar(aes(x=wday, y=retweetCount), stat="identity")

#4.3.3 Plot Day/Hour
ggplot(tweets.df)+geom_jitter(aes(x=wday,y=hour))

##########################################
#### Sentiment Analysis across brands ####
##########################################

# 1. Data identification
# Visit http://www.twitter.com/urbanoutfitters 
# Visit http://www.twitter.com/Abercrombie
# Visit http://www.twitter.com/Forever21

# 2.1 Data extraction (connection)
# Load the twitteR, tm, ggplot2, wordcloud and snowballC packages

# 2.1 Data extraction (parsing)
# Extract 1000 tweets from Comcast and Forever 21 timelines
CC.tweets=searchTwitter('@comcast', n=1000)
F21.tweets=searchTwitter('@Forever21', n=1000)

# 2.2 Extract text from lexicons
pos.words = scan('positive-words.txt',what='character', comment.char=';')
neg.words = scan('negative-words.txt',what='character', comment.char=';')

# 3.1.2 Write in function to score sentiment
library(plyr)

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{ # function to score the sentiments
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use 
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

clean.tweets <- function(tweets.df){ # Function to clean the data
  twlist<-twListToDF(tweets.df)
  dattemp <- unlist(strsplit(twlist$text, split=", "))
  # remove usernames
  datatemp<-gsub("@[[:alnum:]]*","",dattemp)
  # to ASCII
  datatemp <- iconv(datatemp, "latin1", "ASCII", sub="")
  datatemp <- str_replace_all(datatemp,"[^[:graph:]]", " ") 
  # remove punctuation
  datatemp<-gsub("[[:punct:]]", "", datatemp)
  # remove htpp
  datatemp<-gsub("http[[:alnum:]]*","",datatemp)
  # remove numbers 
  datatemp<-gsub("\\d", "",datatemp)
  # remove unrecognized chars
  datatemp<-gsub("�", "",datatemp)
  # remove "stop words"
  myStopWords <-c(stopwords('english'))
  datatemp<-removeWords(datatemp,myStopWords)
  # Strip whitespace
  datatemp<-stripWhitespace(datatemp)
  # to lowercase
  datatemp <-tolower(datatemp)
  return(datatemp)
}
# 4.1 Score tweets' sentiment
UO.score=score.sentiment(clean.tweets(UO.tweets), pos.words, neg.words, .progress='text')
CC.score=score.sentiment(clean.tweets(CC.tweets), pos.words, neg.words, .progress='text')
F21.score=score.sentiment(clean.tweets(F21.tweets), pos.words, neg.words, .progress='text')
# 4.2 Configure colums for further plotting
UO.score$brand="Urban Outfitters"
UO.score$code="UO"
CC.score$brand="Comcast"
CC.score$code="CC"
F21.score$brand="Forever 21"
F21.score$code="F21"
# 4.3 Bind scores for brands 
brands.score=rbind(UO.score, CC.score, F21.score)

# 5 Data visualization
# plot of the score by brand
g = ggplot(data=brands.score, mapping=aes(x=score, fill=brand) )
g = g + geom_bar(binwidth=1) # Do a histogram
g = g + facet_grid(brand~.) # Have a different plot for each brand
g = g + theme_bw() + scale_fill_brewer() # Define the colors (blue in a b&w theme)
g

#########################
#### GeoLocalisation ####
#########################
library(maps)

# 1 Data preparation
france<-map(database="france")
departements <- match.map(france,france$names) # Select only the departments index
radius<-'40km' # gross estimate of the average radius of a departement

#create a string of the lat, long, and radius for entry into searchTwitter()
for(i in 1:length(departements)){
  france$search.twitter.entry[departements[i]]<-toString(c(france$y[departements[i]],france$x[departements[i]],radius))
}
# take out spaces in the string
france$search.twitter.entry<-gsub(" ","", france$search.twitter.entry ,fixed=TRUE)
  
# 2 Data extraction
#Search twitter at each location, check how many tweets and put into dataframe
searchtext<-'@fhollande'

maxtweets<-200
for(i in 1:length(departements)){
  if(!is.na(france$y[departements[i]])) {
  france$number.of.tweets[departements[i]]<-
      length(searchTwitter(searchString=searchtext,n=maxtweets,geocode=france$search.twitter.entry[departements[i]]))
  }}

france$number.of.tweets[is.na(france$number.of.tweets)]<-0

# 3 Data visualization
gray.colors<-function(n) gray(rev(0:(n-1)/1.5)/n)
colors<-gray.colors(maxtweets)[france$number.of.tweets[departements]]

#making the map of France
map(database="france", fill=TRUE,col=colors,resolution=0)

save.image(file="Session8.Rdata")
