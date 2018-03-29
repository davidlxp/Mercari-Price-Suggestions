#install packages 
install.packages("tm")
library('tm')
install.packages("wordcloud")
library('wordcloud')
install.packages("data.table")
library("data.table")
install.packages("dplyr")
library("dplyr")

#read in the data 
train <- as.data.frame(fread("train.tsv"))
attach(train)

##word cloud for item_description
corpus <- Corpus(VectorSource(item_description))
#check the first comment 
corpus[[1]][1]

#Text cleaning
#convert the text to lower case 
corpus <- tm_map(corpus,content_transformer(tolower))
#Remove numbers 
corpus <- tm_map(corpus,removeNumbers)
#Remove common stopwords
corpus <- tm_map(corpus,removeWords,c("this","the","is","in","i","they","no","you","and","for","with",
                                      "but","are","not","have","from","please","have","can","all","each",
                                      "will","been","just","your","that","yet","has","any","these"))
#Remove punctuations
corpus <- tm_map(corpus,removePunctuation)
#Eliminate extra white space
corpus <- tm_map(corpus,stripWhitespace)

#Build wordcloud
wordcloud(corpus,random.order=F,scale=c(2,0.5),max.words = 40,color=rainbow(100))



##item_description for category 1
#Women
Women_item_description <- item_description[which(sp$cat1=="Women")]
cat1_Women_item_description <- data.frame(Women_item_description)

##word cloud for Women_item_description
corpus <- Corpus(VectorSource(cat1_Women_item_description$Women_item_description))
#check the first comment 
corpus[[1]][1]

#Text cleaning
#convert the text to lower case 
corpus <- tm_map(corpus,content_transformer(tolower))
#Remove numbers 
corpus <- tm_map(corpus,removeNumbers)
#Remove common stopwords
corpus <- tm_map(corpus,removeWords,c("this","the","is","in","i","they","no","you","and","for","with",
                                      "but","are","not","have","from","please","have","can","all","each",
                                      "will","been","just","your","that"))
#Remove punctuations
corpus <- tm_map(corpus,removePunctuation)
#Eliminate extra white space
corpus <- tm_map(corpus,stripWhitespace)

#Build wordcloud
wordcloud(corpus,random.order=F,scale=c(2,0.5),max.words = 40,color=rainbow(100))





#Beauty
Beauty_item_description <- item_description[which(sp$cat1=="Beauty")]
cat1_Beauty_item_description <- data.frame(Beauty_item_description)

##word cloud for Beauty_item_description
corpus <- Corpus(VectorSource(cat1_Beauty_item_description$Beauty_item_description))
#check the first comment 
corpus[[1]][1]

#Text cleaning
#convert the text to lower case 
corpus <- tm_map(corpus,content_transformer(tolower))
#Remove numbers 
corpus <- tm_map(corpus,removeNumbers)
#Remove common stopwords
corpus <- tm_map(corpus,removeWords,c("this","the","is","in","i","they","no","you","and","for","with",
                                      "but","are","not","have","from","please","have","can","all","each",
                                      "will","been","just","your","that"))
#Remove punctuations
corpus <- tm_map(corpus,removePunctuation)
#Eliminate extra white space
corpus <- tm_map(corpus,stripWhitespace)

#Build wordcloud
wordcloud(corpus,random.order=F,scale=c(2,0.5),max.words = 40,color=rainbow(100))



#Kids
Kids_item_description <- item_description[which(sp$cat1=="Kids")]
cat1_Kids_item_description <- data.frame(Kids_item_description)

##word cloud for Kids_item_description
corpus <- Corpus(VectorSource(cat1_Kids_item_description$Kids_item_description))
#check the first comment 
corpus[[1]][1]

#Text cleaning
#convert the text to lower case 
corpus <- tm_map(corpus,content_transformer(tolower))
#Remove numbers 
corpus <- tm_map(corpus,removeNumbers)
#Remove common stopwords
corpus <- tm_map(corpus,removeWords,c("this","the","is","in","i","they","no","you","and","for","with",
                                      "but","are","not","have","from","please","have","can","all","each",
                                      "will","been","just","your","that"))
#Remove punctuations
corpus <- tm_map(corpus,removePunctuation)
#Eliminate extra white space
corpus <- tm_map(corpus,stripWhitespace)

#Build wordcloud
wordcloud(corpus,random.order=F,scale=c(2,0.5),max.words = 40,color=rainbow(100))



#Electronics
Electronics_item_description <- item_description[which(sp$cat1=="Electronics")]
cat1_Electronics_item_description <- data.frame(Electronics_item_description)

##word cloud for Electronics_item_description
corpus <- Corpus(VectorSource(cat1_Electronics_item_description$Electronics_item_description))
#check the first comment 
corpus[[1]][1]

#Text cleaning
#convert the text to lower case 
corpus <- tm_map(corpus,content_transformer(tolower))
#Remove numbers 
corpus <- tm_map(corpus,removeNumbers)
#Remove common stopwords
corpus <- tm_map(corpus,removeWords,c("this","the","is","in","i","they","no","you","and","for","with",
                                      "but","are","not","have","from","please","have","can","all","each",
                                      "will","been","just","your","that","has"))
#Remove punctuations
corpus <- tm_map(corpus,removePunctuation)
#Eliminate extra white space
corpus <- tm_map(corpus,stripWhitespace)

#Build wordcloud
wordcloud(corpus,random.order=F,scale=c(2,0.5),max.words = 40,color=rainbow(100))




#Men
Men_item_description <- item_description[which(sp$cat1=="Men")]
cat1_Men_item_description <- data.frame(Men_item_description)

##word cloud for Men_item_description
corpus <- Corpus(VectorSource(cat1_Men_item_description$Men_item_description))
#check the first comment 
corpus[[1]][1]

#Text cleaning
#convert the text to lower case 
corpus <- tm_map(corpus,content_transformer(tolower))
#Remove numbers 
corpus <- tm_map(corpus,removeNumbers)
#Remove common stopwords
corpus <- tm_map(corpus,removeWords,c("this","the","is","in","i","they","no","you","and","for","with",
                                      "but","are","not","have","from","please","have","can","all","each",
                                      "will","been","just","your","that","has"))
#Remove punctuations
corpus <- tm_map(corpus,removePunctuation)
#Eliminate extra white space
corpus <- tm_map(corpus,stripWhitespace)

#Build wordcloud
wordcloud(corpus,random.order=F,scale=c(2,0.5),max.words = 40,color=rainbow(100))



#Home
Home_item_description <- item_description[which(sp$cat1=="Home")]
cat1_Home_item_description <- data.frame(Home_item_description)

##word cloud for Home_item_description
corpus <- Corpus(VectorSource(cat1_Home_item_description$Home_item_description))
#check the first comment 
corpus[[1]][1]

#Text cleaning
#convert the text to lower case 
corpus <- tm_map(corpus,content_transformer(tolower))
#Remove numbers 
corpus <- tm_map(corpus,removeNumbers)
#Remove common stopwords
corpus <- tm_map(corpus,removeWords,c("this","the","is","in","i","they","no","you","and","for","with",
                                      "but","are","not","have","from","please","have","can","all","each",
                                      "will","been","just","your","that","has","yet","these"))
#Remove punctuations
corpus <- tm_map(corpus,removePunctuation)
#Eliminate extra white space
corpus <- tm_map(corpus,stripWhitespace)

#Build wordcloud
wordcloud(corpus,random.order=F,scale=c(2,0.5),max.words = 40,color=rainbow(100))



#Vintage&Collectibles
VC_item_description <- item_description[which(sp$cat1=="Vintage & Collectibles")]
cat1_VC_item_description <- data.frame(VC_item_description)

##word cloud for VC_item_description
corpus <- Corpus(VectorSource(cat1_VC_item_description$VC_item_description))
#check the first comment 
corpus[[1]][1]

#Text cleaning
#convert the text to lower case 
corpus <- tm_map(corpus,content_transformer(tolower))
#Remove numbers 
corpus <- tm_map(corpus,removeNumbers)
#Remove common stopwords
corpus <- tm_map(corpus,removeWords,c("this","the","is","in","i","they","no","you","and","for","with",
                                      "but","are","not","have","from","please","have","can","all","each",
                                      "will","been","just","your","that","has","yet","these"))
#Remove punctuations
corpus <- tm_map(corpus,removePunctuation)
#Eliminate extra white space
corpus <- tm_map(corpus,stripWhitespace)

#Build wordcloud
wordcloud(corpus,random.order=F,scale=c(2,0.5),max.words = 40,color=rainbow(100))




#other
Other_item_description <- item_description[which(sp$cat1=="Other")]
cat1_Other_item_description <- data.frame(Other_item_description)

##word cloud for Other_item_description
corpus <- Corpus(VectorSource(cat1_Other_item_description$Other_item_description))
#check the first comment 
corpus[[1]][1]

#Text cleaning
#convert the text to lower case 
corpus <- tm_map(corpus,content_transformer(tolower))
#Remove numbers 
corpus <- tm_map(corpus,removeNumbers)
#Remove common stopwords
corpus <- tm_map(corpus,removeWords,c("this","the","is","in","i","they","no","you","and","for","with",
                                      "but","are","not","have","from","please","have","can","all","each",
                                      "will","been","just","your","that","has","yet","these"))
#Remove punctuations
corpus <- tm_map(corpus,removePunctuation)
#Eliminate extra white space
corpus <- tm_map(corpus,stripWhitespace)

#Build wordcloud
wordcloud(corpus,random.order=F,scale=c(2,0.5),max.words = 40,color=rainbow(100))




#Handmade
Handmade_item_description <- item_description[which(sp$cat1=="Handmade")]
cat1_Handmade_item_description <- data.frame(Handmade_item_description)

##word cloud for Handmade_item_description
corpus <- Corpus(VectorSource(cat1_Handmade_item_description$Handmade_item_description))
#check the first comment 
corpus[[1]][1]

#Text cleaning
#convert the text to lower case 
corpus <- tm_map(corpus,content_transformer(tolower))
#Remove numbers 
corpus <- tm_map(corpus,removeNumbers)
#Remove common stopwords
corpus <- tm_map(corpus,removeWords,c("this","the","is","in","i","they","no","you","and","for","with",
                                      "but","are","not","have","from","please","have","can","all","each",
                                      "will","been","just","your","that","has","yet","these"))
#Remove punctuations
corpus <- tm_map(corpus,removePunctuation)
#Eliminate extra white space
corpus <- tm_map(corpus,stripWhitespace)

#Build wordcloud
wordcloud(corpus,random.order=F,scale=c(2,0.5),max.words = 40,color=rainbow(100))



#Sports & Outdoors
SO_item_description <- item_description[which(sp$cat1=="Sports & Outdoors")]
cat1_SO_item_description <- data.frame(SO_item_description)

##word cloud for SO_item_description
corpus <- Corpus(VectorSource(cat1_SO_item_description$SO_item_description))
#check the first comment 
corpus[[1]][1]

#Text cleaning
#convert the text to lower case 
corpus <- tm_map(corpus,content_transformer(tolower))
#Remove numbers 
corpus <- tm_map(corpus,removeNumbers)
#Remove common stopwords
corpus <- tm_map(corpus,removeWords,c("this","the","is","in","i","they","no","you","and","for","with",
                                      "but","are","not","have","from","please","have","can","all","each",
                                      "will","been","just","your","that","has","yet","these"))
#Remove punctuations
corpus <- tm_map(corpus,removePunctuation)
#Eliminate extra white space
corpus <- tm_map(corpus,stripWhitespace)

#Build wordcloud
wordcloud(corpus,random.order=F,scale=c(2,0.5),max.words = 40,color=rainbow(100))




#No Label
NL_item_description <- item_description[which(sp$cat1=="")]
cat1_NL_item_description <- data.frame(NL_item_description)

##word cloud for NL_item_description
corpus <- Corpus(VectorSource(cat1_NL_item_description$NL_item_description))
#check the first comment 
corpus[[1]][1]

#Text cleaning
#convert the text to lower case 
corpus <- tm_map(corpus,content_transformer(tolower))
#Remove numbers 
corpus <- tm_map(corpus,removeNumbers)
#Remove common stopwords
corpus <- tm_map(corpus,removeWords,c("this","the","is","in","i","they","no","you","and","for","with",
                                      "but","are","not","have","from","please","have","can","all","each",
                                      "will","been","just","your","that","has","yet","these"))
#Remove punctuations
corpus <- tm_map(corpus,removePunctuation)
#Eliminate extra white space
corpus <- tm_map(corpus,stripWhitespace)

#Build wordcloud
wordcloud(corpus,random.order=F,scale=c(2,0.5),max.words = 40,color=rainbow(100))




