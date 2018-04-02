#install packages
install.packages("dplyr")
library("dplyr")
install.packages("data.table")
library("data.table")
install.packages("ggplot2")
library("ggplot2")
install.packages("tm")
library('tm')
install.packages("wordcloud")
library('wordcloud')


#read in the data
train <- as.data.frame(fread("train.tsv"))
attach(train)


#############################################################price
#boxpplot of price 
boxplot(train$price)


#histogram of price without log transformation to show the large picture
ggplot(data=train,aes(price))+
  geom_histogram(binwidth=10,fill="steelblue",col="black")+
  ggtitle("Distribution of price")


#histogram of log(price+1) using ggplot
p = log(price+1)                 #use price+1 to avoid zero values 
ggplot(data=train,aes(log(price+1)))+
  geom_histogram(binwidth=0.2,fill="steelblue",col="black")+
  ggtitle("Distribution of log(price+1)")


#ks test for normality 
ks.test((train$price - mean(train$price))/sd(train$price), pnorm)

#############################################################Shipping type 
#get the number of shippings paid by sellers 
len_1 = length(which(train['shipping']==1))
#get the number of shippings paid by users
len_2 = length(which(train['shipping']==0))
#calculate the percentage 
per_1 = len_1/nrow(train)              #percentage of shippings paid by sellers is 0.45
per_2 = len_2/nrow(train)              #percentage of shippings paid by users is 0.55

#create a new dataframe to store shpping paid by sellers
train_1 <- train[which(train['shipping']==1),]
#create a new dataframe to store shpping paid by users
train_0 <- train[which(train['shipping']==0),]

#distribution of log(price+1) of shipping paid by sellers and by users
p_1 = log(train_1$price)
p_0 = log(train_0$price)
hist(p_1,col = rgb(1,0,0,0.5),breaks=30,xlab="log(price+1)",main="Distribution of price by shipping")
hist(p_0,col = rgb(0,0,1,0.5),breaks=30,add=TRUE)


#average price for different shipping type 
m1 = mean(train_1$price)
m0 = mean(train_0$price)



#shipping barplots 
train %>% ggplot(aes(x=train$shipping))+
  geom_bar(width=0.5)


#boxplot of shipping versus log(price+1)
train %>% ggplot(aes(x=shipping,y=log(price+1),fill=shipping,group=shipping))+
  geom_boxplot(width=0.5)+
  ggtitle("Boxplot of log(price+1) versus shipping")+
  theme(legend.position="none")



#############################################################brand
#summary of the brand name 
#median price from largest to lowest of each brand
a <- train %>%
  group_by(brand_name) %>%
  summarise(median=median(price),n=n())

md_brand <- a[order(desc(a$median)),]

#the top 20 most expensive brands by median price,cutoff number of sales=100
md_brand[which(md_brand$n>=100),] %>%
  head(20) %>%
  ggplot(aes(x = median, y = reorder(brand_name,median))) + 
  geom_point(color = 'red') + 
  scale_x_continuous(labels = scales::dollar) + 
  labs(x = "median", y = 'Brand name', title = 'Top 20 most expensive brands') 


#to search a certain brand's median price
md_brand[which(md_brand$brand_name == "Air Jordan"),]



##################################################################category 
#median price from largest to lowest of each brand by cat2
#add levels of category to the original 'train' dataframe
sp = data.frame(train, str_split_fixed(train$category_name, '/', 4)) %>%
  mutate(cat1=X1, cat2=X2, cat3=X3, cat4=X4) %>% select(-X1, -X2, -X3, -X4)

a <- sp %>%
  group_by(brand_name,cat2) %>%
  summarise(median=median(price),n=n())

md_brand_2 <- a[order(desc(a$median)),]

#look up Air Jordan's information
a <- filter(md_brand_2,brand_name == "Air Jordan")


#cat1 summary 
cat1_sum <- as.data.frame(summary(sp$cat1))

#cat2 summary 
cat2_sum <- as.data.frame(summary(sp$cat2))

#cat3 summary
cat3_sum <- as.data.frame(summary(sp$cat3))

#cat4 summary
cat4_sum <- as.data.frame(summary(sp$cat4))


#Number of cat1,cat2,cat3,cat4
num_cat1=length(unique(sp$cat1))
num_cat2=length(unique(sp$cat2))
num_cat3=length(unique(sp$cat3))
num_cat4=length(unique(sp$cat4))


##cat1
#create the data frame for sales in cat1 
Category1 <- c("No Label","Beauty","Electronics","Handmade","Home","Kids","Men",
               "Other","Sports & Outdoors","Vintage & Collectibles","Women")
Sales1 <- c()
for(i in 1:nrow(cat1_sum)){
  Sales1[i] = cat1_sum[i,1]
}

cat1 <- data.frame(Category1,Sales1)

#histogram for cat1
ggplot(data=cat1, aes(x=reorder(Category1,Sales1), y=Sales1)) +
  geom_bar(stat="identity")+
  scale_x_discrete(name="Category")+
  ggtitle("Category level 1")


##cat2 
#create the data frame for top10 sales in cat2
Category2 <- c("Athletic Apparel","Makeup","Tops & Blouses","Shoes","Jewelry","Toys",
               "Cell Phones & Accessories","Women's Handbags","Dresses","Women's Accessories")
Sales2 <- c()
for(i in 1:nrow(cat2_sum)){
  Sales2[i] = cat2_sum[i,1]
}

cat2 <- data.frame(Category2,Sales2)

#histogram for cat2
ggplot(data=cat2,aes(x=reorder(Category2,Sales2),y=Sales2))+
  geom_bar(stat="identity")+
  scale_x_discrete(name="Category")+
  ggtitle("Category level 2")



##cat3
#create the data frame for top10 sales in cat3
Category3 <- c("Pants, Tights, Leggings","Other","Face","T-Shirts","Shoes",
               "Games","Lips","Athletic","Eyes","Cases, Covers & Skins")
Sales3 <- c()
for(i in 1:nrow(cat3_sum)){
  Sales3[i] = cat3_sum[i,1]
}

cat3 <- data.frame(Category3,Sales3)

#histogram for cat3
ggplot(data=cat3,aes(x=reorder(Category3,Sales3),y=Sales3))+
  geom_bar(stat="identity")+
  scale_x_discrete(name="Category")+
  ggtitle("Category level 3")



##cat4
#create the data frame for cat4
Category4 <- c("No brand","Ballet","Baseball","Bomber","Outdoor Games","Serving","eBook Access",
               "eBook Readers")

Sales4 <- c()
for(i in 1:nrow(cat4_sum)){
  Sales4[i] = cat4_sum[i,1]
}

cat4 <- data.frame(Category4,Sales4)



#histogram for cat4
ggplot(data=cat4, aes(x=reorder(Category4,Sales4), y=Sales4)) +
  geom_bar(stat="identity")+
  scale_x_discrete(name="Category")+
  ggtitle("Category level 4")








##########################################################Wordcloud
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








##2nd and 3rd category level under women 
#prepare the dataframe and combine 2nd category and 3rd category
w <- sp %>% filter(sp$cat1 == "Women") 
w$cat2_3 <- paste(w$cat2,w$cat3)
##word cloud for 2nd category level under women 
corpus <- Corpus(VectorSource(w$cat2))
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




########################################################item condition
#barplot of item condition
train %>% ggplot(aes(x=item_condition_id,fill=item_condition_id,group=item_condition_id))+
  geom_bar()+
  ggtitle("Barplot of item condition")+
  theme(legend.position="none")



#boxplot of item condition versus log(price+1)
train %>% ggplot(aes(x=item_condition_id,y=log(price+1),fill=item_condition_id,group=item_condition_id))+
  geom_boxplot(width=0.5)+
  ggtitle("Boxplot of item condition versus log(price+1)")+
  theme(legend.position="none")



######################################################has brand or not 
sp <- mutate(sp,has_brand=ifelse(brand_name != "",TRUE,FALSE))


#histogram of has_brand
sp %>% ggplot(aes(x=has_brand,fill=has_brand))+
  geom_bar(width=0.5)+
  ggtitle("Barplot of having brand or not")


#boxplot of has_brand and log(price+1)
sp %>% ggplot(aes(x=has_brand,y=log(price+1),fill=has_brand))+
  geom_boxplot()+
  ggtitle("Boxplot of has_brand versus log(price+1)")


#1st category has brand or not 
sp %>% ggplot(aes(x=cat1,y=log(price+1)))+
  geom_boxplot(aes(fill=has_brand))+
  ggtitle("Boxplot of cat1 versus log(price+1)")



#has_brand percentage
len_b = length(which(sp['has_brand']==TRUE))
per_b = len_b/nrow(sp)
len_nb = length(which(sp['has_brand']==FALSE))
per_nb = len_nb/nrow(sp)



#has_brand median price 
brand <- sp[which(sp['has_brand'] == TRUE),]
md_b = median(brand$price)
no_brand <- sp[which(sp['has_brand'] == FALSE),]
md_nb = median(no_brand$price)



#wordcloud for name 
##word cloud for item name 
corpus <- Corpus(VectorSource(sp$name))
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




