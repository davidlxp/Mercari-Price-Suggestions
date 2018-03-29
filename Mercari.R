#install packages
install.packages("dplyr")
library("dplyr")
install.packages("data.table")
library("data.table")
install.packages("ggplot2")
library("ggplot2")


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
ggplot(data=train,aes(p))+
  geom_histogram(binwidth=0.2,fill="steelblue",col="black")+
  ggtitle("Distribution of log(price+1)")



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










######################################################Experiment
library(data.table)
library(magrittr)
install.packages("ggplot2")
library(ggplot2)
library(scales)
library(stringr)
install.packages("quanteda")
library(quanteda)
library(gridExtra)


md_brand %>%
  head(25) %>%
  ggplot(aes(x = reorder(md_brand$brand_name, md_brand$median), y = md_brand$median)) + 
  geom_point(color = 'cyan2') + 
  scale_y_continuous(labels = scales::dollar) + 
  coord_flip() +
  labs(x = '', y = 'Median price', title = 'Top 25 most expensive brands') 


ggplot(data = train, aes(x = log(price+1))) + 
  geom_histogram(fill = 'orangered2') +
  labs(title = 'Histogram of log item price + 1')


ggplot(data = train, aes(x = as.factor(item_condition_id), y = log(price + 1))) + 
  geom_boxplot(fill = 'cyan2', color = 'darkgrey')




#split the category_name
library(stringr)
install.packages("treemapify")
library(treemapify)

sp = data.frame(train, str_split_fixed(train$category_name, '/', 4)) %>%
  mutate(cat1=X1, cat2=X2, cat3=X3, cat4=X4) %>% select(-X1, -X2, -X3, -X4)

sp %>% summarise(Num_Cat1 = length(unique(cat1)), Num_Cat2 = length(unique(cat2)),
                    Num_Cat3 = length(unique(cat3)), Num_Cat4 = length(unique(cat4)))


options(repr.plot.width=7, repr.plot.height=7)

sp %>%
  group_by(cat1, cat2) %>%
  count() %>%
  ungroup() %>%
  ggplot(aes(area=n, fill=cat1, label=cat2, subgroup=cat1)) +
  geom_treemap() +
  geom_treemap_subgroup_text(grow = T, alpha = 0.5, colour =
                               "black", fontface = "italic", min.size = 0) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T) +
  theme(legend.position = "null") +
  ggtitle("1st and 2nd Hierarchical Category Levels")







