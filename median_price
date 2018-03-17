#install packages
install.packages("dplyr")
library("dplyr")
install.packages("data.table")
library("data.table")

#read in the data
train <- as.data.frame(fread("train.tsv"))
attach(train)

#######################################brand
#summary of the brand name 
train_brandname <- as.data.frame(table(brand_name))

#median price from largest to lowest of each brand
a <- train %>%
  group_by(brand_name) %>%
   summarise(median=median(price),n=n())

md_brand <- a[order(desc(a$median)),]

#to search a certain brand's median price
md_brand[which(b$brand_name == "UGG"),]
