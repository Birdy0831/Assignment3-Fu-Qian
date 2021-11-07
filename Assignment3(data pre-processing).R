#Read the file
mydata<-read.csv('/Users/mac/Desktop/R/Assignment3-Fu-Qian/food-price-index-September-2021-index-numbers-csv-tables.csv')
#Check the data with the first 6 rows
head(mydata)
#Check the dimensionality of data
dim(mydata)
#Check the structure of data
str(mydata)
#Obtain descriptive statistics
summary(mydata)

#Check for missing values,whether the values corresponding to indicators have NA or not.
colSums(is.na(mydata))

#According to the results,Data_value has NA,the number of NA is 82
#Therefore,remove the rows which contain NA value
good<-complete.cases(mydata)
mydata_clean<-mydata[good,]

#Check the quantiles of quantitative variables
quantile(mydata_clean$Data_value)


#Because the dataset is large, table()function is used to check some variables which I am not sure 
#whether variables contain the same value.The below STATUS,UNITS,Subjects and Group,have same values

#convert dataframe to table
library(data.table)
tmydata_clean=data.table(mydata_clean)
#check how many evaluation/data exploration
tmydata_clean[,table(Series_reference)]
tmydata_clean[,table(STATUS)]
tmydata_clean[,table(UNITS)]
tmydata_clean[,table(Subject)]
tmydata_clean[,table(Group)]
tmydata_clean[,table(Series_title_1)]
#check how many 'Oranges, 1kg' in the column Series_title_1
table(data1$Series_title_1 %in% c('Oranges, 1kg'))


#Just remove the status,units,subjects and groups which remain the same property.
data1<-mydata_clean[,c(2,1,8,3)]

library(plyr)
arrange(data1,Data_value)   #Sort data by Data_value

#Cut produced factor varibles
library(Hmisc)
data1$Groups=cut2(data1$Data_value,g=4)
table(data1$Groups)

#Cast data frames
#extract main variables into a new data frame
data2<-data1[,c(2:4)]

#Format the data，change the long data to wide data
library(reshape2)
newdata<-dcast(data2,Series_title_1~Data_value)

#Group the products with series reference to check whether each reference correspond to only one product
sp<-split(data1$Series_title_1,data1$Series_reference)
#4 different funtions to conduct group statistics
spvalue=split(data1$Data_value,data1$Series_title_1)
###1.lapply
lapply(spvalue,mean)
###2.tapply
tapply(data1$Data_value,data1$Series_title_1,mean)
###3.ddply
library(plyr)
data3<-data2[,c(2:3)]
ddply(data3,.(Series_title_1),summarize,mean=mean(Data_value))
###4.sapply
#Convert Series_title_1 into factor
data1$factor<-factor(data1$Series_title_1)
dat<-sapply(split(data1$Data_value,data1$factor),mean)
dat

#Draw bar graphs on commodities and average prices
newdata1<-data.frame(dat)
#Convert row names to column
name <- rownames(newdata1)
#bind the 'name' and 'dat'
cbind(name,newdata1$dat)
rownames(newdata1) <- NULL
newdata2 <- cbind(name,newdata1)
library(ggplot2)
p<-ggplot(data=newdata2, aes(x=name, y=dat))+geom_bar(stat="identity",fill = 'blue',width = 0.9)+
  theme(panel.background=element_rect(fill='transparent',
                                      color ="gray"), 
        axis.text.x = element_text(angle = 90, hjust = 0.5, 
                                   vjust = 0.5,color = "black",size=6))
 

p
