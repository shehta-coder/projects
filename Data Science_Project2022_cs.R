#Review of Big Data Analytic Methods::
#Step1:
#set working directory 
setwd("F:/Data Science project 2022")
zeta <- read.csv(file = 'zeta.csv')
#make summary of data set
summary(zeta) 
#number of rows of data are there in the zeta table
dim(zeta)
#create data_frame 
zeta_table<-(zeta)
zeta_table
#take non duplicated data from zeta_table in new table
zeta_nodupes<-(unique(zeta_table))
dim(zeta_nodupes)
#save non duplicated data in CSV_file
write.csv(zeta_nodupes,"F:\Data Science project 2022\zeta_nodupes.csv", row.names = FALSE)
#______
#Step2:

#load zip_income
data <- zipIncome ###data <- read.table('zipIncome.txt',sep=',',header=TRUE)
data
#change name of columns
colnames(data)<-c("zipCode","income")
dim(data)
#make summary of data set
summary(data)
#make plot for data 
plot(data$income,data$zipCode,main = "R Scatter Plot",xlab = "income",ylab = "zip Code",las = 1)
boxplot(data$income,data$zipCode,main = "Box Plot",xlab = "income",ylab = "zip Code",las = 1)
#take subset of data 
sub1<-subset(data,data$income >7000 & data$income < 200000)
sub1
#make summary of subset
summary(sub1)
#take mean of subset
#______
#step3:
#make box-plot for data
boxplot(data$income ~as.factor(data$zipCode),main ="R boxplot1",xlab="zipCode",ylab="Income")
new_data<-log10(data)
boxplot(new_data$income ~as.factor(new_data$zipCode),main ="R boxplot2",xlab="income",ylab="zipCode")
#------------------------------------------------------------------------
#second
#Access data_set and make new table
income_elec_state <- read.csv(file = 'income_elec_state.csv')
new_table<-(income_elec_state)
colnames(new_table)<-c("state","mean household income","mean electricity usage")
new_table
#drop first column to apply k_mean algorithm 
table_drob<-new_table[c(2:3)]
#fit the K_mean cluster with 10 initial  cluster centers
first_km <- kmeans(table_drob,10,10)
first_km
#make plot for new table 
plot(table_drob,col=first_km$cluster)
#find best value for k
points(first_km$centers,col=1:10,pch=8)
wss<- numeric(15)
for(i in 1:15)wss[i]<-sum(kmeans(table_drob,centers=i)$withinss)
plot(1:15,wss,type = "b",xlab="number of clusters",ylab = "within groups sum of squares")
#---------------------------------------------------------------------------------------------
#scale the data using log10
data_frame_scale<-log10(table_drob)
data_frame_scale
#fit the K_mean cluster with 4 initial  cluster centers for new data
second_km <- kmeans(data_frame_scale,4,10)
second_km
#make plot for scaled data 
plot(data_frame_scale,col=second_km$cluster)
#find best value for k
points(second_km$centers,col=1:4,pch=8)
wss<- numeric(15)
for(i in 1:15)wss[i]<-sum(kmeans(data_frame_scale,centers=i)$withinss)
plot(1:15,wss,type = "b",xlab="number of clusters",ylab = "within groups sum of squares")
#-----------------------------------------------------------------------------------------
#remove the outliers from data
sub<-subset(data_frame_scale,data_frame_scale$`mean household income` >=4.6 )
sub
km_sub <- kmeans(sub,4,10)
km_sub
#make plot for clean data
plot(sub,col=km_sub$cluster)
points(km_sub$centers,col=1:4,pch=8)
wss<- numeric(15)
for(i in 1:15)wss[i]<-sum(kmeans(sub,centers=i)$withinss)
plot(1:15,wss,type = "b",xlab="number of clusters",ylab = "within groups sum of squares")
#final
