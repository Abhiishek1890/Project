rm(list=ls())

#Set working directory
setwd("D:/Data Science/Project 1")

#Get working directory
getwd()

#Load Libraries
x=c("ggplot2","corrgram","DMwR")

install.packages(x)
lapply(x,require,character.only = TRUE)
rm(x)


#Importing File
credit=read.csv("credit-card-data.csv", header = T)

names(credit)

str(credit)


#Convert a Data in proper data type
credit$CUST_ID=as.factor(as.character(credit$CUST_ID))
credit$CASH_ADVANCE_TRX=as.numeric(as.integer(credit$CASH_ADVANCE_TRX))
credit$PURCHASES_TRX=as.numeric(as.integer(credit$PURCHASES_TRX))
credit$TENURE=as.numeric(as.integer(credit$TENURE))

str(credit)

summary(credit)

################### Missing value analysis ######################


#creat dataframe with missing percentage
missing_val=data.frame(apply(credit,2,function(x){sum(is.na(x))}))
missing_val

#Convert row names into column
missing_val$Columns=row.names(missing_val)
row.names(missing_val)=NULL    

#Rename the variable name
names(missing_val)[1]="Missing_percentage"

#calculate percentage
missing_val$Missing_percentage=(missing_val$Missing_percentage/nrow(credit))*100

#Arrange is descending order
missing_val=missing_val[order(-missing_val$Missing_percentage),]

#Rearranging the column
missing_val=missing_val[,c(2,1)]

#Write output results back into disk
write.csv(missing_val,"Missing_Perc.CSV",row.names = F)

#Plot bar graph for missing values
ggplot(data=missing_val[1:3,],aes(x=reorder(Columns,-Missing_percentage),y=Missing_percentage))+
            geom_bar(stat = "identity",fill="grey")+xlab("parameter")+
            ggtitle("Missing Data Percentage(train)")+theme_bw()



credit[9,16]


#Actual Value = 311.9634

#Mean = 864.2705
#Median = 312.4523
#KNN = 440.5623
  


#Mean Method
credit[9,16]=NA
credit$MINIMUM_PAYMENTS[is.na(credit$MINIMUM_PAYMENTS)]=mean(credit$MINIMUM_PAYMENTS,na.rm=T)
credit[9,16]

#Median Method
credit[9,16]=NA
credit$MINIMUM_PAYMENTS[is.na(credit$MINIMUM_PAYMENTS)]=median(credit$MINIMUM_PAYMENTS,na.rm=T)
credit[9,16]

#KNN Imputation
credit[9,16]=NA
credit=knnImputation(credit,k=5)
credit[9,16]

 
########### We have to finalize Median Method for Missing Value Imputation ###########

#Median Method
credit[9,16]=NA
credit$MINIMUM_PAYMENTS[is.na(credit$MINIMUM_PAYMENTS)]=median(credit$MINIMUM_PAYMENTS,na.rm=T)
credit[9,16]

#Checking missing value in data
sum(is.na(credit))


#We impute value by using median method

credit$MINIMUM_PAYMENTS[is.na(credit$MINIMUM_PAYMENTS)]=median(credit$MINIMUM_PAYMENTS,na.rm=T)

credit$CREDIT_LIMIT[is.na(credit$CREDIT_LIMIT)]=median(credit$CREDIT_LIMIT,na.rm=T)

sum(is.na(credit))



#### New Variables creation ##### Deriving New KPI ####


credit$MONTHLY_AVG_PURCHASES = credit$PURCHASES/(credit$TENURE)
credit$MONTHLY_CASH_ADVANCE = credit$CASH_ADVANCE/(credit$TENURE)
credit$LIMIT_USAGE = credit$BALANCE/credit$CREDIT_LIMIT
credit$MIN_PAYMENTS_RATIO = credit$PAYMENTS/credit$MINIMUM_PAYMENTS


str(credit)

names(credit)


sum(is.na(credit))

#We impute value by using median method

credit$MONTHLY_AVG_PURCHASES[is.na(credit$MONTHLY_AVG_PURCHASES)]=median(credit$MONTHLY_AVG_PURCHASES,na.rm=T)
credit$MONTHLY_CASH_ADVANCE[is.na(credit$MONTHLY_CASH_ADVANCE)]=median(credit$MONTHLY_CASH_ADVANCE,na.rm=T)
credit$LIMIT_USAGE[is.na(credit$LIMIT_USAGE)]=median(credit$LIMIT_USAGE,na.rm=T)
credit$MIN_PAYMENTS_RATIO[is.na(credit$MIN_PAYMENTS_RATIO)]=median(credit$MIN_PAYMENTS_RATIO,na.rm=T)

sum(is.na(credit))

write.csv(credit,"Missing_value_treatment.csv")


boxplot(credit)

str(credit)


#Data Manupulation : convert string categories into factor numeric( to convert character variable into factor numeric for column no. 1 )

for(i in 1:ncol(credit))
{
  if (class(credit[,i])=='factor')
  {
    credit[,i]=factor(credit[,i],
    labels = (1:length(levels(factor(credit[,i])))))
  }
}




######################## Outlier Analysis ############################

numeric_index=sapply(credit,is.numeric) #selecting only numeric
numeric_index



mystats = function(x) 
  {
  nmiss = sum(is.na(x))  # Checking Missing value in X column
  nmiss                  # Missing value count
  
  a = x[!is.na(x)]       # Not NA values store in a
  
  m = mean(a)     #Mean of variable
  
  n = length(a)   #Row Count of variable
  
  s = sd(a)       # Standard deviation
  
  min = min(a)    #Minimum value of variable store in min
  
  p1 = quantile(a,0.01)  #Quantile 10% Value
 
  q1 = quantile(a,0.25)  #Lower Quartile 25% Value
  q2 = quantile(a,0.5)   #Middle Quartile Median 50% Value
  q3 = quantile(a,0.75)  #Upper Quartile 75% Value
  
  p99 = quantile(a,0.99) #Quantile 99% Value
  
  max = max(a)  # Maximun value of variable store in max
  
  UC = m+2*s #For such distributions it is always the case that 68% of values are less than one standard deviation (1SD) away from the mean value,that 95% of values are less than two standard deviations (2SD) away from the mean and that 99% of values are less than three standard deviations (3SD) away from the mean.
  LC = m-2*s  ##Lower Fence (Cut)
  
  outlier_flag = max>UC | min<LC
  
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, p1=p1,q1=q1,q2=q2,q3=q3,p99=p99,max=max, UC=UC, LC=LC ))
}




Num_Vars = c("BALANCE","BALANCE_FREQUENCY","PURCHASES","ONEOFF_PURCHASES","INSTALLMENTS_PURCHASES","CASH_ADVANCE","PURCHASES_FREQUENCY",
             "ONEOFF_PURCHASES_FREQUENCY","PURCHASES_INSTALLMENTS_FREQUENCY","CASH_ADVANCE_FREQUENCY","CASH_ADVANCE_TRX","PURCHASES_TRX",
             "CREDIT_LIMIT","PAYMENTS","MINIMUM_PAYMENTS","PRC_FULL_PAYMENT","TENURE","MONTHLY_AVG_PURCHASES",
             "MONTHLY_CASH_ADVANCE","LIMIT_USAGE","MIN_PAYMENTS_RATIO")


Outliers = t(data.frame(apply(credit[Num_Vars], 2, mystats)))

View(Outliers)


write.csv(Outliers,"Outliers.csv")



##### Outlier Treatment ######

credit$BALANCE[credit$BALANCE>5727.53] = 5727.53
credit$BALANCE_FREQUENCY[credit$BALANCE_FREQUENCY>1.3510787] = 1.3510787
credit$PURCHASES[credit$PURCHASES>5276.46] = 5276.46
credit$ONEOFF_PURCHASES[credit$ONEOFF_PURCHASES>3912.2173709] = 3912.2173709
credit$INSTALLMENTS_PURCHASES[credit$INSTALLMENTS_PURCHASES>2219.7438751] = 2219.7438751
credit$CASH_ADVANCE[credit$CASH_ADVANCE>5173.1911125] = 5173.1911125
credit$PURCHASES_FREQUENCY[credit$PURCHASES_FREQUENCY>1.2930919] = 1.2930919
credit$ONEOFF_PURCHASES_FREQUENCY[credit$ONEOFF_PURCHASES_FREQUENCY>0.7991299] = 0.7991299
credit$PURCHASES_INSTALLMENTS_FREQUENCY[credit$PURCHASES_INSTALLMENTS_FREQUENCY>1.1593329] = 1.1593329
credit$CASH_ADVANCE_FREQUENCY[credit$CASH_ADVANCE_FREQUENCY>0.535387] = 0.535387
credit$CASH_ADVANCE_TRX[credit$CASH_ADVANCE_TRX>16.8981202] = 16.8981202
credit$PURCHASES_TRX[credit$PURCHASES_TRX>64.4251306] = 64.4251306
credit$CREDIT_LIMIT[credit$CREDIT_LIMIT>11771.67] = 11771.67
credit$PAYMENTS[credit$PAYMENTS>7523.27] = 7523.27
credit$MINIMUM_PAYMENTS[credit$MINIMUM_PAYMENTS>5525.38] = 5525.38
credit$PRC_FULL_PAYMENT[credit$PRC_FULL_PAYMENT>0.738713] = 0.738713
credit$TENURE[credit$TENURE>14.19398] = 14.19398
credit$MONTHLY_AVG_PURCHASES[credit$MONTHLY_AVG_PURCHASES>710.57] = 710.57
credit$MONTHLY_CASH_ADVANCE[credit$MONTHLY_CASH_ADVANCE>1886.06] = 1886.06
credit$LIMIT_USAGE[credit$LIMIT_USAGE>1.1683] = 1.1683
credit$MIN_PAYMENTS_RATIO[credit$MIN_PAYMENTS_RATIO>245.38] = 245.38


write.csv(credit,"Outlier_treatment.csv")

sum(is.na(credit))




############## Correlation Plot ################

corrgram(credit[,numeric_index],order=F,upper.panel=panel.pie,text.panel=panel.txt,main = "correlation plot")


#Dimension Reduction : Variable Reduction (Factor Analysis)
credit_deleted=subset(credit,select=-c(PURCHASES,PURCHASES_TRX,CASH_ADVANCE))


write.csv(credit_deleted, "Correlation_matrix.csv")



###################### Feature scaling ######################

#Normality check
qqnorm(credit_deleted$MONTHLY_AVG_PURCHASES) #for comparing two probability distributions by plotting their quantiles against each other.

#If the two distributions being compared are similar, the points in the Q-Q plot will approximately lie on the line y = x.

#If the distributions are linearly related, the points in the Q-Q plot will approximately lie on a line.

hist(credit_deleted$MONTHLY_AVG_PURCHASES)
hist(credit_deleted$MIN_PAYMENTS_RATIO)

#Data is left skewed.We will standardize the data

cnames=c("BALANCE","BALANCE_FREQUENCY","ONEOFF_PURCHASES","INSTALLMENTS_PURCHASES","PURCHASES_FREQUENCY",
         "ONEOFF_PURCHASES_FREQUENCY","PURCHASES_INSTALLMENTS_FREQUENCY","CASH_ADVANCE_FREQUENCY","CASH_ADVANCE_TRX",
         "CREDIT_LIMIT","PAYMENTS","MINIMUM_PAYMENTS","PRC_FULL_PAYMENT","TENURE","MONTHLY_AVG_PURCHASES",
         "MONTHLY_CASH_ADVANCE","LIMIT_USAGE","MIN_PAYMENTS_RATIO")


#Standardisation

for(i in cnames)
{
  print(i)
  credit_deleted[,i]=(credit_deleted[,i]-mean(credit_deleted[,i]))/sd(credit_deleted[,i])
  
}



write.csv(credit_deleted, "Standardisation_data.csv")

df = credit_deleted

str(df)

################################################

# This is a Unsupervised data.We are go for clustering....


df_new=data.frame(scale(df[-1])) # Scale will convert all the numerical variable from different range to same range..


#Method to calculate optimal k
k.max = 10 #Maximum 10 clusters assumed
wss=rep(NA,k.max)
nClust=list()

for(i in 1:k.max)
{
  driveClasses=kmeans(df_new,i)
  wss[i]=driveClasses$tot.withinss
  nClust[[i]]=driveClasses$size
}

plot(1:k.max,wss,type="b",pch=19,xlab="Number of clusters K",ylab="Total within-clusters sum of squares:Credit")



#Building clusters for 3,4,5,6 using k-means clustering 

cluster_three = kmeans(df_new,3)
cluster_three

cluster_four = kmeans(df_new,4)
cluster_four

cluster_five = kmeans(df_new,5)
cluster_five

cluster_six = kmeans(df_new,6)
cluster_six


credit_new = cbind(credit,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster,km_clust_5=cluster_five$cluster ,km_clust_6=cluster_six$cluster)

View(credit_new)



##### Profiling ######


Num_Vars2 = c("BALANCE","BALANCE_FREQUENCY","ONEOFF_PURCHASES","INSTALLMENTS_PURCHASES","PURCHASES_FREQUENCY",
              "ONEOFF_PURCHASES_FREQUENCY","PURCHASES_INSTALLMENTS_FREQUENCY","CASH_ADVANCE_FREQUENCY","CASH_ADVANCE_TRX",
              "CREDIT_LIMIT","PAYMENTS","MINIMUM_PAYMENTS","PRC_FULL_PAYMENT","TENURE","MONTHLY_AVG_PURCHASES",
              "MONTHLY_CASH_ADVANCE","LIMIT_USAGE","MIN_PAYMENTS_RATIO")

install.packages("tables")

require(tables)

tt = cbind(tabular(1+factor(km_clust_3)+factor(km_clust_4)+factor(km_clust_5)+factor(km_clust_6)~Heading()*length*All(credit[1]),data=credit_new),
           tabular(1+factor(km_clust_3)+factor(km_clust_4)+factor(km_clust_5)+factor(km_clust_6)~Heading()*mean*All(credit[Num_Vars2]),data=credit_new))

View(tt)

tt2 = as.data.frame.matrix(tt)

View(tt2)


rownames(tt2) = c("ALL","KM3_1","KM3_2","KM3_3","KM4_1","KM4_2","KM4_3","KM4_4", "KM5_1","KM5_2","KM5_3","KM5_4","KM5_5","KM6_1", "KM6_2","KM6_3","KM6_4","KM6_5","KM6_6")




colnames(tt2) = c("BALANCE","BALANCE_FREQUENCY","ONEOFF_PURCHASES","INSTALLMENTS_PURCHASES","PURCHASES_FREQUENCY",
                     "ONEOFF_PURCHASES_FREQUENCY","PURCHASES_INSTALLMENTS_FREQUENCY","CASH_ADVANCE_FREQUENCY","CASH_ADVANCE_TRX",
                     "CREDIT_LIMIT","PAYMENTS","MINIMUM_PAYMENTS","PRC_FULL_PAYMENT","TENURE","MONTHLY_AVG_PURCHASES",
                     "MONTHLY_CASH_ADVANCE","LIMIT_USAGE","MIN_PAYMENTS_RATIO")




cluster_profiling2 = t(tt2)



write.csv(cluster_profiling2,'cluster_profiling2.csv')

