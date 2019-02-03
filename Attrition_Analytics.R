#Importing all the datasets:
setwd("C:/Users/adubey53/Desktop/PGDDA/Course3_ML/HR Case Study/PA-I_Case_Study_HR_Analytics")

# Install and Load the required packages
install.packages("MASS")
install.packages("car")
install.packages("e1071")
install.packages("caret")
install.packages("cowplot")
install.packages("GGally")
install.packages("caTools")
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)

library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(plyr)

#Reading Datasets:
emp_survey <- read.csv("employee_survey_data.csv",stringsAsFactors = F)
general_data <- read.csv("general_data.csv",stringsAsFactors = F)
in_time <- read.csv("in_time.csv",stringsAsFactors = F)
manager_survey <- read.csv("manager_survey_data.csv",stringsAsFactors = F)
out_time <- read.csv("out_time.csv",stringsAsFactors = F)

#Coverting Time data sets to long format:
in_time_long <- gather(in_time,day,day_time_IN,X2015.01.01:X2015.12.31)
colnames(in_time_long) [1]  <- "EmployeeID"
in_time_long <- in_time_long[order(in_time_long$EmployeeID),]
in_time_long <- na.omit(in_time_long)


out_time_long <- gather(out_time,day,day_time_Out,X2015.01.01:X2015.12.31)
colnames(out_time_long)[1] <- "EmployeeID"
out_time_long <- out_time_long[order(out_time_long$EmployeeID),]
out_time_long <- na.omit(out_time_long)
day_time_Out <- out_time_long$day_time_Out


in_out_time_data <- cbind(in_time_long,day_time_Out)

in_out_time_data$day_time_IN <- as.POSIXlt(in_out_time_data$day_time_IN, format = "%Y-%m-%d %H:%M:%S") 
in_out_time_data$day_time_Out <- as.POSIXlt(in_out_time_data$day_time_Out,format = "%Y-%m-%d %H:%M:%S")
str(in_out_time_data)
in_out_time_data$Work_hours <- in_out_time_data$day_time_Out - in_out_time_data$day_time_IN

Average_work_hour <- aggregate(Work_hours~EmployeeID,in_out_time_data,mean)
Average_work_hour <- Average_work_hour[order(Average_work_hour$EmployeeID),]
 
 
 # Collate the data together in one single file
length(unique(tolower(emp_survey$EmployeeID)))    # 4410, confirming EmployeeID is key 
length(unique(tolower(general_data$EmployeeID)))  # 4410, confirming EmployeeID is key
length(unique(tolower(in_time_long$EmployeeID))) # 4410, confirming EmployeeID is key
length(unique(out_time_long$EmployeeID))          #4410 confirming EmployeeID
length(unique(manager_survey$EmployeeID))         #4410, confirming EmployeeID is key
length(unique(Average_work_hour$EmployeeID))     #4410 , confirming EmployeeID is key

setdiff(general_data$EmployeeID,emp_survey$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,in_time_long$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,out_time_long$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,manager_survey$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,Average_work_hour$EmployeeID) # Identical EmployeeID across these datasets

Hr_data<- merge(general_data,emp_survey, by="EmployeeID", all = F)
Hr_data<- merge(Hr_data,manager_survey, by="EmployeeID", all = F)
Hr_data <- merge(Hr_data,Average_work_hour,by="EmployeeID", all = F)
backup <- Hr_data

#Data Cleaning,Preparation and EDA:

str(Hr_data)
colSums(is.na(Hr_data))

#19 NA's in NumCompaniesWorked
#9 NA's in TotalWorkingYears
#25 NA's in EnvironmentSatisfaction
#20 NA's in JobSatisfaction
#38 NA's in WorkLifeBalance

#Checking NA's for NumCompaniesWorked
View(subset(Hr_data, is.na(NumCompaniesWorked),select = c(NumCompaniesWorked,YearsAtCompany,TotalWorkingYears)))

#NA Imputaion for NumCompaniesWorked logic:
#Trend is  if Employees with same YearsAtCompany and TotalWorkingYears , then NumCompaniesWorked is always 1
#And,for NumCompaniesWorked with 0:
# YearsAtCompany + 1 = TotalWorkingYears

Hr_data$NumCompaniesWorked[which(is.na(Hr_data$NumCompaniesWorked))] <- ifelse(Hr_data$YearsAtCompany[which(is.na(Hr_data$NumCompaniesWorked))] == Hr_data$TotalWorkingYears[which(is.na(Hr_data$NumCompaniesWorked))],1,NA)


Hr_data$NumCompaniesWorked[which(is.na(Hr_data$NumCompaniesWorked))] <- ifelse(Hr_data$YearsAtCompany[which(is.na(Hr_data$NumCompaniesWorked))]+1 == Hr_data$TotalWorkingYears[which(is.na(Hr_data$NumCompaniesWorked))],0,NA )




#Checking NA's again:
colSums(is.na(Hr_data))
#NA's dropped to 9 in NumCompaniesWorked
#9 NA's in TotalWorkingYears
#25 NA's in EnvironmentSatisfaction
#20 NA's in JobSatisfaction
#38 NA's in WorkLifeBalance

View(subset(Hr_data, is.na(NumCompaniesWorked),select = c(NumCompaniesWorked,YearsAtCompany,TotalWorkingYears)))

View(subset(Hr_data, is.na(NumCompaniesWorked),select = c(NumCompaniesWorked,YearsAtCompany,TotalWorkingYears,EnvironmentSatisfaction,JobSatisfaction,WorkLifeBalance)))

#Checking NA's of TotalWorkingYears:
View(subset(Hr_data,is.na(TotalWorkingYears)))
#for NumCompaniesWorked with 0 then YearsAtCompany + 1 = TotalWorkingYears
#For NumCompaniesWorked is always 1: then YearsAtCompany =TotalWorkingYears 

Hr_data$TotalWorkingYears[which(is.na(Hr_data$TotalWorkingYears))] <- ifelse(Hr_data$NumCompaniesWorked[which(is.na(Hr_data$TotalWorkingYears))] == 0,Hr_data$YearsAtCompany[which(is.na(Hr_data$TotalWorkingYears))]+1,NA)
Hr_data$TotalWorkingYears[which(is.na(Hr_data$TotalWorkingYears))] <- ifelse(Hr_data$NumCompaniesWorked[which(is.na(Hr_data$TotalWorkingYears))] == 1,Hr_data$YearsAtCompany[which(is.na(Hr_data$TotalWorkingYears))],NA)

View(subset(Hr_data,is.na(TotalWorkingYears)))
#Checking NA's again:
colSums(is.na(Hr_data))
#9 NA's in NumCompaniesWorked
#NA's dropped to 5 TotalWorkingYears
#25 NA's in EnvironmentSatisfaction
#20 NA's in JobSatisfaction
#38 NA's in WorkLifeBalance


#Checking NA's in EnvironmentSatisfaction:
View(subset(Hr_data,is.na(EnvironmentSatisfaction)))
table(Hr_data$EnvironmentSatisfaction)
#Imputing the missing value with mean of EnvironmentSatisfaction:
Hr_data$EnvironmentSatisfaction[which(is.na(Hr_data$EnvironmentSatisfaction))]  <- round(mean(Hr_data$EnvironmentSatisfaction,na.rm = T))

#Checking NA's again:
colSums(is.na(Hr_data))
#9 NA's in NumCompaniesWorked
#5 NA's TotalWorkingYears
#20 NA's in JobSatisfaction
#38 NA's in WorkLifeBalance

#Checking NA's for Job Satisfaction:
View(subset(Hr_data,is.na(JobSatisfaction)))
summary(Hr_data$JobSatisfaction)
table(Hr_data$JobSatisfaction)
#Imputing the missing value with mean of JobSatisfaction:

Hr_data$JobSatisfaction[which(is.na(Hr_data$JobSatisfaction))] <- round(mean(Hr_data$JobSatisfaction,na.rm = T))


#Checking NA's again:
colSums(is.na(Hr_data))
#9 NA's in NumCompaniesWorked
#5 NA's TotalWorkingYears
#38 NA's in WorkLifeBalance

#Checking NA's for WorkLifeBalance :
View(subset(Hr_data,is.na(WorkLifeBalance)))
summary(Hr_data$WorkLifeBalance)
table(Hr_data$WorkLifeBalance)
#Imputing the missing value with mean of WorkLifeBalance:
                        
Hr_data$WorkLifeBalance[which(is.na(Hr_data$WorkLifeBalance))] <- round(mean(Hr_data$WorkLifeBalance,na.rm = T))




#After Checking NA's only 9 for NumCompaniesWorked and 5 for TotalWorkingYears.A
#Checking NA's for NumCompaniesWorked,TotalWorkingYears
View(subset(Hr_data,is.na(Hr_data$NumCompaniesWorked)))
View(subset(Hr_data,is.na(Hr_data$TotalWorkingYears)))

#So, Altogether 14 NA's ,ie 0.32% of the observation can be dropped.
Hr_data <- Hr_data[!is.na(Hr_data$NumCompaniesWorked),]
Hr_data <- Hr_data[!is.na(Hr_data$TotalWorkingYears),]



#Converting Attrition to factor:

Hr_data$Attrition <- as.factor(Hr_data$Attrition)

#Converting Work_hours to Integer type:

Hr_data$Work_hours <- as.integer(Hr_data$Work_hours)

#Elimainating coulmns with only 1 Unique value as we do not need a cloulmn for only 1 unique value

Hr_data <- Hr_data[sapply(Hr_data, function(x) {!length(unique(x))== 1})]
#Checking Blanks:
Hr_data[Hr_data == ""]
#No Blanks Found
#Eliminating EmployeeID as this is a key Variable:
Hr_data <- within(Hr_data,rm(EmployeeID))


# Barcharts for categorical features with stacked 
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")


plot_grid(ggplot(Hr_data, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(Hr_data, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(Hr_data, aes(x=factor(Education),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(Hr_data, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(Hr_data, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(Hr_data, aes(x=factor(JobLevel),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")  
#Inference:
# Seems like BusinessTravel with category Travel_Rarely is a strong indicator of Attrition
#Research & development category in Department is a strong indicator of Attrition
#Education with level 3 and 4 are a strong indicator of Attrition
#Life Sciences & Medical in EducationField s a strong indicator of Attrition
#Initial joblevel such as 1 and 2 are strong indicator of Attrition

plot_grid(ggplot(Hr_data, aes(x=factor(JobRole),fill=Attrition))+ geom_bar() + bar_theme1,
          ggplot(Hr_data, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(Hr_data, aes(x=factor(NumCompaniesWorked),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(Hr_data, aes(x=factor(StockOptionLevel),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(Hr_data, aes(x=factor(TrainingTimesLastYear),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(Hr_data, aes(x=factor(EnvironmentSatisfaction),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h") 
#Research Scientist,Sales Executive,Laboratory Technician are the job Roles that are strong indicator of Attrition
#Marital Staus Single is more prone to Attrition
#Number of Companies worked 1 is more Prone to Attrition,as number of companies increases Attrition amongst employees reduces
#Stock option level with 0 and 1 are more prone to Attrition
#TrainingTimesLastYear with 2 and 3 are more prone to Attrition

plot_grid(ggplot(Hr_data, aes(x=factor(JobSatisfaction),fill=Attrition))+ geom_bar() + bar_theme1,
          ggplot(Hr_data, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(Hr_data, aes(x=factor(JobInvolvement),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(Hr_data, aes(x=factor(PerformanceRating),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(Hr_data, aes(x=factor(Work_hours),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h") 

#Job Satisfaction low is a strong indicator of Attrition
#Work life balance Bad is a strong indicator of Attrition 
#Job Involvement High is a strong indicator of Attrition


#EDA for Numeric Variable using Box plot:

box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(Hr_data, aes(Age))+ geom_histogram(binwidth = 10),
          ggplot(Hr_data, aes(x="",y=Attrition))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(Hr_data, aes(DistanceFromHome))+ geom_histogram(binwidth = 20),
          ggplot(Hr_data, aes(x="",y=Attrition))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(Hr_data, aes(MonthlyIncome))+ geom_histogram(binwidth = 10000),
          ggplot(Hr_data, aes(x="",y=Attrition))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

plot_grid(ggplot(Hr_data, aes(YearsAtCompany))+ geom_histogram(),
          ggplot(Hr_data, aes(x="",y=Attrition))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

plot_grid(ggplot(Hr_data, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 2),
          ggplot(Hr_data, aes(x="",y=Attrition))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

plot_grid(ggplot(Hr_data, aes(TotalWorkingYears))+ geom_histogram(binwidth = 5),
          ggplot(Hr_data, aes(x="",y=Attrition))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(Hr_data, aes(PercentSalaryHike))+ geom_histogram(binwidth = 3),
          ggplot(Hr_data, aes(x="",y=Attrition))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(Hr_data, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 3),
          ggplot(Hr_data, aes(x="",y=Attrition))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

# Boxplots of numeric variables relative to Attrition status
plot_grid(ggplot(Hr_data, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(Hr_data, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(Hr_data, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(Hr_data, aes(x= Attrition,y=YearsAtCompany,fill=Attrition)) + geom_boxplot(width=0.2)+coord_flip()+box_theme_y,
          align = "v",nrow = 1)

plot_grid(ggplot(Hr_data, aes(x=Attrition,y=YearsWithCurrManager, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(Hr_data, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(Hr_data, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          ggplot(Hr_data, aes(x= Attrition,y=YearsSinceLastPromotion,fill=Attrition)) + geom_boxplot(width=0.2)+coord_flip()+box_theme_y,
          align = "v",nrow = 1)



# Correlation between numeric variables
library(GGally)
ggpairs(Hr_data[, c("Age", "DistanceFromHome", "MonthlyIncome","YearsAtCompany","YearsWithCurrManager","TotalWorkingYears","PercentSalaryHike","YearsSinceLastPromotion")])

# TotalWorkingYears and Age are highly correlated (corr 0.68)
#YearsWithCurrManager and YearsAtCompany are highly correlated (corr 0.76



###################Coverting Categoricals Variable to Numeric for Logistic Regression Modelling#########################
#Variable with only 2 Levels such as Gender and PerformanceRating

#For variable "Gender" :
# convert it to numeric is to replace the levels-  with 1:Female and 0:Male(Not Female,ie Male) is:
Hr_data$Gender <- as.factor(Hr_data$Gender)
summary(Hr_data$Gender)
levels(Hr_data$Gender)<-c(1,0)

# Now store the numeric values in the same variable
Hr_data$Gender<- as.numeric(levels(Hr_data$Gender))[Hr_data$Gender]

# Check the summary of Gender variable
table(Hr_data$Gender)
str(Hr_data)


#For variable"PerformanceRating" :
# convert PerformanceRating variable to numeric is to replace the levels-  with 1:Rating 3 and 0 with rating 4 is:
Hr_data$PerformanceRating <- as.factor(Hr_data$PerformanceRating)
summary(Hr_data$PerformanceRating)
levels(Hr_data$PerformanceRating)<-c(1,0)

# Now store the numeric values in the same variable
Hr_data$PerformanceRating<- as.numeric(levels(Hr_data$PerformanceRating))[Hr_data$PerformanceRating]

# Check the summary of PerformanceRating variable
table(Hr_data$PerformanceRating)
str(Hr_data)

############## ...Outliers Treatment on Continous Variable:    #################
#Checking Outliers in Age Variable
box <- boxplot.stats(Hr_data$Age)
box$out
quantile(Hr_data$Age,seq(0,1,0.01))
#No Outliers in Age Variable.


#Checking Outliers in DistanceFromHome:
box <- boxplot.stats(Hr_data$DistanceFromHome)
box$out
quantile(Hr_data$DistanceFromHome,seq(0,1,0.01))
#No Outliers in DistanceFromHome Variable.

#Checking Outliers in MonthlyIncome:
box <- boxplot.stats(Hr_data$MonthlyIncome)
box$out
quantile(Hr_data$MonthlyIncome,seq(0,1,0.01))
#There is a Sharp increase in MonthlyIncome after 90%,Hence Capping 90% value 137980.0 for MonthlyIncome.
Hr_data$MonthlyIncome[which(Hr_data$MonthlyIncome>137980.0)] <- 137980.0


#Checking Outliers in YearsAtCompany:
box <- boxplot.stats(Hr_data$YearsAtCompany)
length(box$out)
quantile(Hr_data$YearsAtCompany,seq(0,1,0.01))
#There is a Sharp increase in YearsAtCompany after 97%,Hence Capping 97% value 22.00 for YearsAtCompany
Hr_data$YearsAtCompany[which(Hr_data$YearsAtCompany>22.00)] <- 22.00


#Checking Outliers in YearsWithCurrManager:
box <- boxplot.stats(Hr_data$YearsWithCurrManager)
length(box$out)
#Outliers are there.
quantile(Hr_data$YearsWithCurrManager,seq(0,1,0.01))
#There is a sharp increase in YearsWithCurrManager after 99%,Hence Capping 99% value 14 for YearsWithCurrManager
Hr_data$YearsWithCurrManager[which(Hr_data$YearsWithCurrManager>14)] <- 14

#Checking Outliers in TotalWorkingYears:
box <- boxplot.stats(Hr_data$TotalWorkingYears)
length(box$out)
#Outliers are there.
quantile(Hr_data$TotalWorkingYears,seq(0,1,0.01))
#There is a sharp increase in TotalWorkingYears after 98%,Hence Capping 98% value 32 for TotalWorkingYears
Hr_data$TotalWorkingYears[which(Hr_data$TotalWorkingYears>32)] <- 32

#Checking Outliers in PercentSalaryHike:
box <- boxplot.stats(Hr_data$PercentSalaryHike)
length(box$out)
#No Outliers are there.

#Checking Outliers in YearsSinceLastPromotion:
quantile(Hr_data$YearsSinceLastPromotion,seq(0,1,0.01))
#No Outliers


# creating a dataframe of categorical features
Hr_data_chr<- Hr_data[,c(3,4,6,7,9,10,11,13,15,17,21,22,23,24,25,26)]

# converting categorical attributes to factor
Hr_data_fact<- data.frame(sapply(Hr_data_chr, function(x) factor(x)))
str(Hr_data_fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(Hr_data_fact, function(x) data.frame(model.matrix(~x-1,data =Hr_data_fact))[,-1]))


#Coalating All Numeric Attributes:

Hr_data_num<- Hr_data[,-c(3,4,6,7,9,10,11,13,15,17,21,22,23,24,25,26)]
str(Hr_data_num)
#Final Dataset by binding Numeric Attribute and Dummies.
Hr_data_final <- cbind(Hr_data_num,dummies)
str(Hr_data_final)
#Normalising Continous Features in Final Dataset:
Hr_data_final$Age <- scale(Hr_data$Age)
Hr_data_final$DistanceFromHome <- scale(Hr_data$DistanceFromHome)
Hr_data_final$MonthlyIncome <- scale(Hr_data$MonthlyIncome)
Hr_data_final$PercentSalaryHike <- scale(Hr_data$PercentSalaryHike)
Hr_data_final$TotalWorkingYears <- scale(Hr_data$TotalWorkingYears)
Hr_data_final$YearsAtCompany <- scale(Hr_data$YearsAtCompany)
Hr_data_final$YearsSinceLastPromotion <- scale(Hr_data$YearsSinceLastPromotion)
Hr_data_final$YearsWithCurrManager <- scale(Hr_data$YearsWithCurrManager)

# converting target variable Attrition from No/Yes character to factorwith levels 0/1 
Hr_data_final$Attrition<- ifelse(Hr_data_final$Attrition=="Yes",1,0)
#Final data set has 4396 obs and 74 Variables.

########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(Hr_data_final$Attrition, SplitRatio = 0.7)

train = Hr_data_final[indices,]

test = Hr_data_final[!(indices),]

########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) 

# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2)

# Removing multicollinearity through VIF check
library(car)
vif(model_2)

#dropping BusinessTravel.xTravel_Rarely

model_3 <- glm(formula = Attrition ~ Age + PercentSalaryHike + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                 Department.xSales + Education.x2 + Education.x5 + JobLevel.x2 + 
                 JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + NumCompaniesWorked.x1 + NumCompaniesWorked.x2 + 
                 NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
                 NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
                 TrainingTimesLastYear.x2 + TrainingTimesLastYear.x3 + TrainingTimesLastYear.x6 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + Work_hours.x5 + 
                 Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)
summary(model_3)
vif(model_3)

# Department.xResearch...Development + has high VIF and p value,so dropping them
model_4 <- glm(formula = Attrition ~ Age + PercentSalaryHike + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently +
                 Department.xSales + Education.x2 + Education.x5 + JobLevel.x2 + 
                 JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + NumCompaniesWorked.x1 + NumCompaniesWorked.x2 + 
                 NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
                 NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
                 TrainingTimesLastYear.x2 + TrainingTimesLastYear.x3 + TrainingTimesLastYear.x6 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + Work_hours.x5 + 
                 Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)
summary(model_4)
vif(model_4)

#Dropping Age

model_5 <- glm(formula = Attrition ~ PercentSalaryHike + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently +
                 Department.xSales + Education.x2 + Education.x5 + JobLevel.x2 + 
                 JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + NumCompaniesWorked.x1 + NumCompaniesWorked.x2 + 
                 NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
                 NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
                 TrainingTimesLastYear.x2 + TrainingTimesLastYear.x3 + TrainingTimesLastYear.x6 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + Work_hours.x5 + 
                 Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)
summary(model_5)
vif(model_5)

#Dropping JobRole.xResearch.Scientist
model_6 <- glm(formula = Attrition ~ PercentSalaryHike + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently +
                 Department.xSales + Education.x2 + Education.x5 + JobLevel.x2 + 
                 JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive +MaritalStatus.xSingle + NumCompaniesWorked.x1 + NumCompaniesWorked.x2 + 
                 NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
                 NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
                 TrainingTimesLastYear.x2 + TrainingTimesLastYear.x3 + TrainingTimesLastYear.x6 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + Work_hours.x5 + 
                 Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)
summary(model_6)
vif(model_6)

#dropping NumCompaniesWorked.x2 :
model_7 <- glm(formula = Attrition ~ PercentSalaryHike + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently +
                 Department.xSales + Education.x2 + Education.x5 + JobLevel.x2 + 
                 JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive +MaritalStatus.xSingle + NumCompaniesWorked.x1  + 
                 NumCompaniesWorked.x4 + NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
                 NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
                 TrainingTimesLastYear.x2 + TrainingTimesLastYear.x3 + TrainingTimesLastYear.x6 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + Work_hours.x5 + 
                 Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)

summary(model_7)
vif(model_7)

#Dropping NumCompaniesWorked.x4
model_8 <- glm(formula = Attrition ~ PercentSalaryHike + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently +
                 Department.xSales + Education.x2 + Education.x5 + JobLevel.x2 + 
                 JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive +MaritalStatus.xSingle + NumCompaniesWorked.x1  + 
                  NumCompaniesWorked.x5 + NumCompaniesWorked.x6 + 
                 NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
                 TrainingTimesLastYear.x2 + TrainingTimesLastYear.x3 + TrainingTimesLastYear.x6 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + Work_hours.x5 +Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)

summary(model_8)
vif(model_8)
# Dropping NumCompaniesWorked.x1
model_9 <-glm(formula = Attrition ~ PercentSalaryHike + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently +
                Department.xSales + Education.x2 + Education.x5 + JobLevel.x2 + 
                JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xSales.Executive +MaritalStatus.xSingle   + 
                NumCompaniesWorked.x5 + NumCompaniesWorked.x6 +NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
                TrainingTimesLastYear.x2 + TrainingTimesLastYear.x3 + TrainingTimesLastYear.x6 + 
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + JobInvolvement.x3 + Work_hours.x5 +Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)

summary(model_9)
vif(model_9)

#Drop TrainingTimesLastYear.x2
model_10 <-glm(formula = Attrition ~ PercentSalaryHike + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently +
                 Department.xSales + Education.x2 + Education.x5 + JobLevel.x2 + 
                 JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive +MaritalStatus.xSingle   + 
                 NumCompaniesWorked.x5 + NumCompaniesWorked.x6 +NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
                   TrainingTimesLastYear.x3 + TrainingTimesLastYear.x6 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + Work_hours.x5 +Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)

summary(model_10)
vif(model_10)

#Drop  JobSatisfaction.x2
model_11 <-glm(formula = Attrition ~ PercentSalaryHike + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently +
                 Department.xSales + Education.x2 + Education.x5 + JobLevel.x2 + 
                 JobRole.xLaboratory.Technician + JobRole.xResearch.Director +JobRole.xSales.Executive +MaritalStatus.xSingle   + 
                 NumCompaniesWorked.x5 + NumCompaniesWorked.x6 +NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
                 TrainingTimesLastYear.x3 + TrainingTimesLastYear.x6 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4  + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + Work_hours.x5 +Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)
summary(model_11)
vif(model_11)
#Remove JobRole.xLaboratory.Technician
model_12 <-glm(formula = Attrition ~ PercentSalaryHike + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently +
                 Department.xSales + Education.x2 + Education.x5 + JobLevel.x2 + 
                   JobRole.xResearch.Director +JobRole.xSales.Executive +MaritalStatus.xSingle   + 
                 NumCompaniesWorked.x5 + NumCompaniesWorked.x6 +NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
                 TrainingTimesLastYear.x3 + TrainingTimesLastYear.x6 + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4  + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + Work_hours.x5 +Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)
summary(model_12)
vif(model_12)

#Drop WorkLifeBalance.x2 
model_13 <-glm(formula = Attrition ~ PercentSalaryHike + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently +
                 Department.xSales + Education.x2 + Education.x5 + JobLevel.x2 +JobRole.xResearch.Director +JobRole.xSales.Executive +MaritalStatus.xSingle   + 
                 NumCompaniesWorked.x5 + NumCompaniesWorked.x6 +NumCompaniesWorked.x7 + NumCompaniesWorked.x8 + NumCompaniesWorked.x9 + 
                 TrainingTimesLastYear.x3 + TrainingTimesLastYear.x6 +EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4  + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + JobInvolvement.x3 + Work_hours.x5 +Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)
summary(model_13)
vif(model_13)
# removing NumCompaniesWorked.x8
model_14 <- glm(formula = Attrition ~ PercentSalaryHike + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently +
                  Department.xSales + Education.x2 + Education.x5 + JobLevel.x2 +JobRole.xResearch.Director +JobRole.xSales.Executive +MaritalStatus.xSingle   + 
                  NumCompaniesWorked.x5 + NumCompaniesWorked.x6 +NumCompaniesWorked.x7  + NumCompaniesWorked.x9 + 
                  TrainingTimesLastYear.x3 + TrainingTimesLastYear.x6 +EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4  + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobInvolvement.x3 + Work_hours.x5 +Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)
summary(model_14)
vif(model_14)

#Drop JobInvolvement.x3

model_15 <- glm(formula = Attrition ~ PercentSalaryHike + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently +
                  Department.xSales + Education.x2 + Education.x5 + JobLevel.x2 +JobRole.xResearch.Director +JobRole.xSales.Executive +MaritalStatus.xSingle   + 
                  NumCompaniesWorked.x5 + NumCompaniesWorked.x6 +NumCompaniesWorked.x7  + NumCompaniesWorked.x9 + 
                  TrainingTimesLastYear.x3 + TrainingTimesLastYear.x6 +EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4  + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4  + Work_hours.x5 +Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)
summary(model_15)
vif(model_15)
# remove TrainingTimesLastYear.x3
model_16 <- glm(formula = Attrition ~ PercentSalaryHike + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently +
                  Department.xSales + Education.x2 + Education.x5 + JobLevel.x2 +JobRole.xResearch.Director +JobRole.xSales.Executive +MaritalStatus.xSingle   + 
                  NumCompaniesWorked.x5 + NumCompaniesWorked.x6 +NumCompaniesWorked.x7  + NumCompaniesWorked.x9 + 
                    TrainingTimesLastYear.x6 +EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4  + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 +  WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4  + Work_hours.x5 +Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)
summary(model_16)
vif(model_16)

#Drop Education.x2 
model_17 <- glm(formula = Attrition ~ PercentSalaryHike + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently +
                  Department.xSales  + Education.x5 + JobLevel.x2 +JobRole.xResearch.Director +JobRole.xSales.Executive +MaritalStatus.xSingle   + 
                  NumCompaniesWorked.x5 + NumCompaniesWorked.x6 +NumCompaniesWorked.x7  + NumCompaniesWorked.x9 + 
                  TrainingTimesLastYear.x6 +EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4  + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 +  WorkLifeBalance.x3 +WorkLifeBalance.x4  + Work_hours.x5 +Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)
summary(model_17)
vif(model_17)

# Drop  PercentSalaryHike
model_18 <- glm(formula = Attrition ~ TotalWorkingYears +YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently +
                  Department.xSales  + Education.x5 + JobLevel.x2 +JobRole.xResearch.Director +JobRole.xSales.Executive +MaritalStatus.xSingle   + 
                  NumCompaniesWorked.x5 + NumCompaniesWorked.x6 +NumCompaniesWorked.x7  + NumCompaniesWorked.x9 + 
                  TrainingTimesLastYear.x6 +EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4  + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 +  WorkLifeBalance.x3 +WorkLifeBalance.x4  + Work_hours.x5 +Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)

summary(model_18)
vif(model_18)

# remove Department.xSales 
model_19 <- glm(formula = Attrition ~ TotalWorkingYears +YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently +
                   Education.x5 + JobLevel.x2 +JobRole.xResearch.Director +JobRole.xSales.Executive +MaritalStatus.xSingle   + 
                  NumCompaniesWorked.x5 + NumCompaniesWorked.x6 +NumCompaniesWorked.x7  + NumCompaniesWorked.x9 + 
                  TrainingTimesLastYear.x6 +EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4  + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 +  WorkLifeBalance.x3 +WorkLifeBalance.x4  + Work_hours.x5 +Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)

summary(model_19)
vif(model_19)

#Drop JobRole.xSales.Executive

model_20 <- glm(formula = Attrition ~ TotalWorkingYears +YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently +
                  Education.x5 + JobLevel.x2 +JobRole.xResearch.Director  +MaritalStatus.xSingle   + 
                  NumCompaniesWorked.x5 + NumCompaniesWorked.x6 +NumCompaniesWorked.x7  + NumCompaniesWorked.x9 + 
                  TrainingTimesLastYear.x6 +EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4  + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 +  WorkLifeBalance.x3 +WorkLifeBalance.x4  + Work_hours.x5 +Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)

summary(model_20)
vif(model_20)

#Drop WorkLifeBalance.x4 
model_21 <- glm(formula = Attrition ~ TotalWorkingYears +YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently +
                  Education.x5 + JobLevel.x2 +JobRole.xResearch.Director  +MaritalStatus.xSingle   + 
                  NumCompaniesWorked.x5 + NumCompaniesWorked.x6 +NumCompaniesWorked.x7  + NumCompaniesWorked.x9 + 
                  TrainingTimesLastYear.x6 +EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4  + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 +  WorkLifeBalance.x3  + Work_hours.x5 +Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)

summary(model_21)
vif(model_21)

#Drop JobSatisfaction.x3
model_22 <- glm(formula = Attrition ~ TotalWorkingYears +YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently +
                  Education.x5 + JobLevel.x2 +JobRole.xResearch.Director  +MaritalStatus.xSingle   + 
                  NumCompaniesWorked.x5 + NumCompaniesWorked.x6 +NumCompaniesWorked.x7  + NumCompaniesWorked.x9 + 
                  TrainingTimesLastYear.x6 +EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4  + 
                  JobSatisfaction.x4 +  WorkLifeBalance.x3  + Work_hours.x5 +Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)

summary(model_22)
vif(model_22)

#Drop NumCompaniesWorked.x9
model_23 <- glm(formula = Attrition ~ TotalWorkingYears +YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently +
                  Education.x5 + JobLevel.x2 +JobRole.xResearch.Director  +MaritalStatus.xSingle   + 
                  NumCompaniesWorked.x5 + NumCompaniesWorked.x6 +NumCompaniesWorked.x7   + 
                  TrainingTimesLastYear.x6 +EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4  + 
                  JobSatisfaction.x4 +  WorkLifeBalance.x3  + Work_hours.x5 +Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)

summary(model_23)
vif(model_23)

#Drop Education.x5
model_24 <- glm(formula = Attrition ~ TotalWorkingYears +YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently +
                    JobLevel.x2 +JobRole.xResearch.Director  +MaritalStatus.xSingle +NumCompaniesWorked.x5 + NumCompaniesWorked.x6 +NumCompaniesWorked.x7   + 
                  TrainingTimesLastYear.x6 +EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4  + 
                  JobSatisfaction.x4 +  WorkLifeBalance.x3  + Work_hours.x5 +Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)

summary(model_24)
vif(model_24)

#Drop TrainingTimesLastYear.x6
model_25 <- glm(formula = Attrition ~ TotalWorkingYears +YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently +
                  JobLevel.x2 +JobRole.xResearch.Director  +MaritalStatus.xSingle +NumCompaniesWorked.x5 + NumCompaniesWorked.x6 +NumCompaniesWorked.x7   + 
                   EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4  + 
                  JobSatisfaction.x4 +  WorkLifeBalance.x3  + Work_hours.x5 +Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)

summary(model_25)
vif(model_25)

#Drop JobLevel.x2
model_25 <- glm(formula = Attrition ~ TotalWorkingYears +YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently +
                   JobRole.xResearch.Director  +MaritalStatus.xSingle +NumCompaniesWorked.x5 + NumCompaniesWorked.x6 +NumCompaniesWorked.x7   + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4  + 
                  JobSatisfaction.x4 +  WorkLifeBalance.x3  + Work_hours.x5 +Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)

summary(model_25)
vif(model_25)

#Drop NumCompaniesWorked.x6
model_26 <- glm(formula = Attrition ~ TotalWorkingYears +YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently +
                  JobRole.xResearch.Director  +MaritalStatus.xSingle +NumCompaniesWorked.x5  +NumCompaniesWorked.x7   + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4  + 
                  JobSatisfaction.x4 +  WorkLifeBalance.x3  + Work_hours.x5 +Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)

summary(model_26)
vif(model_26)

#Drop JobRole.xResearch.Director  
model_27 <- glm(formula = Attrition ~ TotalWorkingYears +YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently +
                  MaritalStatus.xSingle +NumCompaniesWorked.x5  +NumCompaniesWorked.x7   + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4  + 
                  JobSatisfaction.x4 +  WorkLifeBalance.x3  + Work_hours.x5 +Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)

summary(model_27)
vif(model_27)


#Drop  Work_hours.x5 
model_28 <- glm(formula = Attrition ~ TotalWorkingYears +YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently +
                  MaritalStatus.xSingle +NumCompaniesWorked.x5  +NumCompaniesWorked.x7   + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4  + 
                  JobSatisfaction.x4 +  WorkLifeBalance.x3   +Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)

summary(model_28)
vif(model_28)

#Drop WorkLifeBalance.x3
model_29 <- glm(formula = Attrition ~ TotalWorkingYears +YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently +
                  MaritalStatus.xSingle +NumCompaniesWorked.x5  +NumCompaniesWorked.x7   + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4  + 
                  JobSatisfaction.x4    +Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)

summary(model_29)
vif(model_29)

#Drop NumCompaniesWorked.x5

model_30 <- glm(formula = Attrition ~ TotalWorkingYears +YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently +
                  MaritalStatus.xSingle   +NumCompaniesWorked.x7   + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4  + 
                  JobSatisfaction.x4    +Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)

summary(model_30)
vif(model_30)

#Drop NumCompaniesWorked.x7   

model_31 <- glm(formula = Attrition ~ TotalWorkingYears +YearsSinceLastPromotion + YearsWithCurrManager + BusinessTravel.xTravel_Frequently +
                  MaritalStatus.xSingle   +
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 +EnvironmentSatisfaction.x4  + 
                  JobSatisfaction.x4    +Work_hours.x6 + Work_hours.x7, family = "binomial", data = train)

summary(model_31)
vif(model_31)



final_model<- model_31

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Attrition 1 for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])


# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)
# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_actual_attrition,test_pred_attrition)

#Accuracy Here is 85.67% (1076+54)/1076+31+158+54 =0.8567
#Attrition Accuracy:54/158+54= 0.254 ,ie 25.4%,this is Sensitivity of the model
#Non-Attrition Accuracy:1076/1076+31 ,ie 97%,this is Specificity of the model

#Inference:
#sensitivity of the model is 25.4% , which indicates how many of the actual Attrition the model was able to predict accurately
#Specificity of the model id 97%, which indicates ,how many of actual Non-Attrition of the model was predicted accurately

###Trying with different Threshold value now:


#######################################################################
test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

install.packages("e1071")
library(e1071)
test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

#Now Sensitivity of Model Increased to 34.91% & Specificity  94.40%
#######################################################################

#########################################################################################
# Choosing the cutoff value and find out the optimal probalility cutoff
 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


# Creating cutoff values from 0.0009104 to 0.8938134  for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.89,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Let's choose a cutoff value of 0.1611 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.1611, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

View(test)

#With The cutoff 0.1611,got decent model with Accuracy as 71.79%,Sensitivity as 71.22% and Specificity as 71.90%
#Thus Model's sensitivity 71.22% , which indicates the actual Attrition the model is predicting accurately
#Specificity of the model id 71.90%, which indicates ,the Non-Attrition the model is predicting accurately
##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)



library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Atrrition_decile = lift(test_actual_attrition, test_pred, groups = 10)

#This  model, KS statistic around more than 40% and lies in the top few deciles (1st to 4th).Hence,we conclude this to be good model.
