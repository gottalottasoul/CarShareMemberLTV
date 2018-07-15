###########################################################################
# Zipcar CLV
# 1/9/17
###########################################################################


require(zipcarFunctions)

#unload all previously loaded packages
detachAllPackages()


#load required libraries
if (!require(readr)) {
  install.packages('readr') # read preformatted dates
  require(read)
}
if (!require(data.table)) {
  install.packages('data.table') # faster fread() and better weekdays()
  require(data.table)
}
if (!require(dplyr)) {
  install.packages('dplyr') # consistent data.frame operations
  require(dplyr)
}
if (!require(purrr)) {
  install.packages('purrr') # consistent & safe list/vector munging
  require(purrr)
}
if (!require(tidyr)) {
  install.packages('tidyr') # consistent data.frame cleaning
  require(tidyr)
}
if (!require(lubridate)) {
  install.packages('lubridate') # date manipulation
  require(lubridate)
}
if (!require(broom)) {
  install.packages('broom') # date manipulation
  require(broom)
}
if (!require(caret)) {
  install.packages('caret') # date manipulation
  require(caret)
}
if (!require(xgboost)) {
  install.packages('xgboost') # date manipulation
  require(xgboost)
}
if (!require(randomForest)) {
  install.packages('randomForest') # date manipulation
  require(randomForest)
}
if (!require(quantregForest)) {
  install.packages('quantregForest') # date manipulation
  require(quantregForest)
}
if (!require(e1071)) {
  install.packages('e1071') # date manipulation
  require(e1071)
}
if (!require(ranger)) {
  install.packages('ranger') # date manipulation
  require(ranger)
}
if (!require(devtools)) {
  install.packages('devtools') # date manipulation
  require(devtools)
}
if (!require(here)) {
  devtools::install_github("krlmlr/here") #relative working directory
  require(here)
}

#set working directory
setwd(here())

## clear the console of all variables
rm(list = ls())
## free up unused memory
gc()

####start functions
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

####end functions

#define some constants
up.revenue.margin<-.3
up.revenue.margin.wkday<-.40
up.revenue.margin.wkend<-.15


three_year_driving_revenue <- read_delim("../../Data/CLV/three_year_driving_revenue.txt"
                                         ,"\t"
                                         , escape_double = FALSE
                                         , col_types = cols(JOIN_APP_FEE = col_double()
                                                            ,JOIN_DATE = col_date(format = "%m/%d/%Y")
                                                            ,LEAVE_DATE = col_date(format = "%m/%d/%Y")
                                                            ,MEMBER_ID = col_character()
                                                            , RP_FEE = col_double()
                                                            ,RESERVATIONS=col_double()))
                                         

three_year_first_ninety_days <- read_delim("../../Data/CLV/three_year_first_ninety_days.txt"
                                           ,"\t"
                                           , escape_double = FALSE
                                           , col_types = cols(MEMBER_ID = col_character()))

#three_year_first_year <- read_delim("../../Data/CLV/three_year_first_360_days.txt"
                                           ,"\t"
                                           , escape_double = FALSE
                                           , col_types = cols(MEMBER_ID = col_character()))


three_year_fee_revenue <- read_delim("../../Data/CLV/three_year_fee_revenue.txt"
                                     ,"\t"
                                     , escape_double = FALSE
                                     , col_types = cols(MEMBER_ID = col_character()
                                                        ,YEARONEAPPFEE = col_double()
                                                        ,YEARTHREEAPPFEE = col_double()
                                                        ,YEARTWOAPPFEE = col_double()))


application_source <- read_delim("../../Data/CLV/application_source.txt"
                                 ,"\t"
                                 ,escape_double = FALSE
                                 ,col_types = cols(APPLICATION_ID = col_character()
                                                   ,APP_DATE = col_date(format = "%m/%d/%Y")
                                                   ,MEMBER_ID = col_character()))

member.clv.all<-merge(three_year_driving_revenue,three_year_fee_revenue,all.x=TRUE,by='MEMBER_ID')
member.clv.all<-merge(member.clv.all,three_year_first_ninety_days,all.x=TRUE,by='MEMBER_ID')
#member.clv.all<-merge(member.clv.all,three_year_first_year,all.x=TRUE,by='MEMBER_ID')
member.clv.all<-merge(member.clv.all,application_source,all.x=TRUE,by='MEMBER_ID')
member.clv<-member.clv.all%>%
  mutate(Join_Year=as.numeric(year(JOIN_DATE)))%>%
  filter(Join_Year<=2014)



member.clv[which(is.na(member.clv$LEAVE_DATE)),]$LEAVE_DATE<- as.Date('2199-01-01')
member.clv[which(is.na(member.clv$GA_MEDIUM)),]$GA_MEDIUM<- 'Not Available'
member.clv[which(is.na(member.clv$SOURCE)),]$SOURCE<- 'Not Available'
member.clv$APP_DATE<-NULL
member.clv[is.na(member.clv)]<-0
member.clv$ZIPFLEET<-ifelse(member.clv$ZIPFLEET %in% c('country_us_airports','fleet_montreal_qc','fleet_canada_template','hawaii_region'),'other_region',member.clv$ZIPFLEET)
member.clv$ZIPFLEET<-factor(member.clv$ZIPFLEET, levels=c('dc_region','ny_region','country_us_agencies','philadelphia_region','baltimore_region','pittsburgh_region'
                                                          ,'vancouver_region','providence_region','sanfrancisco_region','chicago_region','toronto_region','sacramento_region'
                                                          ,'seattle_region','dallas_region','atlanta_region','boston_region','portland_region','los_angeles_region'
                                                          ,'san_diego_region','miami_region','houston_region','milwaukee_region','austin_region'
                                                          ,'detroit_region','minneapolis_region','denver_region','other_region'))
member.clv$BUSINESS_SEGMENT<-factor(member.clv$BUSINESS_SEGMENT,levels=c('Consumer','Business','Collegiate'))
#since there is a category for university, it will be perfectly correlated with biz seg
#therefore roll it into other
member.clv[which(member.clv$PLAN_CATEGORY=='University'),]$PLAN_CATEGORY<-'Other'
member.clv[which(member.clv$PLAN_CATEGORY=='Corporate'),]$PLAN_CATEGORY<-'Other'
member.clv$PLAN_CATEGORY<-factor(member.clv$PLAN_CATEGORY,levels=c('ODP',"Access",'Employee','EVP','Monthly','Other'))
member.clv$SOURCE<-factor(member.clv$SOURCE,levels=c('acs','ij','ijd','Not Available'))
member.clv$GA_MEDIUM<-factor(member.clv$GA_MEDIUM,levels=c('Not Available','(none)','referral','organic','affiliate','(not set)','cpc','display','mobile','local_dc','email'))
member.clv$DiscountApplication<-as.factor(ifelse(member.clv$JOIN_APP_FEE==0,1,0))
member.clv$JoinMonth<-factor(month(member.clv$JOIN_DATE),levels=c(7,1,2,3,4,5,6,8,9,10,11,12))
member.clv$DiscountedMembership<-as.factor(ifelse(member.clv$RP_FEE %in% c(6,7,60,70),0,1))
member.clv$LATE_RETURNS_IND_90_DAYS<-as.factor(ifelse(member.clv$LATE_RETURNS_90_DAYS>0,1,0))
member.clv$BILLING_CHANGES_IND_90_DAYS<-as.factor(ifelse(member.clv$BILLING_CHANGES_90_DAYS>0,1,0))
member.clv$ACCIDENTS_IND_90_DAYS<-as.factor(ifelse(member.clv$ACCIDENTS_90_DAYS>0,1,0))
member.clv$DELINQUENCIES_IND_90_DAYS<-as.factor(ifelse(member.clv$DELINQUENCIES_90_DAYS>0,1,0))
member.clv$LOW_FUEL_TICKETS_IND_90_DAYS<-as.factor(ifelse(member.clv$LOW_FUEL_TICKETS_90_DAYS>0,1,0))
member.clv$CAR_NOT_CLEAN_TICKETS_IND_90_DAYS<-as.factor(ifelse(member.clv$CAR_NOT_CLEAN_TICKETS_90_DAYS>0,1,0))
member.clv$MINOR_DAMAGE_TICKETS_IND_90_DAYS<-as.factor(ifelse(member.clv$MINOR_DAMAGE_TICKETS_90_DAYS>0,1,0))
member.clv$FRAUD_WARNING_TICKETS_IND_90_DAYS<-as.factor(ifelse(member.clv$FRAUD_WARNING_TICKETS_90_DAYS>0,1,0))
#these are the year indicators
member.clv$LATE_RETURNS_IND_YEAR<-as.factor(ifelse(member.clv$LATE_RETURNS_YEAR>0,1,0))
member.clv$BILLING_CHANGES_IND_YEAR<-as.factor(ifelse(member.clv$BILLING_CHANGES_YEAR>0,1,0))
member.clv$ACCIDENTS_IND_YEAR<-as.factor(ifelse(member.clv$ACCIDENTS_YEAR>0,1,0))
member.clv$DELINQUENCIES_IND_YEAR<-as.factor(ifelse(member.clv$DELINQUENCIES_YEAR>0,1,0))
member.clv$LOW_FUEL_TICKETS_IND_YEAR<-as.factor(ifelse(member.clv$LOW_FUEL_TICKETS_YEAR>0,1,0))
member.clv$CAR_NOT_CLEAN_TICKETS_IND_YEAR<-as.factor(ifelse(member.clv$CAR_NOT_CLEAN_TICKETS_YEAR>0,1,0))
member.clv$MINOR_DAMAGE_TICKETS_IND_YEAR<-as.factor(ifelse(member.clv$MINOR_DAMAGE_TICKETS_YEAR>0,1,0))
member.clv$FRAUD_WARNING_TICKETS_IND_YEAR<-as.factor(ifelse(member.clv$FRAUD_WARNING_TICKETS_YEAR>0,1,0))
#endyear indicators
member.clv$YEAR_INC<-member.clv$Join_Year-2010
member.clv$MemLength<-as.numeric(member.clv$LEAVE_DATE-member.clv$JOIN_DATE)
#member.clv$LTV<-(member.clv$YEARONEREV*up.revenue.margin)+(member.clv$YEARTWOREV*up.revenue.margin)+(member.clv$YEARTHREEREV*up.revenue.margin)+member.clv$YEARONEAPPFEE+member.clv$YEARTWOAPPFEE+member.clv$YEARTHREEAPPFEE+member.clv$YEARONEMEMBERFEE+member.clv$YEARTWOMEMBERFEE+member.clv$YEARTHREEMEMBERFEE

member.clv$LTV<-(member.clv$YEARONEREV_WKEND*up.revenue.margin.wkend)+(member.clv$YEARTWOREV_WKEND*up.revenue.margin.wkend)+(member.clv$YEARTHREEREV_WKEND*up.revenue.margin.wkend)+
  (member.clv$YEARONEREV_WKDAY*up.revenue.margin.wkday)+(member.clv$YEARTWOREV_WKDAY*up.revenue.margin.wkday)+(member.clv$YEARTHREEREV_WKDAY*up.revenue.margin.wkday)+
  member.clv$YEARONEAPPFEE+member.clv$YEARTWOAPPFEE+member.clv$YEARTHREEAPPFEE+
  member.clv$YEARONEMEMBERFEE+member.clv$YEARTWOMEMBERFEE+member.clv$YEARTHREEMEMBERFEE
#member.clv$LTV.Y1<-(member.clv$YEARONEREV*up.revenue.margin)+member.clv$YEARONEAPPFEE+member.clv$YEARONEMEMBERFEE
#member.clv$LTV.Y1<-(member.clv$YEARONEREV_WKEND*up.revenue.margin.wkend)+(member.clv$YEARONEREV_WKDAY*up.revenue.margin.wkday)+member.clv$YEARONEAPPFEE+member.clv$YEARONEMEMBERFEE

#let's flag LTV outliers
#summary(member.clv$LTV)
lower.quantile<-quantile(member.clv$LTV,probs=.25)
upper.quantile<-quantile(member.clv$LTV,probs=.75)
inter.quantile<-upper.quantile-lower.quantile
lower.limit=lower.quantile-(inter.quantile*1.5)
upper.limit=upper.quantile+(inter.quantile*1.5)
member.clv$outlier<-ifelse(member.clv$LTV>upper.limit,1,0)
#for some reason there are a handful of records with large weekday and weekend differentials that offset to (relatively) 0
# going to flage them as outliers as well
member.clv[which(member.clv$WKEND_REVENUE< 0),]$outlier<-2
member.clv.filtered<-member.clv%>%
#  filter(MemLength>=90 & FIRST_YEAR_RESERVATIONS>0 &FIRST_YEAR_REVENUE>0 & outlier<2)
#filter(MemLength>=90 & outlier<2)
filter(outlier<1)

member.clv.train<-member.clv.filtered%>%
  filter(Join_Year<2014)

member.clv.test<-member.clv.filtered%>%
  filter(Join_Year==2014)

ltv.summary<-member.clv.train %>% 
#ltv.summary.w.outliers<-member.clv.train %>% 
  mutate(JoinYear=year(JOIN_DATE)
         ,JoinMonth=month(JOIN_DATE)) %>% 
#  group_by(JoinYear,JoinMonth,BUSINESS_SEGMENT) %>%
  group_by(JoinYear,BUSINESS_SEGMENT) %>%   
  summarise(members=n()
            ,rev=sum(LTV)
            ,ltv=rev/members)

member.clv.yearly.avg<-member.clv.train%>%
  group_by(PLAN_CATEGORY,Join_Year)%>%
  summarise(members=n()
            ,avg_rev=mean(LTV))%>%
  mutate(weight=ifelse(Join_Year==2010,1
                       ,ifelse(Join_Year==2011,2
                               ,ifelse(Join_Year==2012,3,4))))%>%
  mutate(weighted_avg=avg_rev*weight)%>%
  group_by(PLAN_CATEGORY)%>%
  summarise(weightedLTV=sum(weighted_avg/sum(weight)))

blake.temp<-member.clv.train%>%
  filter(outlier<2 & LTV>300 & LTV<500)


ggplot(member.clv.train[which(member.clv.train$outlier<2),], aes(LTV)) +
  geom_histogram(bins = 500) +
  fte_theme() +
  labs(title="LTV Distribution (2010-2013)", x="LTV", y="Count of Members") 

ggplot(member.clv.train[which(member.clv.train$outlier<1),], aes(LTV)) +
  geom_histogram(bins = 500) +
  fte_theme() +
  labs(title="LTV Distribution (2010-2013) - exclude outliers ", x="LTV", y="Count of Members") 


ggplot(member.clv.train[which(member.clv.train$outlier<2),], aes(LTV)) +
  geom_histogram(bins = 500) +
  fte_theme() +
  labs(title="LTV Distribution (2010-2013) - exclude outliers ", x="LTV", y="Count of Members") 

ggplot(member.clv.train[which(member.clv.train$outlier<2),], aes(factor(BUSINESS_SEGMENT), LTV)) +
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot() +
#  geom_boxplot(outlier.shape=NA) + #avoid plotting outliers twice
#  geom_jitter(position=position_jitter(width=.1, height=0)) +
  fte_theme() +
  labs(title="LTV Distribution by Business Segment", x="Business Segment", y="LTV") 

ggplot(member.clv.train[which(member.clv.train$outlier<2),], aes(factor(BUSINESS_SEGMENT), LTV)) +
  stat_boxplot(geom ='errorbar') + 
  geom_boxplot() +
  #  geom_boxplot(outlier.shape=NA) + #avoid plotting outliers twice
  #  geom_jitter(position=position_jitter(width=.1, height=0)) +
  coord_cartesian(ylim = c(3000,-500)) +
  fte_theme() +
  labs(title="LTV Distribution by Business Segment (outliers limited)", x="Business Segment", y="LTV") 


member.clv.lm<-lm(~ZIPFLEET+BUSINESS_SEGMENT+PLAN_CATEGORY+DiscountApplication+RP_FEE
                  +DiscountedMembership+NINETY_DAY_RESERVATIONS+NINETY_DAY_REVENUE+JoinMonth
                  +ACCIDENTS_IND_90_DAYS+DELINQUENCIES_IND_90_DAYS+LOW_FUEL_TICKETS_IND_90_DAYS+MINOR_DAMAGE_TICKETS_IND_90_DAYS
                  +FRAUD_WARNING_TICKETS_IND_90_DAYS+TICKETS_90_DAYS+LATE_RETURNS_IND_90_DAYS+BILLING_CHANGES_IND_90_DAYS
                  +ACCIDENTS_90_DAYS+DELINQUENCIES_90_DAYS+LOW_FUEL_TICKETS_90_DAYS+MINOR_DAMAGE_TICKETS_90_DAYS
                  +FRAUD_WARNING_TICKETS_90_DAYS+LATE_RETURNS_90_DAYS+YEAR_INC
                  ,data=member.clv.train)
glance(member.clv.lm)
tidy(member.clv.lm)

plot(member.clv.lm)
my.d<-density(member.clv$LTV)


predictions <- predict(member.clv.lm, member.clv.test)

member.clv.prediction<-member.clv.test%>%
  select(MEMBER_ID,BUSINESS_SEGMENT,PLAN_CATEGORY,ZIPFLEET,LTV)

member.clv.prediction$model.forecast<-predictions
member.clv.prediction<-merge(member.clv.prediction,member.clv.yearly.avg,all.x=TRUE,by='PLAN_CATEGORY')
member.clv.prediction$ModelError<-member.clv.prediction$model.forecast-member.clv.prediction$LTV
member.clv.prediction$WeightedLTVError<-member.clv.prediction$weightedLTV-member.clv.prediction$LTV
member.clv.prediction$AbsModelError<-abs(member.clv.prediction$model.forecast-member.clv.prediction$LTV)
member.clv.prediction$AbsWeightedLTVError<-abs(member.clv.prediction$weightedLTV-member.clv.prediction$LTV)
member.clv.prediction$ModelMAE<- abs(member.clv.prediction$model.forecast-member.clv.prediction$LTV)/member.clv.prediction$LTV
member.clv.prediction$WeightedLTVMAE<- abs(member.clv.prediction$weightedLTV-member.clv.prediction$LTV)/member.clv.prediction$LTV


ggplot(member.clv.prediction, aes(abs(WeightedLTVError))) +
  geom_histogram(bins = 500) +
  fte_theme() +
  labs(title="Weighted Avg Absolute Residual values", x="Residual", y="Count of Occurences") +
  scale_x_continuous(limits = c(0, 5000))

summary(member.clv.prediction)


write_delim(member.clv.prediction,"c:/users/babbenante/downloads/blake.txt",delim="\t")



split=0.80
trainIndex <- createDataPartition(member.clv$LTV, p=split, list=FALSE)
data_train <- member.clv[ trainIndex,]
data_test <- member.clv[-trainIndex,]
member.clv.lm<-lm(LTV.Y1~ZIPFLEET+BUSINESS_SEGMENT+PLAN_CATEGORY+DiscountApplication+RP_FEE
                  +DiscountedMembership+NINETY_DAY_RESERVATIONS+NINETY_DAY_REVENUE+JoinMonth
                  +ACCIDENTS_IND_90_DAYS+DELINQUENCIES_IND_90_DAYS+LOW_FUEL_TICKETS_IND_90_DAYS+MINOR_DAMAGE_TICKETS_IND_90_DAYS
                  +FRAUD_WARNING_TICKETS_IND_90_DAYS+TICKETS_90_DAYS+LATE_RETURNS_IND_90_DAYS+BILLING_CHANGES_IND_90_DAYS
                  +ACCIDENTS_90_DAYS+DELINQUENCIES_90_DAYS+LOW_FUEL_TICKETS_90_DAYS+MINOR_DAMAGE_TICKETS_90_DAYS
                  +FRAUD_WARNING_TICKETS_90_DAYS+LATE_RETURNS_90_DAYS+BILLING_CHANGES_90_DAYS
                  ,data=data_train)

predictions <- predict(member.clv.lm, data_test)

data_test$Prediction<-predictions
write_delim(data_test,"c:/users/babbenante/downloads/blake.txt",delim="\t")
#train_control <- trainControl(method="boot", number=100)
train_control <- trainControl(method="cv", number=5)
# train the model
model.lm <- train(LTV~ZIPFLEET+BUSINESS_SEGMENT+PLAN_CATEGORY+DiscountApplication+RP_FEE
                  +DiscountedMembership+NINETY_DAY_RESERVATIONS+NINETY_DAY_REVENUE+JoinMonth
                  +ACCIDENTS_IND_90_DAYS+DELINQUENCIES_IND_90_DAYS+LOW_FUEL_TICKETS_IND_90_DAYS+MINOR_DAMAGE_TICKETS_IND_90_DAYS
                  +FRAUD_WARNING_TICKETS_IND_90_DAYS+TICKETS_90_DAYS+LATE_RETURNS_IND_90_DAYS+BILLING_CHANGES_IND_90_DAYS
                  +ACCIDENTS_90_DAYS+DELINQUENCIES_90_DAYS+LOW_FUEL_TICKETS_90_DAYS+MINOR_DAMAGE_TICKETS_90_DAYS
                  +FRAUD_WARNING_TICKETS_90_DAYS+LATE_RETURNS_90_DAYS+BILLING_CHANGES_90_DAYS+YEAR_INC
                  ,data=member.clv.train
                  ,trControl=train_control
                  ,method="lm"
                  ,metric="RMSE")
model.rf <- train(LTV~ZIPFLEET+BUSINESS_SEGMENT+PLAN_CATEGORY+DiscountApplication+RP_FEE
                  +DiscountedMembership+NINETY_DAY_RESERVATIONS+NINETY_DAY_REVENUE+JoinMonth
                  +ACCIDENTS_IND_90_DAYS+DELINQUENCIES_IND_90_DAYS+LOW_FUEL_TICKETS_IND_90_DAYS+MINOR_DAMAGE_TICKETS_IND_90_DAYS
                  +FRAUD_WARNING_TICKETS_IND_90_DAYS+TICKETS_90_DAYS+LATE_RETURNS_IND_90_DAYS+BILLING_CHANGES_IND_90_DAYS
                  +ACCIDENTS_90_DAYS+DELINQUENCIES_90_DAYS+LOW_FUEL_TICKETS_90_DAYS+MINOR_DAMAGE_TICKETS_90_DAYS
                  +FRAUD_WARNING_TICKETS_90_DAYS+LATE_RETURNS_90_DAYS+BILLING_CHANGES_90_DAYS+YEAR_INC
                  ,data=member.clv.train
                  ,trControl=train_control
                  ,method="ranger"
                  ,metric="RMSE")
model.svm <- train(LTV~ZIPFLEET+BUSINESS_SEGMENT+PLAN_CATEGORY+DiscountApplication+RP_FEE
                  +DiscountedMembership+NINETY_DAY_RESERVATIONS+NINETY_DAY_REVENUE+JoinMonth
                  +ACCIDENTS_IND_90_DAYS+DELINQUENCIES_IND_90_DAYS+LOW_FUEL_TICKETS_IND_90_DAYS+MINOR_DAMAGE_TICKETS_IND_90_DAYS
                  +FRAUD_WARNING_TICKETS_IND_90_DAYS+TICKETS_90_DAYS+LATE_RETURNS_IND_90_DAYS+BILLING_CHANGES_IND_90_DAYS
                  +ACCIDENTS_90_DAYS+DELINQUENCIES_90_DAYS+LOW_FUEL_TICKETS_90_DAYS+MINOR_DAMAGE_TICKETS_90_DAYS
                  +FRAUD_WARNING_TICKETS_90_DAYS+LATE_RETURNS_90_DAYS+BILLING_CHANGES_90_DAYS+YEAR_INC
                  ,data=member.clv.train
                  ,trControl=train_control
                  ,method="svmLinear2"
                  ,metric="RMSE")
model.xb <- train(LTV~ZIPFLEET+BUSINESS_SEGMENT+PLAN_CATEGORY+DiscountApplication+RP_FEE
                  +DiscountedMembership+NINETY_DAY_RESERVATIONS+NINETY_DAY_REVENUE+JoinMonth
                  +ACCIDENTS_IND_90_DAYS+DELINQUENCIES_IND_90_DAYS+LOW_FUEL_TICKETS_IND_90_DAYS+MINOR_DAMAGE_TICKETS_IND_90_DAYS
                  +FRAUD_WARNING_TICKETS_IND_90_DAYS+TICKETS_90_DAYS+LATE_RETURNS_IND_90_DAYS+BILLING_CHANGES_IND_90_DAYS
                  +ACCIDENTS_90_DAYS+DELINQUENCIES_90_DAYS+LOW_FUEL_TICKETS_90_DAYS+MINOR_DAMAGE_TICKETS_90_DAYS
                  +FRAUD_WARNING_TICKETS_90_DAYS+LATE_RETURNS_90_DAYS+BILLING_CHANGES_90_DAYS+YEAR_INC
                  ,data=member.clv.train
                  ,trControl=train_control
                  ,method="xgbLinear"
                  ,metric="RMSE")


print(model.xb)
summary(model.xb)

predictions <- predict(model.xb,newdata = data_test)

data_test$Prediction<-predictions
write_delim(data_test,"c:/users/babbenante/downloads/blake.txt",delim="\t")


######explicit xmboost below
data_train_compact<-data_train%>%
  select(LTV,ZIPFLEET,BUSINESS_SEGMENT,PLAN_CATEGORY,DiscountApplication,RP_FEE
         ,DiscountedMembership,ninety_day_reservations,ninety_day_revenue,JoinMonth
         ,ACCIDENTS,DELINQUENCIES,LOW_FUEL_TICKETS,MINOR_DAMAGE_TICKETS
         ,FRAUD_WARNING_TICKETS,TICKETS,LATE_RETURNS,BILLING_CHANGES)

sparse_matrix <- sparse.model.matrix(LTV ~ .-1, data = data_train_compact)
output_vector <- data_train_compact[,1]

data_test_compact<-data_test%>%
  select(LTV,ZIPFLEET,BUSINESS_SEGMENT,PLAN_CATEGORY,DiscountApplication,RP_FEE
         ,DiscountedMembership,ninety_day_reservations,ninety_day_revenue,JoinMonth
         ,ACCIDENTS,DELINQUENCIES,LOW_FUEL_TICKETS,MINOR_DAMAGE_TICKETS
         ,FRAUD_WARNING_TICKETS,TICKETS,LATE_RETURNS,BILLING_CHANGES)

sparse_matrix_test <- sparse.model.matrix(LTV ~ .-1, data = data_test_compact)



bstSparse <- xgboost(data = sparse_matrix
                     ,label=output_vector
                     ,booster='gblinear'
                     ,max.depth = 8
                     ,colsample_bytree=.5
                     ,min_child_weight=.8
                     ,nthread = 2
                     ,nround = 5
                     ,objective = "reg:linear"
                     ,eval_metric='mae')
y_pred <- predict(bstSparse, sparse_matrix_test)
data_test_compact$Predict<-y_pred
