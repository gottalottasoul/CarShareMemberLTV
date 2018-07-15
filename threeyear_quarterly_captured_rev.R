require(zipcarFunctions)

#if(!exists("detachAllPackages", mode="function")) source("C:\\Users\\babbenante\\OneDrive - Avis Budget Group\\My Stuff\\code\\utils\\utils.r")
#unload all previously loaded packages
detachAllPackages()

## clear the console of all variables
rm(list = ls())
## free up unused memory
gc()

#load required libraries
if (!require(readr)) {
  install.packages('readr') # read preformatted dates
  require(read)
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
if (!require(ggplot2)) {
  install.packages('ggplot2') # date manipulation
  require(ggplot2)
}
if (!require(scales)) {
  install.packages('scales') # date manipulation
  require(scales)
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


three_year_quarterly_revenue <- read_delim("C:/Users/babbenante/OneDrive - Avis Budget Group/My Stuff/Data/CLV/three_year_quarterly_revenue.txt"
                                           , "\t"
                                           , escape_double = FALSE
                                           , col_types = cols(JOIN_DATE = col_date(format = "%m/%d/%Y")
                                                              , LEAVE_DATE = col_date(format = "%m/%d/%Y")
                                                              , MEMBER_ID = col_character()), trim_ws = TRUE)

three_year_quarterly_fee_revenue <- read_delim("C:/Users/babbenante/OneDrive - Avis Budget Group/My Stuff/Data/CLV/three_year_quarterly_fee_revenue.txt"
                                               , "\t"
                                               , escape_double = FALSE
                                               , col_types = cols(MEMBER_ID = col_character())
                                               , trim_ws = TRUE)


three_year_quarterly_fee_revenue[is.na(three_year_quarterly_fee_revenue)]<-0

names(three_year_quarterly_fee_revenue)[14]<-'Y1Q1WAIVERREV'

three_year_rev=left_join(three_year_quarterly_revenue,three_year_quarterly_fee_revenue,by='MEMBER_ID')

three_year_rev<-three_year_rev[complete.cases(three_year_rev),]

three_year_rev$Y1Q1REV<-three_year_rev$Y1Q1REV+three_year_rev$Y1Q1APPREV+three_year_rev$Y1Q1FEEREV+three_year_rev$Y1Q1WAIVERREV
three_year_rev$Y1Q2REV<-three_year_rev$Y1Q2REV+three_year_rev$Y1Q2APPREV+three_year_rev$Y1Q2FEEREV+three_year_rev$Y1Q2WAIVERREV
three_year_rev$Y1Q3REV<-three_year_rev$Y1Q3REV+three_year_rev$Y1Q3APPREV+three_year_rev$Y1Q3FEEREV+three_year_rev$Y1Q3WAIVERREV
three_year_rev$Y1Q4REV<-three_year_rev$Y1Q4REV+three_year_rev$Y1Q4APPREV+three_year_rev$Y1Q4FEEREV+three_year_rev$Y1Q4WAIVERREV
three_year_rev$Y2Q1REV<-three_year_rev$Y2Q1REV+three_year_rev$Y2Q1APPREV+three_year_rev$Y2Q1FEEREV+three_year_rev$Y2Q1WAIVERREV
three_year_rev$Y2Q2REV<-three_year_rev$Y2Q2REV+three_year_rev$Y2Q2APPREV+three_year_rev$Y2Q2FEEREV+three_year_rev$Y2Q2WAIVERREV
three_year_rev$Y2Q3REV<-three_year_rev$Y2Q3REV+three_year_rev$Y2Q3APPREV+three_year_rev$Y2Q3FEEREV+three_year_rev$Y2Q3WAIVERREV
three_year_rev$Y2Q4REV<-three_year_rev$Y2Q4REV+three_year_rev$Y2Q4APPREV+three_year_rev$Y2Q4FEEREV+three_year_rev$Y2Q4WAIVERREV
three_year_rev$Y3Q1REV<-three_year_rev$Y3Q1REV+three_year_rev$Y3Q1APPREV+three_year_rev$Y3Q1FEEREV+three_year_rev$Y3Q1WAIVERREV
three_year_rev$Y3Q2REV<-three_year_rev$Y3Q2REV+three_year_rev$Y3Q2APPREV+three_year_rev$Y3Q2FEEREV+three_year_rev$Y3Q2WAIVERREV
three_year_rev$Y3Q3REV<-three_year_rev$Y3Q3REV+three_year_rev$Y3Q3APPREV+three_year_rev$Y3Q3FEEREV+three_year_rev$Y3Q3WAIVERREV
three_year_rev$Y3Q4REV<-three_year_rev$Y3Q4REV+three_year_rev$Y3Q4APPREV+three_year_rev$Y3Q4FEEREV+three_year_rev$Y3Q4WAIVERREV
three_year_rev$TotalRev=three_year_rev$Y1Q1REV+three_year_rev$Y1Q2REV+three_year_rev$Y1Q3REV+three_year_rev$Y1Q4REV+
  three_year_rev$Y2Q1REV+three_year_rev$Y2Q2REV+three_year_rev$Y2Q3REV+three_year_rev$Y2Q4REV+
  three_year_rev$Y3Q1REV+three_year_rev$Y3Q2REV+three_year_rev$Y3Q3REV+three_year_rev$Y3Q4REV

three_year_rev_compact<-three_year_rev %>% 
  ungroup(.) %>% 
#  filter(MEMBER_ID==763561132) %>% 
  select(MEMBER_ID,BUSINESS_SEGMENT,PLAN_CATEGORY,ZIPFLEET,JOIN_DATE,TotalRev
         ,Y1Q1REV,Y1Q2REV,Y1Q3REV,Y1Q4REV
         ,Y2Q1REV,Y2Q2REV,Y2Q3REV,Y2Q4REV
         ,Y3Q1REV,Y3Q2REV,Y3Q3REV,Y3Q4REV) %>% 
  gather(measure.quarter,quarterly.rev,Y1Q1REV:Y3Q4REV)%>%
  group_by(MEMBER_ID) %>% 
  arrange(MEMBER_ID,measure.quarter) %>% 
  mutate(RunningRev=cumsum(quarterly.rev)) %>% 
  mutate(RevFulfilled= ifelse(TotalRev==0,1,RunningRev/TotalRev)) %>% 
  mutate(RevFulfilled = ifelse(RevFulfilled>1,1,RevFulfilled))

bus.seg.summary<-three_year_rev_compact %>% 
  group_by(BUSINESS_SEGMENT,measure.quarter) %>% 
  summarise(RunningRev=sum(RunningRev,na.rm=TRUE)
            ,TotalRev=sum(TotalRev,na.rm=TRUE))

join.year.summary<-three_year_rev_compact %>% 
  mutate(JoinYear=year(JOIN_DATE)) %>% 
  group_by(JoinYear,measure.quarter) %>% 
  summarise(RunningRev=sum(RunningRev,na.rm=TRUE)
            ,TotalRev=sum(TotalRev,na.rm=TRUE))


bus.seg.summary.2<-three_year_rev_compact %>% 
  group_by(BUSINESS_SEGMENT,measure.quarter) %>% 
  summarise(avg.mem.full=mean(RevFulfilled)
            ,med.mem.full=median(RevFulfilled))

join.year.summary.2<-three_year_rev_compact %>% 
  mutate(JoinYear=year(JOIN_DATE)) %>% 
  group_by(JoinYear,measure.quarter) %>% 
  summarise(avg.mem.full=mean(RevFulfilled)
            ,med.mem.full=median(RevFulfilled))

bus.seg.summary.3<-three_year_rev_compact %>% 
  group_by(BUSINESS_SEGMENT,measure.quarter) %>% 
  summarise(completed.mem=sum(RevFulfilled==1)
            ,total.mem=n())

join.year.summary.3<-three_year_rev_compact %>% 
  mutate(JoinYear=year(JOIN_DATE)) %>% 
  group_by(JoinYear,measure.quarter) %>%
  summarise(completed.mem=sum(RevFulfilled==1)
            ,total.mem=n())


blake.temp<-three_year_rev_compact[which(three_year_rev_compact$BUSINESS_SEGMENT=='Business' & three_year_rev_compact$measure.quarter=='Y3Q1REV'),]
blake.temp<-three_year_rev[which(three_year_rev$MEMBER_ID=='763561132'),]


unique(MemberActivitySpan$CURRENT_STATUS)

blake.temp<-MemberActivitySpan %>% 
  filter(CURRENT_STATUS %in% c('approved','suspended')) %>% 
  mutate(JoinYear=year(JOIN_DATE)
         ,StartDate=rollback(JOIN_DATE,roll_to_first=TRUE,preserve_hms=FALSE) 
         ,ActivitySpan = ifelse(is.na(MOST_RECENT_ACTIVITY),0,as.numeric(MOST_RECENT_ACTIVITY-StartDate))
         ,ActivityMonths =ceiling(blake.temp$ActivitySpan/30)) %>% 
  group_by(JoinYear,CURRENT_STATUS) %>% 
  summarise(members=n()
            ,quant.10=quantile(ActivityMonths,probs=.1)
            ,quant.20=quantile(ActivityMonths,probs=.2)
            ,quant.30=quantile(ActivityMonths,probs=.3)
            ,quant.40=quantile(ActivityMonths,probs=.4)
            ,quant.50=quantile(ActivityMonths,probs=.5)
            ,quant.60=quantile(ActivityMonths,probs=.6)
            ,quant.70=quantile(ActivityMonths,probs=.7)
            ,quant.80=quantile(ActivityMonths,probs=.8)
            ,quant.90=quantile(ActivityMonths,probs=.9))