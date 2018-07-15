
three_year_quarterly_revenue <- read_delim("C:/Users/babbenante/OneDrive - Avis Budget Group/My Stuff/Data/CLV/three_year_quarterly_revenue.txt"
                                           ,"\t"
                                           , escape_double = FALSE
                                           , col_types = cols(JOIN_DATE = col_date(format = "%m/%d/%Y")
                                                              ,LEAVE_DATE = col_date(format = "%m/%d/%Y")
                                                              ,MEMBER_ID = col_character())
                                           , trim_ws = TRUE)

three_year_quarterly_fee_revenue <- read_delim("C:/Users/babbenante/OneDrive - Avis Budget Group/My Stuff/Data/CLV/three_year_quarterly_fee_revenue.txt"
                                               , "\t"
                                               , escape_double = FALSE
                                               , col_types = cols(MEMBER_ID = col_character()
                                                                  , Y1Q1APPREV = col_double(), Y1Q1WAIVERREV = col_double()
                                                                  , Y1Q2APPREV = col_double(), Y1Q2WAIVERREV = col_double()
                                                                  , Y1Q3APPREV = col_double(), Y1Q3FEEREV = col_double()
                                                                  , Y1Q3WAIVERREV = col_double(), Y1Q4APPREV = col_double()
                                                                  , Y1Q4FEEREV = col_double(), Y1Q4WAIVERREV = col_double()
                                                                  , Y2Q1APPREV = col_double(), Y2Q1FEEREV = col_double()
                                                                  , Y2Q1WAIVERREV = col_double(), Y2Q2APPREV = col_double()
                                                                  , Y2Q2FEEREV = col_double(), Y2Q2WAIVERREV = col_double()
                                                                  , Y2Q3APPREV = col_double(), Y2Q3FEEREV = col_double()
                                                                  , Y2Q3WAIVERREV = col_double(), Y2Q4APPREV = col_double()
                                                                  , Y2Q4FEEREV = col_double(), Y2Q4WAIVERREV = col_double()
                                                                  , Y3Q1APPREV = col_double(), Y3Q1FEEREV = col_double()
                                                                  , Y3Q1WAIVERREV = col_double(), Y3Q2APPREV = col_double()
                                                                  , Y3Q2FEEREV = col_double(), Y3Q2WAIVERREV = col_double()
                                                                  , Y3Q3APPREV = col_double(), Y3Q3FEEREV = col_double()
                                                                  , Y3Q3WAIVERREV = col_double(), Y3Q4APPREV = col_double()
                                                                  , Y3Q4FEEREV = col_double(), Y3Q4WAIVERREV = col_double())
                                               , trim_ws = TRUE)


three_year_quarterly_fee_revenue[is.na(three_year_quarterly_fee_revenue)]<-0


three_year_rev=left_join(three_year_quarterly_revenue,three_year_quarterly_fee_revenue,by='MEMBER_ID')


three_year_rev$LEAVE_DATE<- ifelse(is.na(three_year_rev$LEAVE_DATE),as.Date('2017-04-01'),three_year_rev$LEAVE_DATE)
three_year_rev[is.na(three_year_rev)]<-0

three_year_rev<-three_year_rev[complete.cases(three_year_rev),]


margin.wkday<-.40
margin.wkend<-.15
margin.wvr<-.8


three_year_rev$Y1Q1REV<-(three_year_rev$Y1Q1_WE_REV*margin.wkend)+(three_year_rev$Y1Q1_WD_REV*margin.wkday)+three_year_rev$Y1Q1APPREV+three_year_rev$Y1Q1FEEREV+(three_year_rev$Y1Q1WAIVERREV*margin.wvr)
three_year_rev$Y1Q2REV<-(three_year_rev$Y1Q2_WE_REV*margin.wkend)+(three_year_rev$Y1Q2_WD_REV*margin.wkday)+three_year_rev$Y1Q2APPREV+three_year_rev$Y1Q2FEEREV+(three_year_rev$Y1Q2WAIVERREV*margin.wvr)
three_year_rev$Y1Q3REV<-(three_year_rev$Y1Q3_WE_REV*margin.wkend)+(three_year_rev$Y1Q3_WD_REV*margin.wkday)+three_year_rev$Y1Q3APPREV+three_year_rev$Y1Q3FEEREV+(three_year_rev$Y1Q3WAIVERREV*margin.wvr)
three_year_rev$Y1Q4REV<-(three_year_rev$Y1Q4_WE_REV*margin.wkend)+(three_year_rev$Y1Q4_WD_REV*margin.wkday)+three_year_rev$Y1Q4APPREV+three_year_rev$Y1Q4FEEREV+(three_year_rev$Y1Q4WAIVERREV*margin.wvr)
three_year_rev$Y2Q1REV<-(three_year_rev$Y2Q1_WE_REV*margin.wkend)+(three_year_rev$Y2Q1_WD_REV*margin.wkday)+three_year_rev$Y2Q1APPREV+three_year_rev$Y2Q1FEEREV+(three_year_rev$Y2Q1WAIVERREV*margin.wvr)
three_year_rev$Y2Q2REV<-(three_year_rev$Y2Q2_WE_REV*margin.wkend)+(three_year_rev$Y2Q2_WD_REV*margin.wkday)+three_year_rev$Y2Q2APPREV+three_year_rev$Y2Q2FEEREV+(three_year_rev$Y2Q2WAIVERREV*margin.wvr)
three_year_rev$Y2Q3REV<-(three_year_rev$Y2Q3_WE_REV*margin.wkend)+(three_year_rev$Y2Q3_WD_REV*margin.wkday)+three_year_rev$Y2Q3APPREV+three_year_rev$Y2Q3FEEREV+(three_year_rev$Y2Q3WAIVERREV*margin.wvr)
three_year_rev$Y2Q4REV<-(three_year_rev$Y2Q4_WE_REV*margin.wkend)+(three_year_rev$Y2Q4_WD_REV*margin.wkday)+three_year_rev$Y2Q4APPREV+three_year_rev$Y2Q4FEEREV+(three_year_rev$Y2Q4WAIVERREV*margin.wvr)
three_year_rev$Y3Q1REV<-(three_year_rev$Y3Q1_WE_REV*margin.wkend)+(three_year_rev$Y3Q1_WD_REV*margin.wkday)+three_year_rev$Y3Q1APPREV+three_year_rev$Y3Q1FEEREV+(three_year_rev$Y3Q1WAIVERREV*margin.wvr)
three_year_rev$Y3Q2REV<-(three_year_rev$Y3Q2_WE_REV*margin.wkend)+(three_year_rev$Y3Q2_WD_REV*margin.wkday)+three_year_rev$Y3Q2APPREV+three_year_rev$Y3Q2FEEREV+(three_year_rev$Y3Q2WAIVERREV*margin.wvr)
three_year_rev$Y3Q3REV<-(three_year_rev$Y3Q3_WE_REV*margin.wkend)+(three_year_rev$Y3Q3_WD_REV*margin.wkday)+three_year_rev$Y3Q3APPREV+three_year_rev$Y3Q3FEEREV+(three_year_rev$Y3Q3WAIVERREV*margin.wvr)
three_year_rev$Y3Q4REV<-(three_year_rev$Y3Q4_WE_REV*margin.wkend)+(three_year_rev$Y3Q4_WD_REV*margin.wkday)+three_year_rev$Y3Q4APPREV+three_year_rev$Y3Q4FEEREV+(three_year_rev$Y3Q4WAIVERREV*margin.wvr)
three_year_rev$TotalRev=three_year_rev$Y1Q1REV+three_year_rev$Y1Q2REV+three_year_rev$Y1Q3REV+three_year_rev$Y1Q4REV+
  three_year_rev$Y2Q1REV+three_year_rev$Y2Q2REV+three_year_rev$Y2Q3REV+three_year_rev$Y2Q4REV+
  three_year_rev$Y3Q1REV+three_year_rev$Y3Q2REV+three_year_rev$Y3Q3REV+three_year_rev$Y3Q4REV

lower.quantile<-quantile(three_year_rev$TotalRev,probs=.25)
upper.quantile<-quantile(three_year_rev$TotalRev,probs=.75)
inter.quantile<-upper.quantile-lower.quantile
lower.limit=lower.quantile-(inter.quantile*1.5)
upper.limit=upper.quantile+(inter.quantile*1.5)
three_year_rev$outlier<-ifelse(three_year_rev$TotalRev>upper.limit,1,0)

three_year_rev[which(three_year_rev$Y1Q1_WE_REV< 0 | three_year_rev$Y1Q2_WE_REV< 0 | three_year_rev$Y1Q3_WE_REV< 0 | three_year_rev$Y1Q4_WE_REV< 0 |
                       three_year_rev$Y2Q1_WE_REV< 0 | three_year_rev$Y2Q2_WE_REV< 0 | three_year_rev$Y2Q3_WE_REV< 0 | three_year_rev$Y2Q4_WE_REV< 0 |
                       three_year_rev$Y3Q1_WE_REV< 0 | three_year_rev$Y3Q2_WE_REV< 0 | three_year_rev$Y3Q3_WE_REV< 0 | three_year_rev$Y3Q4_WE_REV< 0),]$outlier<-2


three_year_rev.filtered<-three_year_rev %>% 
  mutate(JoinYear=as.numeric(year(JOIN_DATE))) %>% 
  filter(outlier<2)



three_year_rev_compact<-three_year_rev.filtered %>% 
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
  mutate(JoinYear=year(JOIN_DATE)
         ,JoinMonth=month(JOIN_DATE)) %>% 
  group_by(BUSINESS_SEGMENT,JoinYear,measure.quarter) %>% 
  summarise(RunningRev=sum(RunningRev,na.rm=TRUE)
            ,TotalRev=sum(TotalRev,na.rm=TRUE))


ltv.summary<-three_year_rev.filtered %>% 
  mutate(JoinYear=year(JOIN_DATE)
         ,JoinMonth=month(JOIN_DATE)) %>% 
  group_by(JoinYear,BUSINESS_SEGMENT) %>% 
  summarise(members=n()
            ,rev.q1=sum(Y1Q1REV)
            ,rev.q5=sum(Y1Q1REV)+sum(Y1Q2REV)+sum(Y1Q3REV)+sum(Y1Q4REV)+sum(Y2Q1REV)
            ,rev=sum(TotalRev)
            ,ltv=rev/members)

write_delim(bus.seg.summary,"c:/users/babbenante/downloads/blake.txt",delim="\t")

join.year.summary<-three_year_rev_compact %>% 
  mutate(JoinYear=year(JOIN_DATE)) %>% 
  group_by(JoinYear,measure.quarter) %>% 
  summarise(RunningRev=sum(RunningRev,na.rm=TRUE)
            ,TotalRev=sum(TotalRev,na.rm=TRUE))

join.year.summary.2<-three_year_rev%>% 
  mutate(JoinYear=year(JOIN_DATE)
         ,JoinMonth=month(JOIN_MONTH)) %>% 
  group_by(BUSINESS_SEGMENT,JoinYear,JoinMonth) %>% 
  summarise(Members=n()
            ,rev=mean(TotalRev))




bus.year.summary<-three_year_rev_compact %>% 
  mutate(JoinYear=year(JOIN_DATE)
         ,JoinMonth=month(JOIN_DATE)) %>% 
  group_by(BUSINESS_SEGMENT,JoinMonth,JoinYear,measure.quarter) %>% 
  summarise(RunningRev=sum(RunningRev,na.rm=TRUE)
            ,TotalRev=sum(TotalRev,na.rm=TRUE))

member.clv.train<-three_year_rev%>%
  filter(JOIN_DATE<=as.Date('2013-12-31'))



member.clv.lm<-lm(TotalRev~ZIPFLEET+JOIN_YEAR+JOIN_MONTH+Y1Q1REV+Y1Q2REV+Y1Q3REV+Y1Q4REV+
                    Y2Q1REV+Y2Q2REV+Y2Q3REV+Y2Q4REV+
                    Y3Q1REV+Y3Q2REV+Y3Q3REV+Y3Q4REV
                  ,data=member.clv.train)
glance(member.clv.lm)
tidy(member.clv.lm)