library(scales)
three_year_revenue<-read_delim("c:/users/babbenante/documents/My Stuff/Data/CLV/three_year_revenue.txt"
                               ,"\t"
                               ,col_types = cols(col_character() #join cohort
                                                 ,col_character() #Mem ID
                                                 ,col_character() #Zipfleet                                                
                                                 ,col_character() #Segment
                                                 ,col_number() #app fee
                                                 ,col_number() #ann fee
                                                 ,col_date("%m/%d/%Y") #join date
                                                 ,col_date("%m/%d/%Y") #leave date
                                                 ,col_number() #res
                                                 ,col_number() #rev
                                                 ,col_number() #rev yr 1
                                                 ,col_number() #rev yr 2
                                                 ,col_number() #rev yr 3
                               )

consumer.ltv<-three_year_revenue%>%
  mutate(join.year=year(JOIN_DATE)
         ,tenure=as.numeric(ifelse(is.na(LEAVE_DATE),Sys.Date()-JOIN_DATE,LEAVE_DATE-JOIN_DATE)))%>%
  group_by(BUSINESS_SEGMENT,join.year)%>%
  summarise(members=n()
            ,year.1=dollar(mean(YEARONEREV))
            ,year.2=dollar(mean(YEARTWOREV))
            ,year.3=dollar(mean(YEARTHREEREV))
            ,join.rev=dollar(mean(JOIN_APP_FEE))
            ,ann.fee.rev=dollar(mean(JOIN_ANNUAL_FEE))
            ,avg.tenure=round(mean(tenure)/365,1))



names(Email_Unique_Opens_2016)[2]<-'Opens'
email_open_summary<-Email_Unique_Opens_2016%>%
  group_by(Opens)%>%
  summarise(members=n())


names(Email_Unique_Clicks_2016)[2]<-'Clicks'
email_click_summary<-Email_Unique_Clicks_2016%>%
  group_by(Clicks)%>%
  summarise(members=n())
