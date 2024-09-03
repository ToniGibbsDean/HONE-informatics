       
####       
        library(tidyverse)

        d<-read.csv("/Users/tg625/Documents/PDA/Directory/Data/pannsexp_tgd.csv")

        dat<-as_tibble(d)

 

############################################################################
#n people missing 1 varialbe at different overall + a time points
#############################################################################
    #remove superfluous info from the data - in time this will be automatic as Tej refines the exports to match the plots 
        FullPossibleData4Viz <-   dat %>%
                                    group_by(RedcapEventName) %>%
                                    filter(!str_detect(RedcapEventName, "arm_3")) %>%
                                    filter(!str_detect(RedcapEventName, "36")) %>%
                                    filter(!str_detect(RedcapEventName, "24")) %>%
                                    filter(!str_detect(RedcapEventName, "18")) %>%
                                    filter(!str_detect(RedcapEventName, "1_month")) %>%
                                    filter(!str_detect(RedcapEventName, "3_months")) %>%
                                    filter(!str_detect(RedcapEventName, "30")) 

    #now we have the exact full data, we can check we have the right number of people for each category on the x axis, in this case year 
           #what we expect to see according to the current plot
                #enr 2014: total n=22; 
                    #first we filter so we only have data for one year
        function(getNonMissings(arm, year)) {
                    
                    yr2014<-  FullPossibleData4Viz %>%
                                    group_by(CSC_Start_date) %>%
                                    filter(str_detect(CSC_Start_date, "2014")) 
                    #then we ask how many unique IDs are included in total
                    totalpossible<-length(unique(yr2014$PId)) #ans=22
                    #then we ask, given the rules for this viz, how much data is missing. 
                        #rule 1. can only include people with all dat
                        
                        missingDat_yearTime(timepoint)
                        
                            timepoint<-list(unique(yr2014$RedcapEventName))
                         
                        base2014<- yr2014 %>%  
                                                group_by(PId) %>%
                                                filter(RedcapEventName=="6_months_arm_1") %>%
                                                mutate(n=n()) %>%
                                                #filter(n==3) %>%
                                                #arrange(desc(PId)) %>%
                                                select(PId, RedcapId, n) %>%
                                                print(length(.))
                
                            


                
                #missing at baseline = ; missing at 6mo =; missing at 12mo=
        
        length(unique(FullPossibleData4Viz$PId))

        length(unique(dat$PId))
        
        
        %>%
        mutate(Inc_1_miss= case_when(P1=="888"|P1=="NULL"|
                                    P2=="888"|P2=="NULL"|
                                    P3=="888"|P3=="NULL"|
                                    N1=="888"|N1=="NULL"|
                                    N4=="888"|N4=="NULL"|
                                    N6=="888"|N6=="NULL"|
                                    G5=="888"|G5=="NULL"|
                                    G9=="888"|G9=="NULL" ~ "miss", .default = "no_miss")) %>%
        mutate_if(is.character, as.factor) %>%
        group_by(Inc_1_miss)# %>%
        #summarise(n())

#"12_months_arm_6" "baseline_arm_1"  "6_months_arm_1"  "12_months_arm_1" "baseline_arm_6"  "6_months_arm_6"  "baseline_arm_5" 
x %>%
    filter(Redcap_event_name=="6_months_arm_1") %>% summarise(n())


temp<- x %>%
                    #filter(Redcap_event_name=="12_months_arm_6") %>%
                    filter(Inc_1_miss=="miss")

                    length(unique(temp$patientid))


#to get the missing by P1: G9
    summary(x$G9) #etc 


#pivot if useful 

 y <- x %>% pivot_wider(names_from="Redcap_event_name",  
                        values_from=c("P1":"G9")) 


##quaterly contro, charts using funnel plots - time series horizontally and funnel plots - dynamically vary 