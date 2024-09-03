       
####       
        library(tidyverse)

        d<-read.csv("/Users/tonigibbs-dean/Downloads/PanssALL.csv")

        dat<-as_tibble(d)

############################################################################
#n people missing 1 varialbe at different overall + a time points
#############################################################################

x<-   dat %>%
        group_by(Redcap_event_name) %>%
        filter(!str_detect(Redcap_event_name, "arm_3")) %>%
        filter(!str_detect(Redcap_event_name, "36")) %>%
        filter(!str_detect(Redcap_event_name, "24")) %>%
        filter(!str_detect(Redcap_event_name, "18")) %>%
        filter(!str_detect(Redcap_event_name, "1_month")) %>%
        filter(!str_detect(Redcap_event_name, "3_months")) %>%
        filter(!str_detect(Redcap_event_name, "30")) %>%
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