

library(tidyverse)

        path<-"/Users/tg625/Documents/PDA/Directory/Results"

        d<-read.csv("/Users/tg625/Documents/PDA/Directory/Data/pannsexp_tgd.csv")

        dat<-as_tibble(d)

        removeTimepoints <-   dat %>%
                                    group_by(RedcapEventName) %>%
                                    filter(!str_detect(RedcapEventName, "arm_3")) %>%
                                    filter(!str_detect(RedcapEventName, "36")) %>%
                                    filter(!str_detect(RedcapEventName, "24")) %>%
                                    filter(!str_detect(RedcapEventName, "18")) %>%
                                    filter(!str_detect(RedcapEventName, "1_month")) %>%
                                    filter(!str_detect(RedcapEventName, "3_months")) %>%
                                    filter(!str_detect(RedcapEventName, "30")) 

          

        removeNon3TimePoints <- removeTimepoints %>% 
                                                group_by(PId) %>%
                                                #filter(RedcapEventName=="6_months_arm_1") %>%
                                                mutate(n=n()) %>%
                                                filter(n==3) 

          #removeNon3TimePoints %>% filter(str_detect(CSC_Start_date, "2022") | str_detect(AdminDate_LHS, "2022"))

            #names(removeNon3TimePoints)
            #unique(removeNon3TimePoints$n)

        removePwithMissingPANSS <- removeNon3TimePoints %>%
                                                    mutate(Inc_1_miss= case_when(P1=="888"|P1=="NULL"|
                                                                                P2=="888"|P2=="NULL"|
                                                                                P3=="888"|P3=="NULL"|
                                                                                N1=="888"|N1=="NULL"|
                                                                                N4=="888"|N4=="NULL"|
                                                                                N6=="888"|N6=="NULL"|
                                                                                G5=="888"|G5=="NULL"|
                                                                                G9=="888"|G9=="NULL" ~ "miss", .default = "no_miss")) %>% #filter(str_detect(CSC_Start_date, "2022") | str_detect(AdminDate_LHS, "2022")) %>% filter(str_detect(RedcapEventName, "12"))
                                                    filter(Inc_1_miss=="no_miss") %>% #filter(str_detect(CSC_Start_date, "2022") | str_detect(AdminDate_LHS, "2022")) %>% #filter(str_detect(RedcapEventName, "12"))
                                                    mutate(enrYear = case_when(str_detect(CSC_Start_date, "2014") | str_detect(AdminDate_LHS, "2014") ~ "2014",
                                                                               str_detect(CSC_Start_date, "2015") | str_detect(AdminDate_LHS, "2015")~ "2015",
                                                                               str_detect(CSC_Start_date, "2016") | str_detect(AdminDate_LHS, "2016")~ "2016",
                                                                               str_detect(CSC_Start_date, "2017") | str_detect(AdminDate_LHS, "2017")~ "2017",
                                                                               str_detect(CSC_Start_date, "2018") | str_detect(AdminDate_LHS, "2018")~ "2018",
                                                                               str_detect(CSC_Start_date, "2019") | str_detect(AdminDate_LHS, "2019")~ "2019", 
                                                                               str_detect(CSC_Start_date, "2020") | str_detect(AdminDate_LHS, "2020")~ "2020", 
                                                                               str_detect(CSC_Start_date, "2021") | str_detect(AdminDate_LHS, "2021")~ "2021", 
                                                                               str_detect(CSC_Start_date, "2022") | str_detect(AdminDate_LHS, "2022")~ "2022", 
                                                                               str_detect(CSC_Start_date, "2023") | str_detect(AdminDate_LHS, "2023")~ "2023")) %>% #filter(enrYear=="2022") %>% #filter(str_detect(RedcapEventName, "12"))
                                                    mutate(timepoint = case_when(str_detect(RedcapEventName, "baseline") ~ "Baseline",
                                                                                 str_detect(RedcapEventName, "6_months") ~ "6 Months",
                                                                                 str_detect(RedcapEventName, "12_months") ~ "12 Months")) %>% #filter(enrYear=="2022") %>% filter(timepoint=="12 Months")
                                                    mutate(P1=as.numeric(P1)) %>%
                                                    mutate(P2=as.numeric(P2)) %>%
                                                    mutate(P3=as.numeric(P3)) %>%
                                                    mutate(N1=as.numeric(N1)) %>%
                                                    mutate(N4=as.numeric(N4)) %>%
                                                    mutate(N6=as.numeric(N6)) %>%
                                                    mutate(G5=as.numeric(G5)) %>%
                                                    mutate(G9=as.numeric(G9)) %>% 
                                                    mutate(remitted = case_when(P1 <=3 & 
                                                                                P2<= 3 &
                                                                                P3<= 3 &
                                                                                N1<= 3 &
                                                                                N4<= 3 &
                                                                                N6<= 3 &
                                                                                G5<= 3 &
                                                                                G9<= 3 ~ 1, .default=0)) 
                                                                                

        #removePwithMissingPANSS %>%
                               # select(enrYear,timepoint, remitted) %>% filter(timepoint=="12 Months") %>% filter(enrYear=="2022")

    #Original plot currently on HONE with a few minor changes - axis labels, 
        OGplot <- removePwithMissingPANSS %>%
                                        mutate(enrYear=as.factor(enrYear)) %>%
                                        mutate(timepoint=as.factor(timepoint)) %>%
                                        group_by(enrYear, timepoint) %>%
                                        select(PId, enrYear, timepoint, remitted) %>%
                                        filter(remitted == 1) %>%
                                        summarise(totalRem=sum(remitted)) %>%
                                        #mutate(totalperYear=sum(totalRem)) %>%
                                       # mutate(perc=(totalRem/totalperYear)*100) %>% 
                                        arrange(desc(enrYear))  %>%
                                            ggplot(aes(x=enrYear, y=after_stat(count/sum(count)), fill=timepoint)) +
                                            annotate("ribbon", x = c(-Inf, Inf), ymin = 70, ymax = 85, 
                                                alpha = 0.2, fill = "#7BC16E") +
                                            geom_bar(position=position_dodge2(preserve = "single")) +
                                            geom_hline(aes(yintercept=70), linetype="dashed", colour="#7BC16E") + 
                                            geom_hline(aes(yintercept=85), linetype="dashed", colour="#26713D") + 
                                        #geom_rect(aes(ymin=70, ymax=85, xmin="2014", xmax="2023", fill="#7BC16E"), alpha=0.4) +
                                        scale_linewidth_manual(values=c(4,0.4,0.4)) +
                                            theme_minimal() +
                                            theme(legend.title=element_blank()) +
                                             labs( y="Percentage of indivdiuals that achieved\nremitted status", 
                                                   x = "Year of Enrollment\nto the STEP clinic",
                                                   title = "Annual STEP clinic symptom remission rates",
                                                   subtitle = "Percentage of indivdiuals who reached symptom remission* at 12 months post enrollment to the STEP service (red)\n
                                                                Remission status also provided for baseline (enrollment) and 6 monthly time points (orange)") 

          
        removeNon3TimePoints %>%
                                dplyr::select(pID, P1)
                                mutate()

                                        

         OGplot<- removePwithMissingPANSS %>%
                                        mutate(enrYear=as.numeric(enrYear)) %>%
                                        mutate(timepoint=as.factor(timepoint)) %>%
                                        group_by(enrYear, timepoint) %>%
                                        select(PId, enrYear, timepoint, remitted) %>%
                                        filter(remitted == 1) %>%
                                        summarise(totalRem=sum(remitted)) %>%
                                        mutate(totalperYear=sum(totalRem)) %>%
                                        mutate(perc=(totalRem/totalperYear)*100) %>% 
                                        mutate(timepoint=factor(timepoint, levels = c("Baseline", "6 Months", "12 Months"))) %>%
                                        #mutate(timepoint=reorder("Baseline", "6 Months", "12 Months")) %>%
                                        arrange(desc(enrYear)) %>%
                                        mutate(enrYear=as.factor(enrYear)) %>%
                                        #arrange(desc(timepoint)) %>% print(n=50)
                                        ggplot(aes(x=enrYear, y=perc, fill=timepoint, alpha=timepoint)) +
                                        geom_col(position=position_dodge2(preserve = "single")) +
                                         annotate("ribbon", x = c(-Inf, Inf), ymin = 70, ymax = 85, 
                                                alpha = 0.2, fill = "#7BC16E") +
                                            geom_hline(aes(yintercept=70), linetype="dashed", colour="#7BC16E") + 
                                            geom_hline(aes(yintercept=85), linetype="dashed", colour="#26713D") + 
                                        #geom_rect(aes(ymin=70, ymax=85, xmin="2014", xmax="2023", fill="#7BC16E"), alpha=0.4) +
                                            theme_minimal() +
                                            theme(legend.title=element_blank()) +
                                             labs( 
                                                    y="Percentage of indivdiuals that achieved\nremitted status", 
                                                    x = "Year of Enrollment\nto the STEP clinic",
                                                    title = "Annual symptom remission rates",
                                                    subtitle = "Percentage of indivdiuals who reached symptom remission* at 12 months post enrollment to the STEP service (red)\nRemission status also provided for baseline (enrollment) and 6 monthly time points (orange)") 
        
        OGplotCOlour<-OGplot + scale_fill_manual(values=c("#FFC685","#F8B15B","#BA4C23"))
        fadecolour<-OGplotCOlour + scale_alpha_manual(values=c(0.4, 0.6, 1))
        finalOGPLot<- fadecolour+ scale_y_continuous(labels = scales::percent_format(scale=1), limits = c(0, 100))

        ggsave(finalOGPLot, file=file.path(path, "PANSS_OGplotimproved.pdf"))        

        stackedlineplot<-removePwithMissingPANSS %>%             
                                        mutate(enrYear=as.numeric(enrYear)) %>%
                                        mutate(timepoint=as.factor(timepoint)) %>%
                                        group_by(enrYear, timepoint) %>%
                                        select(PId, remitted) %>% 
                                        #filter(remitted == "remitted") %>%
                                        summarise(totalRem=sum(remitted)) %>%
                                        mutate(totalperYear=sum(totalRem)) %>% 
                                        mutate(perc=(totalRem/totalperYear)*100) %>% #filter(timepoint=="12 Months") %>% #print(n=100)
                                        #mutate(ymin=c(75)) %>%
                                        #mutate(ymax=c(80)) %>%
                                        #mutate(fill=case_when(perc > ymin ~ TRUE, .default=FALSE)) %>%
                                        #arrange(desc(enrYear))  %>%
                                        ggplot(aes(x=enrYear, y=perc, group =timepoint)) +
                                        annotate("ribbon", x = c(-Inf, Inf), ymin = 70, ymax = 85, 
                                                alpha = 0.2, fill = "#7BC16E") +
                                        geom_point(aes(colour=timepoint), size=10) +
                                        geom_path(aes(colour=timepoint, linewidth=timepoint)) +
                                        geom_hline(aes(yintercept=70), linetype="dashed", colour="#7BC16E") + 
                                        geom_hline(aes(yintercept=85), linetype="dashed", colour="#26713D") + 
                                        #geom_rect(aes(ymin=70, ymax=85, xmin="2014", xmax="2023", fill="#7BC16E"), alpha=0.4) +
                                        scale_linewidth_manual(values=c(4,0.4,0.4)) +
                                           theme_minimal() +
                                            #theme(legend.position=element_blank()) +
                                            labs(   colour = "No. of months\npost enrollment",
                                                    linewidth = "No. of months\npost enrollment",
                                                    y="Percentage of indivdiuals that achieved\nremitted status", 
                                                    x = "Year of Enrollment\nto the STEP clinic",
                                                    title = "Annual symptom remission rates",
                                                    subtitle = "Percentage of indivdiuals who reached symptom remission* at 12 months post enrollment to the STEP service (red)\nRemission status also provided for baseline (enrollment) and 6 monthly time points (orange)") 
  
    
    stackedlineplotCOlour<-stackedlineplot + scale_colour_manual(values=c("#BA4C23", "#F5A040","#FFC685"))
    finalPLot<- stackedlineplotCOlour+ scale_y_continuous(labels = scales::percent_format(scale=1), limits = c(0, 100))
    
    ggsave(finalPLot, file=file.path(path, "PANSSaslineplot.pdf"))
 


 stackedlineplot_nobaseline<-removePwithMissingPANSS %>%             
                                        mutate(enrYear=as.numeric(enrYear)) %>%
                                        mutate(timepoint=as.factor(timepoint)) %>%
                                        group_by(enrYear, timepoint) %>%
                                        select(PId, remitted) %>% 
                                        filter(timepoint %in% c("6 Months", "12 Months")) %>%
                                        #filter(remitted == "remitted") %>%
                                        summarise(totalRem=sum(remitted)) %>%
                                        mutate(totalperYear=sum(totalRem)) %>% 
                                        mutate(perc=(totalRem/totalperYear)*100) %>%
                                        mutate(test=sum(P1)) %>%
                                         #filter(timepoint=="12 Months") %>% #print(n=100)
                                        #mutate(ymin=c(75)) %>%
                                        #mutate(ymax=c(80)) %>%
                                        #mutate(fill=case_when(perc > ymin ~ TRUE, .default=FALSE)) %>%
                                        #arrange(desc(enrYear))  %>%
                                        ggplot(aes(x=enrYear, y=perc, group =timepoint)) +
                                        annotate("ribbon", x = c(-Inf, Inf), ymin = 70, ymax = 85, 
                                                alpha = 0.2, fill = "#7BC16E") +
                                        geom_point(aes(colour=timepoint), size=10) +
                                        geom_path(aes(colour=timepoint, linewidth=timepoint)) +
                                        geom_hline(aes(yintercept=70), linetype="dashed", colour="#7BC16E") + 
                                        geom_hline(aes(yintercept=85), linetype="dashed", colour="#26713D") + 
                                        #geom_rect(aes(ymin=70, ymax=85, xmin="2014", xmax="2023", fill="#7BC16E"), alpha=0.4) +
                                        scale_linewidth_manual(values=c(4,0.4,0.4)) +
                                           theme_minimal() +
                                            #theme(legend.position=element_blank()) +
                                            labs(   colour = "No. of months\npost enrollment",
                                                    linewidth = "No. of months\npost enrollment",
                                                    y="Percentage of indivdiuals that achieved\nremitted status", 
                                                    x = "Year of Enrollment\nto the STEP clinic",
                                                    title = "Annual symptom remission rates",
                                                    subtitle = "Percentage of indivdiuals who reached symptom remission* at 12 months post enrollment to the STEP service (red)\nRemission status also provided for baseline (enrollment) and 6 monthly time points (orange)") 
  
    
    stackedlineplotCOlour_nobase<-stackedlineplot_nobaseline + scale_colour_manual(values=c("#BA4C23", "#FFC685"))
    finalPLot_nobase<- stackedlineplotCOlour_nobase+ scale_y_continuous(labels = scales::percent_format(scale=1), limits = c(0, 100))
    
    ggsave(finalPLot_nobase, file=file.path(path, "PANSSaslineplot_nobaseline.pdf"))


#F5A040))
#BA4C23
#F5A040


    stackedProportionalHis <- removePwithMissingPANSS %>%             
                                        mutate(enrYear=as.factor(enrYear)) %>%
                                        mutate(timepoint=as.factor(timepoint)) %>%
                                        group_by(enrYear, timepoint) %>%
                                        mutate(remitted = case_when(P1 <=3 & 
                                                                    P2<= 3 &
                                                                    P3<= 3 &
                                                                    N1<= 3 &
                                                                    N4<= 3 &
                                                                    N6<= 3 &
                                                                    G5<= 3 &
                                                                    G9<= 3 ~ 1, .default= 0)) %>% 
                                        select(PId, remitted) %>%
                                        #filter(remitted == "remitted") %>%
                                        group_by(enrYear, timepoint) %>%
                                        summarise(totalRem=sum(remitted)) %>%
                                        ggplot(aes(x=enrYear, fill=factor(timepoint, levels = c("0","6","12")))) +
  #geom_histogram(binwidth=500,stat="count")     
                                        geom_histogram(binwidth="3000", position="fill", stat="count")
                                        #geom_freqpoly(binwidth = 500, stat="count")

    stackedProportionalHis
``