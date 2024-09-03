

##################################################################################################################################
#packages /read in data
##################################################################################################################################
        set.seed(1)

        library(forcats)
        library(patchwork)
        library(tidyverse)
        library(lubridate)


        dat<-read.csv("/Users/tg625/Documents/PDA/Directory/Data/4170_021424_2.csv")

        
        dattib<-as_tibble(dat) 
        dattib$todaysDate<-"2024-02-21"
        str(dattib)

        summaryDat<-dattib %>% 
                mutate(IsDischarged=as.factor(IsDischarged)) %>%
                mutate(date_enrol=as.Date(EnrollmentDate)) %>%
                      mutate(as.Date(todaysDate)) %>%
                      mutate(date_firstweight=as.Date(first_recorded_date)) %>%
                      mutate(date_recentweight = as.Date(recent_recorded_date)) %>%
                mutate(RANGE_firstweight_enrol = abs(difftime(date_enrol,date_firstweight, unit="day"))<30) %>%
                      mutate(RANGE_firstBMI_enrol = abs(difftime(date_enrol,first_recorded_BMI, unit="day"))<30) %>%
                      mutate(RANGE_recentweightLess3mo = abs(difftime(recent_recorded_date, todaysDate, unit="day"))<90) %>%
                       mutate(RANGE_recentweightLess1mo = abs(difftime(recent_recorded_date, todaysDate, unit="day"))<30) %>%
                      mutate(EnrolDateEqualtoRecentDate = abs(difftime(date_enrol, recent_recorded_date, unit="day"))==0) %>%
                mutate(firstweightwithinonemoneth=case_when(RANGE_firstweight_enrol == TRUE & !first_recorded_weight == 0 ~ 1, TRUE ~ 0)) %>%
                      mutate(firstBMIwithinonemoneth=case_when(RANGE_firstBMI_enrol == TRUE & !first_recorded_BMI == 0 ~ 1, TRUE ~ 0)) %>%
                      mutate(recentWeightinLast3mo_notenrolweight=case_when(RANGE_recentweightLess3mo == TRUE & EnrolDateEqualtoRecentDate == FALSE ~ 1, TRUE ~ 0)) %>%
                      mutate(RANGE_recentweightLess1mo=case_when(RANGE_recentweightLess1mo == TRUE & EnrolDateEqualtoRecentDate == FALSE ~ 1, TRUE ~ 0)) %>%
                      mutate(recentWeightinLast3mo=case_when(RANGE_recentweightLess3mo == TRUE ~ 1, TRUE ~ 0)) %>%
                      mutate(RANGE_recentweightLess1mo=case_when(RANGE_recentweightLess1mo == TRUE & EnrolDateEqualtoRecentDate == FALSE ~ 1, TRUE ~ 0)) %>%
                
                
        #% of peole with a first weight recorded within one month of enrollment (enrolled n=44/inc enrolled n=128 currently)   
                summaryDat %>%
                        #filter(IsDischarged %in% "No") %>%
                        #summarise(n_firstweightwithinonemoneth=sum(firstweightwithinonemoneth)) 
                        summarise(n_firstweightwithinonemoneth=sum(firstweightwithinonemoneth)/128*100) 
                        summarise(n_firstweightwithinonemoneth=sum(firstweightwithinonemoneth)/44*100) 

               #as above but for bmi - n =0
                    summaryDat %>%
                        #filter(IsDischarged %in% "No") %>%
                        #summarise(n_firstweightwithinonemoneth=sum(firstweightwithinonemoneth)) 
                        summarise(n_firstBMIwithinonemoneth=sum(firstBMIwithinonemoneth)/128*100) 
                        summarise(n_firstBMIwithinonemoneth=sum(firstBMIwithinonemoneth)/44*100) 
                
        #(only enrolled) No. of people with a weight recorded in the 3 months - and whose most recent weight isnt taken at enrollment ; n=10
                 summaryDat %>%
                        filter(IsDischarged %in% "No") %>%
                        #summarise(n_recentWeightinLast3mo_notenrolweight=sum(recentWeightinLast3mo_notenrolweight))
                        summarise(n_recentWeightinLast3mo_notenrolweight=sum(recentWeightinLast3mo_notenrolweight) /44*100)
                
                #as above but looking at last month
                summaryDat %>%
                        filter(IsDischarged %in% "No") %>%
                        summarise(n_RANGE_recentweightLess1mo=sum(RANGE_recentweightLess1mo))  /44*100)

        
        #Same as above but including those whose most recent weight is just enrolment        
                summaryDat %>%
                        summarise(recentWeightinLast3mo=sum(recentWeightinLast3mo)/44*100)


         
        
        percdata<- dattib %>%
                mutate(firstweight_absentVpres = !first_recorded_weight == 0) %>% 
                mutate(mostrecentweight_absentVpres = !recent._recorded_weight == 0) %>%
                mutate(firstBMIabsvpres = !first_recorded_BMI == 0) %>%
                mutate(recent._recorded_BMIabsvpred = !recent._recorded_BMI == 0) %>% 
                summarise(firstweight_absentVpres_perc = sum(firstweight_absentVpres)/126*100, 
                          mostrecentweight_absentVpres_perc=sum(mostrecentweight_absentVpres)/126*100,  
                          firstBMIabsvpres_perc = sum(firstBMIabsvpres)/126*100,
                          recent._recorded_BMI_perc = sum(recent._recorded_BMIabsvpred)/126*100) %>%

                t()


        test<-dattib %>% 
                mutate(new = sub("\\s+[^ ]+$", "", "recent_recorded_date"))



x                
                x <- as.data.frame(x)

                plot <- x %>%
                            ggplot(aes(y=V1, group=V1)) +
                            geom_bar()
plot


                rownames(x) <- paste0("x")
                x %>% pivot_longer(values_from="x") 
                as_tibble(x)

                x %>%
                    tibble::rownames_to_column() %>%  
                    pivot_longer(-"x")


x                
                %>%
                ggplot(aes(x=)) +
                geom_bar()
x

        x %>% 
        


names(x)
plot(barchart(x$firstweight_absentVpres))

x %>% ggplot(aes(x=V1)) +
        geom_bar()
