##################################################################################################################################
#packages /read in data
##################################################################################################################################
        set.seed(1)

        library(forcats)
        library(patchwork)
        library(tidyverse)


        dat<-read.csv("/Users/tonigibbs-dean/Downloads/TG.csv")
        dattib<-as_tibble(dat) 
    
##################################################################################################################################
# Make plot describing BMI trajectories across time for STEP ARM 6
##################################################################################################################################
    #NB: STEP arm 1 appears to have no data for BMI or weight

    #create df with only STEP arm 6
    pwise_BMI<-   dattib %>%
                    group_by(Record.ID) %>%
                    filter(!Event.Name %in% c("1 Month (Arm 1: STEP-ED)" ,"6 Months (Arm 1: STEP-ED)", "12 Months (Arm 1: STEP-ED)")) %>% 
                    mutate(Event.Name=recode(Event.Name, 
                                        "Baseline (Arm 6: STEP-LHS)" = "Baseline",
                                        "6 Months (Arm 6: STEP-LHS)"  = "6mo",
                                        "12 Months (Arm 6: STEP-LHS)" = "12mo",
                                        "18 Months (Arm 6: STEP-LHS)" = "18mo",
                                        "24 Months (Arm 6: STEP-LHS)" = "24mo",
                                        "30 Months (Arm 6: STEP-LHS)" = "30mo",
                                        "36 Months (Arm 6: STEP-LHS)" = "36mo")) 
    #some BMI data listed as 0, so changed to NA
    pwise_BMI$BMI[pwise_BMI$BMI == 0] <- NA

    #plot capturing BMI for each individual at different time points 
    BMIplot<-pwise_BMI %>%
                    ggplot(aes(x=fct_inorder(Event.Name), y=BMI)) +
                    geom_point() +
                    geom_line(aes(group=Record.ID)) +
                    theme_classic()
    
    #working out missing data % for each time point - can be improved
    Perc_missing<-   pwise_BMI %>%
                            group_by(Event.Name) %>%
                            filter(!Event.Name %in% c("1 Month (Arm 1: STEP-ED)" ,"6 Months (Arm 1: STEP-ED)", "12 Months (Arm 1: STEP-ED)")) %>%
                            pivot_wider(names_from="Event.Name", 
                                                values_from=c("Data.Access.Group":"BMI")) %>%
                            dplyr::select(BMI_Baseline, BMI_6mo, BMI_12mo, BMI_18mo, BMI_24mo, BMI_30mo, BMI_36mo) %>% summary
            
    sum(is.na(Perc_missing$BMI)) 
    pwise_BMI %>% summary
        #pwise_BMI[pwise_BMI == NA] <- 0

##################################################################################################################################
# Make plot describing Weight trajectories across time for STEP ARM  6
##################################################################################################################################

        pwise_Weight<-dattib %>%
                    group_by(Record.ID) %>%
                    filter(!Event.Name %in% c("1 Month (Arm 1: STEP-ED)" ,"6 Months (Arm 1: STEP-ED)", "12 Months (Arm 1: STEP-ED)")) %>% 
                    mutate(Event.Name=recode(Event.Name, 
                                        "Baseline (Arm 6: STEP-LHS)" = "Baseline",
                                        "6 Months (Arm 6: STEP-LHS)"  = "6mo",
                                        "12 Months (Arm 6: STEP-LHS)" = "12mo",
                                        "18 Months (Arm 6: STEP-LHS)" = "18mo",
                                        "24 Months (Arm 6: STEP-LHS)" = "24mo",
                                        "30 Months (Arm 6: STEP-LHS)" = "30mo",
                                        "36 Months (Arm 6: STEP-LHS)" = "36mo")) %>%
                # mutate(as.factor(Event.Name)) %>%
                    ggplot(aes(x=fct_inorder(Event.Name), y=Patient.Weight..in.lbs..)) +
                    geom_point(aes(y=Patient.Weight..in.lbs..)) +
                    geom_line(aes(group=Record.ID)) +
                    theme_classic()

    pwiseXtime_plots<- BMIplot / pwise_Weight
     ggsave(pwiseXtime_plots, file=file.path(path,"BMI_weight_plot1.pdf"))


##################################################################################################################################
# Pivot wider df - i.e., personwise
##################################################################################################################################
    x<-   dattib %>%
                #group_by(Visit.ID) %>%
                filter(!Event.Name %in% c("1 Month (Arm 1: STEP-ED)" ,"6 Months (Arm 1: STEP-ED)", "12 Months (Arm 1: STEP-ED)")) %>%
            # mutate(as.factor(Event.Name)) %>%
                pivot_wider(names_from="Event.Name", 
                                    values_from=c("Data.Access.Group":"BMI")) 
                ggplot(aes(x=Event.Name, y=BMI)) +
                geom_boxplot()
    #missing data person wise - i.e., exclude participants with missing data over x amount

    #missing data variable wise - exclude variables with more than x missing 