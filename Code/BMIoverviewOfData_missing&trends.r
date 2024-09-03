library(patchwork)
library(tidyverse)
library(lubridate)

##################################################################
# read in data
##################################################################
dat<-read.csv("Data/data_wide_merged_0806.csv")
path<-"Figures"

dat$new_admission_date <- mdy(dat$new_admission_date)

##################################################################
# Missing data plots - clinic wise
##################################################################

#barchat showing %missing data at baseline, 6, and 12 mo's
missingDataX6moCat_bar <- dat %>%
        replace_na(list(m12_bmi = 0, m6_bmi = 0, new_bmi = 0)) %>%
        mutate(missingm12_bmi = m12_bmi==0) %>%
        mutate(missingm6_bmi = m6_bmi==0) %>%
        mutate(missing_new_bmi = new_bmi==0) %>%
        select(missingm12_bmi, missingm6_bmi, missing_new_bmi) %>%
        pivot_longer(missingm12_bmi:missing_new_bmi) %>%
        mutate(name=as.factor(name)) %>%
        mutate(totalN=nrow(.)) %>%
        group_by(name) %>%
        summarise(n=sum(value==TRUE)) %>%
        mutate(perc=(n/1206)*100) %>%
        ggplot(aes(x=name, y=perc, fill=name)) +
        geom_bar(stat="identity") +
        theme_classic() +
        ylim(0,100)

#runchart showing total missing data for 0, 6 and 12 mo's, for every year
mmissingWithTarget_linechart<- dat %>%
        replace_na(list(m12_bmi = 0, m6_bmi = 0, new_bmi = 0)) %>%
        mutate(missingm12_bmi = m12_bmi==0) %>%
        mutate(missingm6_bmi = m6_bmi==0) %>%
        mutate(missing_new_bmi = new_bmi==0) %>%
        select(subject_id, new_admission_date,  m12_bmi, m6_bmi, new_bmi, missingm12_bmi, missingm6_bmi, missing_new_bmi) %>%
        mutate(yearAdmission = as.numeric(format(new_admission_date,'%Y'))) %>%
        group_by(yearAdmission) %>%
        summarise(totMissing0 = sum(missing_new_bmi==TRUE), 
                totMissing6 = sum(missingm6_bmi==TRUE), 
                totMissing12 = sum(missingm12_bmi==TRUE), 
                totAdmissionxYear = n(),
                percMiss0 = totMissing0/totAdmissionxYear*100,
                percMiss6 = totMissing6/totAdmissionxYear*100,
                percMiss12 = totMissing12/totAdmissionxYear*100) %>%
        pivot_longer(percMiss0:percMiss12) %>% 
        ggplot(aes(x=yearAdmission, y=value)) +
        geom_line(aes(color=name), linewidth=2) +
        #geom_smooth(aes(color=name), se=F) +
        #geom_point(aes(color=name), size=3) +
        geom_point(size=3)+
        theme_classic() +
   #can choose to add in lines and/or target:
        annotate("ribbon", x = c(-Inf, Inf), ymin = 0, ymax = 15, 
        alpha = 0.2, fill = "#7BC16E") +
        geom_hline(aes(yintercept=0), linetype="dashed", colour="#7BC16E") + 
        geom_hline(aes(yintercept=15), linetype="dashed", colour="#26713D") +
        geom_hline(aes(yintercept=10), colour="#8a8a8a") +
        geom_hline(aes(yintercept=20),colour="#8a8a8a") +
        geom_hline(aes(yintercept=30), colour="#8a8a8a") +
        geom_hline(aes(yintercept=40), colour="#8a8a8a") +
        geom_hline(aes(yintercept=50), colour="#8a8a8a") +
        geom_hline(aes(yintercept=60), colour="#8a8a8a") +
        geom_hline(aes(yintercept=70), colour="#8a8a8a") +
        geom_hline(aes(yintercept=80), colour="#8a8a8a") +
        geom_hline(aes(yintercept=90), colour="#8a8a8a") 

##################################################################
# BMI summary plots - clinic wise
##################################################################

#######################################
# annual run charts
#######################################

#runchart showing trends in BMI data when missing data is removed 
avBMIexcludingMissing_linechart <- dat %>%
        replace_na(list(m12_bmi = 0, m6_bmi = 0, new_bmi = 0)) %>%
        mutate(missingm12_bmi = m12_bmi==0) %>%
        mutate(missingm6_bmi = m6_bmi==0) %>%
        mutate(missing_new_bmi = new_bmi==0) %>%
        filter(!missingm12_bmi==TRUE |!missingm6_bmi==TRUE | !missing_new_bmi==TRUE) %>%
        select(subject_id, new_admission_date,  m12_bmi, m6_bmi, new_bmi, missingm12_bmi, missingm6_bmi, missing_new_bmi) %>%
        mutate(yearAdmission = as.numeric(format(new_admission_date,'%Y'))) %>%
        group_by(yearAdmission) %>%
        summarise(avBMI0 = mean(new_bmi), 
                avBMI6 = mean(m6_bmi), 
                avBMI12 = mean(m12_bmi), 
                totAdmissionxYear = n()) %>%
        pivot_longer(avBMI0:avBMI12) %>% 
        ggplot(aes(x=yearAdmission, y=value)) +
        #geom_line(aes(color=name, linewidth=2)) +
        geom_smooth(aes(group=name, colour=name), se=F) +
        geom_point(size=2) +
        theme_classic() +
        geom_hline(aes(yintercept=10), colour="#8a8a8a") +
        geom_hline(aes(yintercept=20),colour="#8a8a8a") +
        geom_hline(aes(yintercept=30), colour="#8a8a8a") +
        geom_hline(aes(yintercept=40), colour="#8a8a8a") +
        geom_hline(aes(yintercept=50), colour="#8a8a8a") +
        geom_hline(aes(yintercept=60), colour="#8a8a8a") +
        geom_hline(aes(yintercept=70), colour="#8a8a8a") +
        geom_hline(aes(yintercept=80), colour="#8a8a8a") +
        geom_hline(aes(yintercept=90), colour="#8a8a8a") 

### insert chart with missing data N on too - e.g., missing bar in background

#######################################
# BMI descriptves 
#######################################

#pyramid plot showing the gender split in those >30 BMI at 0, 6 and 12 months
BMIgender_bar<- dat %>%
        replace_na(list(m12_bmi = 0, m6_bmi = 0, new_bmi = 0)) %>%
        mutate(missingm12_bmi = m12_bmi==0) %>%
        mutate(missingm6_bmi = m6_bmi==0) %>%
        mutate(missing_new_bmi = new_bmi==0) %>%
        filter(!missingm12_bmi==TRUE) %>%
        select(subject_id, new_admission_date, new_gender, m12_bmi, m6_bmi, new_bmi, missingm12_bmi, missingm6_bmi, missing_new_bmi) %>%
        pivot_longer(m12_bmi:new_bmi) %>% 
        group_by(name, new_gender) %>%
        summarise(pop = sum(value > 30), frac = (pop / nrow(.)))

nudge_fun <- function(df) {
  ifelse(df$new_gender == "Male", (sd(df$pop) / 3) * - 1, sd(df$pop) / 3)
}

xlimits <- c((length(BMI_gender$new_gender)*1.5) * -1, length(BMI_gender$new_gender)*1.5)

x2 <- BMIgender_bar %>%
  mutate(
         pop = ifelse(new_gender == "Male", pop * (-1), pop * 1),
         frac = ifelse(new_gender == "Male", frac * (-1), frac * 1),
         share = paste0(abs(round(frac * 100, 1)), "%"),
         identified_gender = as.factor(new_gender))


theme_set(theme_minimal())

pyramid_plot <- x2 %>%
  ggplot(data = ., aes(x = pop, y = name, label = share)) +
  geom_col(aes(fill = new_gender)) +
  geom_text(aes(label = share),
            position = position_nudge(x = nudge_fun(x2)),
            size = 3) +
  geom_vline(xintercept = 0, linetype = "dotted",
             color = "black", size = 1.5) +
  scale_fill_manual("", values = c("#829cb2", "#e36c33", "blue")) +
  scale_x_continuous("", breaks = scales::pretty_breaks(n = 6),
    labels = function(br) {
      ifelse(abs(br) >= 1000,
             paste0(abs(br) / 1000, "k"),
             abs(br))
    }
  ) +
  #labs(x = "No. of people", y = "Age Bands") +
  #xlim(xlimits) +
  theme(legend.position = "bottom", 
        legend.justification = "left",
        legend.text = element_text(size=8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_text(size=8),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())


#plot showing % of patients >30 BMI at baseline, 6, and 12 monhts
BMI_overVsUnder<- dat %>%
        replace_na(list(m12_bmi = 0, m6_bmi = 0, new_bmi = 0)) %>%
        mutate(missingm12_bmi = m12_bmi==0) %>%
        mutate(missingm6_bmi = m6_bmi==0) %>%
        mutate(missing_new_bmi = new_bmi==0) %>%
        filter(!missingm12_bmi==TRUE) %>%
        select(subject_id, new_admission_date, m12_bmi, m6_bmi, new_bmi, missingm12_bmi, missingm6_bmi, missing_new_bmi) %>%
        pivot_longer(m12_bmi:new_bmi) %>% 
        group_by(name) %>%
        mutate(Over30 = value>30) %>%
        summarise(pop = sum(value > 30), 
                  frac = (pop / nrow(.))*100,
                  Over30=Over30) %>%
        ggplot(aes(y=name, x=frac, fill=Over30)) +
        geom_bar(stat="identity") +
        theme_minimal() +
        xlim(0,100) +
        scale_fill_manual(values = alpha(c("grey", "red"), .4)) +
        annotate("ribbon", y = c(-Inf, Inf), xmin = 0, xmax = 10, 
        alpha = 0.2, fill = "#7BC16E") +
        #geom_bar(position=position_dodge2(preserve = "single")) +
        geom_vline(aes(xintercept=0), linetype="dashed", colour="#26713D") + 
        geom_vline(aes(xintercept=10), linetype="dashed", colour="#26713D") 



##################################################################
# BMI summary plots - Patient wise
##################################################################

  #plot showing andivdiduals patients BMI trajectories over 0,6,12 mo's 
    pwise_BMI<-dat %>%
                    mutate(yearAdmission = as.numeric(format(new_admission_date,'%Y'))) %>%
                    group_by(subject_id) %>%
                    select(subject_id, new_admission_date, yearAdmission, new_gender, m12_bmi, m6_bmi, new_bmi) %>%
                   pivot_longer(m12_bmi:new_bmi) %>% 
                   mutate(subject_id=as.factor(subject_id)) %>%
                   mutate(name=as.factor(name)) %>%
                    group_by(subject_id) %>%
                    filter(!value==0) %>%
                    ggplot(aes(x=name, y=value)) +
                    geom_point(aes(y=value)) +
                    geom_line(aes(group=subject_id)) +
                    theme_classic()
    pwise_BMI<-     pwise_BMI + scale_x_discrete(limits = c("new_bmi","m6_bmi","m12_bmi"))

##################################################################
#print
##################################################################

ggsave(missingDataX6moCat_bar, file=file.path(path,"BMI_missingDataX6moCat_bar.png"))
ggsave(mmissingWithTarget_linechart, file=file.path(path,"mmissingWithTarget_linechart.png"))
ggsave(avBMIexcludingMissing_linechart, file=file.path(path,"BMI_avBMIexcludingMissing_linechart.png"))
ggsave(pyramid_plot, file=file.path(path,"BMI_rgender_pyramid_plot.png"))
ggsave(BMI_overVsUnder, file=file.path(path,"BMI_overVsUnder.png"))
ggsave(pwise_BMI, file=file.path(path,"BMI_pwise_BMI.png"))

