library(dplyr)
library(sp)
library(leaflet)
library(lubridate)
library(ggplot2)
library(purrr)
library(tidyverse)
library(stringr)
library(naniar)
library(reticulate)
library(proxy)
library(pracma)
library(reshape2)
library(readxl)
library(janitor)
library(tibble)
library(viridis)
library(scales)
library(blandr)
library(irr)
library(MASS)
library(emg)
library(blandr)
library(ggpubr)
library(ggpmisc)
library(smplot2)

###############################
# SET DIRECTORY AND READ FILE #
###############################

mydir<- "C:\\Users\\mcampi\\Desktop\\Hung&Sylvette\\data\\"

data_ear<- read_excel(paste(mydir, "data_paper_cVEMP.xlsx", sep = ""),
           sheet = 1, col_names = T) 


data_ear<- data_ear[, -c(20,21)]


################
#Missing values#  
################


vis_miss(data_ear)


data_ear2<- data_ear

colnames(data_ear2)[20] <-  "Aaratio"


data_ear2<- data_ear2 %>% 
              mutate(gender = if_else(`sex girls 2 boys 1` == 1, "male", "female"))


data_ear2<- data_ear2 %>% 
                mutate(Age = if_else(`age month` < 192, "child", "adult"))

vis_miss(data_ear2)

ncol(data_ear2)

data_ear2<- data_ear2[,-c(22,23)]


data_ear2$Age_Y = round(data_ear2$`age month`/12, digits = 3)



data_ear2 <- data_ear2 %>%
  mutate(New_Age_group = case_when(
    between(data_ear2$Age_Y, 0, 0.99) ~ 1,
    between(data_ear2$Age_Y, 0.99, 1.99) ~ 2,
    between(data_ear2$Age_Y, 1.99, 2.99) ~ 3,
    between(data_ear2$Age_Y, 2.99, 3.99) ~ 4,
    between(data_ear2$Age_Y, 3.99, 4.99) ~ 5,
    between(data_ear2$Age_Y, 4.99, 5.99) ~ 6,
    between(data_ear2$Age_Y, 5.99, 6.99) ~ 7,
    between(data_ear2$Age_Y, 6.99, 7.99) ~ 8,
    between(data_ear2$Age_Y, 7.99, 8.99) ~ 9,
    between(data_ear2$Age_Y, 8.99, 9.99) ~ 10,
    between(data_ear2$Age_Y, 9.99, 10.99) ~ 11,
    between(data_ear2$Age_Y, 10.99, 11.99) ~ 12,
    between(data_ear2$Age_Y, 11.99, 12.99) ~ 13,
    between(data_ear2$Age_Y, 12.99, 13.99) ~ 14,
    between(data_ear2$Age_Y, 13.99, 14.99) ~ 15,
    between(data_ear2$Age_Y, 14.99, 15.99) ~ 16,
    between(data_ear2$Age_Y, 15.99, 20.99) ~ 17,
    between(data_ear2$Age_Y, 20.99, 40.99) ~ 18,
    between(data_ear2$Age_Y, 40.99, 62) ~ 19
  ))


cont_groups = data_ear2 %>% group_by(New_Age_group)  %>% summarise(count = n())
cont_groups


data_modelling = data_ear2
data_modelling = data_modelling[-which(is.na(data_ear2$`PN/emg BC (amplitude ratio`)),]


subset_data_modelling = data_modelling[,c(1,2,3,4,6:17,21:25)]

vis_miss(subset_data_modelling)

prova = subset_data_modelling %>% group_by(New_Age_group)  %>% summarise(count = n())
prova #reproduce experiments on SAS

which(is.na(subset_data_modelling $`lat P BC`))

subset_data_modelling = subset_data_modelling[-c(327,328),] 

subset_data_modelling %>% group_by(New_Age_group)  %>% summarise(count = n())


subset_data_modelling %>% 
  count(gender,Age) %>% 
  mutate(percent = n/sum(n), n = n)


#check this again
subset_data_modelling %>% group_by(New_Age_group)  %>% summarise(count = n())

check_categories = subset_data_modelling %>% 
                        count(gender, New_Age_group) %>% 
                        mutate(percent = n/sum(n), n = n)
check_categories


final_sub_data_mod = subset_data_modelling[-c(71,72,129,130,171,172,233,234),] 

final_sub_data_mod %>% group_by(New_Age_group)  %>% summarise(count = n())

final_sub_data_mod %>% 
  count(gender, Age) %>% 
  mutate(percent = n/sum(n), ears = n, n = n/2)


####################################
#  Split the original data by ears #
####################################

data_leftright_split<- final_sub_data_mod %>% group_split(`1 =right ear, empty = left ear`) 

data_eright<- data_leftright_split[[1]]
data_eleft<- data_leftright_split[[2]]


###############
#  Histograms #
###############


#counf ot PEOPLE

df_hist1<- data_eright %>% 
  count(gender,Age) %>% 
  mutate(percent = n/sum(n), n = n)

ggplot(df_hist1, aes(x = Age, y = n, fill = gender)) + 
  geom_col(position = "stack") +
  theme_bw()  + 
  geom_label(aes(label = n) , position = "stack",
             color = "white", vjust = 1,show.legend = FALSE) + scale_fill_grey()



ggplot(df_hist1, aes(x = Age, y = percent, fill = gender)) + 
  geom_col(position = "stack") + 
  geom_label(aes(label = percent(percent)), position = "stack", color = "white", vjust = 1, show.legend = FALSE) +
  scale_y_continuous(labels = percent) +
  theme_bw() + scale_fill_grey()


#count of EARS

df_hist2<- final_sub_data_mod %>% 
  count(gender,Age) %>% 
  mutate(percent = n/sum(n), n = n)


ggplot(df_hist2, aes(x = Age, y = n, fill = gender)) + 
  geom_col(position = "stack") +
  theme_bw()  + 
  geom_label(aes(label = n) , position = "stack",
             color = "white", vjust = 1,show.legend = FALSE) + scale_fill_grey()


ggplot(df_hist2, aes(x = Age, y = percent, fill = gender)) + 
  geom_col(position = "stack") + 
  geom_label(aes(label = percent(percent)), position = "stack", color = "white", vjust = 1, show.legend = FALSE) +
  scale_y_continuous(labels = percent) +
  theme_bw()  + scale_fill_grey()


#Count of people by gender and groups --> IN THE PAPER

df_hist3<- final_sub_data_mod %>% 
  count(gender,New_Age_group) %>% 
  mutate(percent = n/sum(n), n = n)


df_hist4<- df_hist3[c(1:16,20:35),]
       
       
label_groups = c("< 1y.o.", "1y.o. < 2y.o.", "2y.o. < 3y.o.", 
                 "3y.o. < 4y.o.", "4y.o. < 5y.o.", 
                 "5y.o. < 6y.o.", "6y.o. < 7y.o.",
                 "7y.o. < 8y.o.", "8y.o. < 9y.o.",
                 "9y.o. < 10y.o.", "10y.o. < 11y.o.",
                 "11y.o. < 12y.o.", "12y.o. < 13y.o.",
                 "13y.o. < 14y.o.", "14y.o. < 15y.o.",
                 "15y.o. < 16y.o.", "16y.o. < 17y.o.",
                 "17y.o. < 18y.o.", "18y.o. < 19y.o.")

label_groups_child = c("<1", "1<2", "2<3", 
                 "3<4", "4<5", 
                 "5<6", "6<7",
                 "7<8", "8<9",
                 "9<10", "10<11",
                 "11<12", "12<13",
                 "13<14", "14<15",
                 "15<16")


df_hist3$color = c(rep("white", 19), rep("black", 19))

df_hist4$color = c(rep("white", 16), rep("black", 16))


ggplot(df_hist4, aes(x = New_Age_group, y = n, fill = gender)) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  theme_bw()  + 
  geom_label(aes(label = n) , position = position_stack(reverse = TRUE),
             color = df_hist4$color, vjust = 1,show.legend = FALSE)  +
  scale_fill_grey() + xlab("Age Intervals (years)") + ylab("Number of tested ears")+
  scale_x_discrete(limit = label_groups_child) +
  theme(axis.text.x = element_text(vjust=0.5, angle=45, size = 15),
        axis.title.y = element_text(size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom')



############
# Boxplots #  
############


box_pn_amp = final_sub_data_mod[,c(8,13,19)]
colnames(box_pn_amp) = c('AC', 'BC', 'Age')


bx1<- ggplot(melt(box_pn_amp, 
            id = c("Age")), 
       aes(x = variable, y = value)) +
  geom_boxplot( alpha=0.3) + 
  facet_wrap(~Age, scales="free_y", labeller= as_labeller(c("adult" = "ADULT", 
                                                            "child" = "CHILD"))) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.text.x = element_text(size = 14)) + ylab("")  + xlab("") 


box_emg_pn_amp = final_sub_data_mod[,c(10,16,19)]
colnames(box_emg_pn_amp) = c('AC', 'BC', 'Age')

bx2<- ggplot(melt(box_emg_pn_amp, 
            id = c("Age")), 
       aes(x = variable, y = value)) +
  geom_boxplot( alpha=0.3) + 
  facet_wrap(~Age, scales="free_y", labeller= as_labeller(c("adult" = "ADULT", 
                                                            "child" = "CHILD"))) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.text.x = element_text(size = 14)) + ylab("")  + xlab("") 

box_tresh = data_ear2[,c(18,19,23)]
colnames(box_tresh) = c('AC', 'BC', 'Age')

bx3<- ggplot(melt(box_tresh, 
            id = c("Age")), 
       aes(x = variable, y = value)) +
  geom_boxplot( alpha=0.3) + 
  facet_wrap(~Age, scales="free_y", labeller= as_labeller(c("adult" = "ADULT", 
                                                            "child" = "CHILD"))) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.text.x = element_text(size = 14)) + ylab("")  + xlab("") 


###########
# EMG uv  #
###########

box_emg_uv = final_sub_data_mod[,c(9,15,19)]
colnames(box_emg_uv) = c('AC', 'BC', 'Age')



bx4<- ggplot(melt(box_emg_uv, 
            id = c("Age")), 
       aes(x = variable, y = value)) +
  geom_boxplot( alpha=0.3) + 
  facet_wrap(~Age, scales="free_y", labeller= as_labeller(c("adult" = "ADULT", 
                                                            "child" = "CHILD"))) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.text.x = element_text(size = 14)) + ylab("")  + xlab("") 


figure <- ggarrange(bx1, bx4,bx2 , bx3,
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2,  align = "v") 

figure



#################
# SCATTER PLOTs #
#################


#AMPLITEDU RATIO
ggplot(data_ear2[,c(11,17)], aes(x=`PN/emg AC (amplitude ratio)`, 
                                 y=`PN/emg BC (amplitude ratio`)) + 
  geom_point()+
  geom_smooth(method=lm,  color = "black") + theme_bw() +
  ylab("PN/emg BC (amplitude ratio)")


ggplot(data_ear2[,c(11,17,23)], aes(x=`PN/emg AC (amplitude ratio)`, 
                                    y=`PN/emg BC (amplitude ratio`,
                                    shape=Age, color = Age, 
                                    linetype = Age)) + 
  geom_point(aes(fill = Age), size = 2.3) +
  scale_shape_manual(values = c(1, 17)) +
  geom_smooth(method=lm, 
              color = "black",
              aes(color = Age), se=F) + theme_bw()   +
  scale_colour_manual(values = c("adult"="black", "child"="black")) +
  ylab("PN/emg BC (amplitude ratio)") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = 'bottom',
        legend.title=element_blank(),
        axis.title.x = element_text( size = 15),
        axis.title.y = element_text(size = 15))  +
#  stat_poly_eq() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")))


ggplot(data_ear2[,c(11,17,23)], aes(x=`PN/emg AC (amplitude ratio)`, 
                                    y=`PN/emg BC (amplitude ratio`,
                                    shape=Age, color = Age)) + 
  geom_point(aes(fill = Age), size = 2.3) +
  scale_shape_manual(values = c(1, 17)) +
  geom_smooth(method=lm, 
              color = "black",
              aes(color = Age), se=F) + theme_bw()   +
  scale_colour_manual(values = c("adult"="black", "child"="black")) +
  ylab("PN/emg BC (amplitude ratio)") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = 'bottom',
        legend.title=element_blank(),
        axis.title.x = element_text( size = 15),
        axis.title.y = element_text(size = 15)) +
  #  stat_poly_eq() +
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label), sep = "*\", \"*")))









ggplot(data_ear2[,c(11,17,23)], aes(x=`PN/emg AC (amplitude ratio)`, 
                                    y=`PN/emg BC (amplitude ratio`,
                                    shape=Age, color = Age, 
                                    linetype = Age)) + 
  geom_point(aes(fill = Age), size = 2.3) +
  scale_shape_manual(values = c(1, 17))  +
  scale_colour_manual(values = c("adult"="black", "child"="black")) +
  ylab("BC amplitude ratio") +
  xlab("AC amplitude ratio") + 
  sm_statCorr(corr_method = 'spearman',
              legends = TRUE) +  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = 'none',
        legend.title=element_blank(),
        axis.title.x = element_text( size = 15),
         axis.title.y = element_text(size = 15)) 
#+
  # geom_text(x = 1.05, y = 3.99,
  #           label = "child",
  #           color = 'black') +
  # geom_text(x = 1.05, y = 4.20,
  #           label = "adult",
  #           color = 'black')


 










################
#  Statisstics #
################


df_stats_gender_Agegroup = (final_sub_data_mod[,c(6:16,18,21)]  %>% group_by(gender, New_Age_group ) %>%
                          summarise(across(everything(), list(mean = mean, 
                                                              sd = sd,
                                                              var = var,
                                                              min = min,
                                                              max = max,
                                                              IQR = IQR), na.rm = TRUE),
                                    .groups = 'drop')  %>%
                          as.data.frame())[,-c(1:2)]  %>% mutate_if(is.character, as.numeric) %>%
  mutate_if(is.numeric, round, digits=1)



########################
########################
##  STATISTICAL TESTS ##
########################
########################

#boxplots and statistical test

#ALL THE PAIRED DATA
ggplot(melt(final_sub_data_mod[,c(6:16,19)], id = "Age"), 
       aes(x = variable, y = value)) +
  geom_boxplot( alpha=0.3) + 
  facet_wrap(~ paste("Age: ", Age, sep = " "))  +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none") +ylab("") + xlab("Variables") 

#THRESHOLDS
ggplot(melt(data_ear2[,c(18,19,23)], id = "Age"), 
       aes(x = variable, y = value)) +
  geom_boxplot( alpha=0.3) + 
  facet_wrap(~ paste("Age: ", Age, sep = " "))  +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none") +ylab("") + xlab("Variables") 


#LATENCY
ggplot(melt(data_ear2[,c(7,8,12,13,23)], id = "Age"), 
       aes(x = variable, y = value)) +
  geom_boxplot( alpha=0.3) + 
  facet_wrap(~ paste( Age, sep = " "))  +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none") +ylab("") + xlab("") 

ggplot(melt(data_ear2[,c(7,8,12,13,23)], 
                  id = c("Age")), 
             aes(x = variable, y = value)) +
  geom_boxplot( alpha=0.3) + 
  facet_wrap(~Age, scales="free_y", labeller= as_labeller(c("adult" = "ADULT", 
                                                            "child" = "CHILD"))) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.text.x = element_text(size = 14)) + ylab("")  + xlab("") 




#AMPLITEDU RATIO
ggplot(melt(data_ear2[,c(11,17,23)], id = "Age"), 
       aes(x = variable, y = value)) +
  geom_boxplot( alpha=0.3) + 
  facet_wrap(~ paste("Age: ", Age, sep = " "))  +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none") +ylab("") + xlab("Variables") 


#OTHERS
ggplot(melt(data_ear2[,c(9,14,10,16,23)], id = "Age"),
       aes(x = variable, y = value)) +
  geom_boxplot( alpha=0.3) + 
  facet_wrap(~ paste("Age: ", Age, sep = " "))  +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position="none") +ylab("") + xlab("Variables") 



#################################################################
#  COMPARISON CHILDREN AND ADULTS  - for Amp. Ratio and Latency #
#################################################################


ampl_ratio<-data_ear2[,c(11,17,23)]  %>% group_split(Age)

wilcox.test(as.numeric(unlist(ampl_ratio[[1]][,1])),
            as.numeric(unlist(ampl_ratio[[2]][,1])), 
            alternative = "two.sided") #difference in AC amplitude ratio

wilcox.test(as.numeric(unlist(ampl_ratio[[1]][,2])),
            as.numeric(unlist(ampl_ratio[[2]][,2])), 
            alternative = "two.sided") #difference in BC amplitude ratio


LatenciesP<-data_ear2[,c(7,12,23)]  %>% group_split(Age)
LatenciesN<-data_ear2[,c(8,13,23)]  %>% group_split(Age)


wilcox.test(as.numeric(unlist(LatenciesP[[1]][,1])),
            as.numeric(unlist(LatenciesP[[2]][,1])), 
            alternative = "two.sided") #difference in AC LatenciesP

wilcox.test(as.numeric(unlist(LatenciesP[[1]][,2])),
            as.numeric(unlist(LatenciesP[[2]][,2])), 
            alternative = "two.sided") #difference in BC LatenciesP



wilcox.test(as.numeric(unlist(LatenciesN[[1]][,1])),
            as.numeric(unlist(LatenciesN[[2]][,1])), 
            alternative = "two.sided") #difference in AC LatenciesN

wilcox.test(as.numeric(unlist(LatenciesN[[1]][,2])),
            as.numeric(unlist(LatenciesN[[2]][,2])), 
            alternative = "two.sided") #difference in BC LatenciesN



####################################################
#  COMPARISON CHILDREN AND ADULTS  - for Threshold #
####################################################

tresh<- data_ear2[,c(18,19,23)] %>% group_split(Age)

wilcox.test(as.numeric(unlist(tresh[[1]][,1])),
            as.numeric(unlist(tresh[[2]][,1])), 
            alternative = "two.sided") #difference in AC thresholds

wilcox.test(as.numeric(unlist(tresh[[1]][,2])),
            as.numeric(unlist(tresh[[2]][,2])),
            alternative = "two.sided") #difference in BC thresholds


######################################################################
#  Paired T TEST, ie AC vs BC for Amp. Ratio, Thresholds and Latency #
######################################################################

#Amp. Ratio
wilcox.test(as.numeric(unlist(data_ear2[,c(11)])),
            as.numeric(unlist(data_ear2[,c(17)])), 
            alternative = "two.sided")

#Thresholds
wilcox.test(as.numeric(unlist(data_ear2[,c(18)])),
            as.numeric(unlist(data_ear2[,c(19)])), 
            alternative = "two.sided")

#Latency
wilcox.test(as.numeric(unlist(data_ear2[,c(7)])),
            as.numeric(unlist(data_ear2[,c(12)])), 
            alternative = "two.sided")

wilcox.test(as.numeric(unlist(data_ear2[,c(8)])),
            as.numeric(unlist(data_ear2[,c(13)])), 
            alternative = "two.sided")



######################################################################
# LEFT VS RIGHT - AMP. RATIO Paired T TEST + BLAND ALTMAN PLOT + ICC #
######################################################################

wilcox.test(as.numeric(unlist(data_eleft[,c(11)])),
            as.numeric(unlist(data_eright[,c(11)])), 
            alternative = "two.sided")


wilcox.test(as.numeric(unlist(data_eleft[,c(17)])),
            as.numeric(unlist(data_eright[,c(17)])), 
            alternative = "two.sided")

gg1 <- blandr.draw( as.numeric(unlist(data_eleft$`PN/emg AC (amplitude ratio)`)) , 
             as.numeric(unlist(data_eright$`PN/emg AC (amplitude ratio)`)),
             plotter="ggplot",
             plotTitle = "Bland-Altman plot for PN/emg AC Amp. Ratio",
             point_size = 2 )  

gg1_build <- ggplot_build(gg1) 

gg1_build$data[[12]][["fill"]] <- "grey0"
gg1_build$data[[13]][["fill"]] <- "grey44"
gg1_build$data[[14]][["fill"]] <- "grey100"

grid::grid.draw(ggplot_gtable(gg1_build))


gg2 <- blandr.draw( as.numeric(unlist(data_eleft$`PN/emg BC (amplitude ratio`)) ,
             as.numeric(unlist(data_eright$`PN/emg BC (amplitude ratio`)),
             plotter="ggplot",
             plotTitle = "Bland-Altman plot for PN/emg BC Amp. Ratio")


gg2_build <- ggplot_build(gg2)

gg2_build$data[[12]][["fill"]] <- "grey0"
gg2_build$data[[13]][["fill"]] <- "grey44"
gg2_build$data[[14]][["fill"]] <- "grey100"

grid::grid.draw(ggplot_gtable(gg2_build))




icc(cbind(data_eleft$`PN/emg AC (amplitude ratio)`,
          data_eright$`PN/emg AC (amplitude ratio)`), 
    model = "oneway", type = "consistency", unit = "single")

icc(cbind(data_eleft$`PN/emg AC (amplitude ratio)`,
          data_eright$`PN/emg AC (amplitude ratio)`), 
    model = "oneway", type = "consistency", unit = "single")


#################################
#################################
# the Royston and Wright method #
#################################
#################################

########################################
# Functions for plotting and prediction#
########################################

## Define function for ggplot2
stat_fun <- function(fun, args, mapping) {
  stat_function(fun = fun, args = args, mapping = mapping, n = 306, size = 1.5, alpha = 1)
}

# Define prediction function
PredFun <- function(x, model, data) {
  
  Age_M = data$Age_M
  
  ## Category indicator variable    
  indicator <- cut(x, 
                   breaks = c(-Inf,age_interval$Last_value_for_break))
  ## Variables for spline
  s1        <- pmax(Age_M -  age_interval$Last_value_for_break[1], 0)
  s2        <- pmax(Age_M -  age_interval$Last_value_for_break[2], 0)
  s3        <- pmax(Age_M -  age_interval$Last_value_for_break[3], 0)
  s4        <- pmax(Age_M -  age_interval$Last_value_for_break[4], 0)
  s5        <- pmax(Age_M -  age_interval$Last_value_for_break[5], 0)
  s6        <- pmax(Age_M -  age_interval$Last_value_for_break[6], 0)
  s7        <- pmax(Age_M -  age_interval$Last_value_for_break[7], 0)
  s8        <- pmax(Age_M -  age_interval$Last_value_for_break[8], 0)
  s9        <- pmax(Age_M -  age_interval$Last_value_for_break[9], 0)
  s10        <- pmax(Age_M -  age_interval$Last_value_for_break[10], 0)
  s11        <- pmax(Age_M -  age_interval$Last_value_for_break[11], 0)
  s12        <- pmax(Age_M -  age_interval$Last_value_for_break[12], 0)
  s13        <- pmax(Age_M -  age_interval$Last_value_for_break[13], 0)
  s14        <- pmax(Age_M -  age_interval$Last_value_for_break[14], 0)
  s15        <- pmax(Age_M -  age_interval$Last_value_for_break[15], 0)
  s16        <- pmax(Age_M -  age_interval$Last_value_for_break[16], 0)
  s17        <- pmax(Age_M -  age_interval$Last_value_for_break[17], 0)
  s18        <- pmax(Age_M -  age_interval$Last_value_for_break[18], 0)
  s19        <- pmax(Age_M -  age_interval$Last_value_for_break[19], 0)
  s20        <- pmax(Age_M -  age_interval$Last_value_for_break[20], 0)
  ## Predict
  predict(object = model, newdata = data.frame(x = x,
                                               indicator = indicator,
                                               s1 = s1, s2 = s2, s3 = s3, s4 = s4, s5 = s5,
                                               s6 = s6, s7 = s7, s8 = s8, s9 = s9, s10 = s10,
                                               s11 = s11, s12 = s12, s13 = s13, s14 = s14, s15 = s15,
                                               s16 = s16, s17 = s17, s18 = s18, s19 = s19, s20 = s20))
}




####################
# DATA PREPARATION #
####################


data_plot_AR_AC<- data.frame(final_sub_data_mod$`age month`,
                             final_sub_data_mod$`PN/emg AC (amplitude ratio)`,
                             final_sub_data_mod$New_Age_group,
                             final_sub_data_mod$Age_Y,
                             final_sub_data_mod$Age) 
colnames(data_plot_AR_AC) <- c("Age_M",
                               "Amplitude_Ratio_AC", 
                               "New_Age_group", 
                               "Age_Y",
                               "Age")
data_plot_AR_AC$gender<- final_sub_data_mod$gender
head(data_plot_AR_AC)
data_plot_AR_AC<- data_plot_AR_AC[-c(1,2), ]
head(data_plot_AR_AC)
vis_miss(data_plot_AR_AC)

data_plot_AR_BC<- data.frame(final_sub_data_mod$`age month`,
                             final_sub_data_mod$`PN/emg BC (amplitude ratio`,
                             final_sub_data_mod$New_Age_group,
                             final_sub_data_mod$Age_Y,
                             final_sub_data_mod$Age)  
colnames(data_plot_AR_BC) <- c("Age_M",
                               "Amplitude_Ratio_BC",
                               "Age_group",
                               "Age_Y",
                               "Age")
data_plot_AR_BC$gender<- final_sub_data_mod$gender
head(data_plot_AR_BC)
data_plot_AR_BC<- data_plot_AR_BC[-c(1,2), ]
head(data_plot_AR_BC)
vis_miss(data_plot_AR_BC)


to_remove1<- which(data_plot_AR_BC$Amplitude_Ratio_BC == 0)

data_plot_AR_AC<- data_plot_AR_AC[-to_remove1,]
data_plot_AR_BC<- data_plot_AR_BC[-to_remove1,]

############################
#BOXCOX TRANFORM IS APPLIED#
############################

b_AC <- boxcox(lm(data_plot_AR_AC$Amplitude_Ratio_AC ~ 1))
b_BC <- boxcox(lm(data_plot_AR_BC$Amplitude_Ratio_BC ~ 1))

# Exact lambda
lambda_AC <- b_AC$x[which.max(b_AC$y)] 
lambda_BC <- b_BC$x[which.max(b_BC$y)] 


new_a_exact_AC <- (data_plot_AR_AC$Amplitude_Ratio_AC ^ lambda_AC - 1) / lambda_AC
new_a_exact_BC <- (data_plot_AR_BC$Amplitude_Ratio_BC ^ lambda_BC - 1) / lambda_BC

data_plot_AR_AC$box_AC <- new_a_exact_AC
data_plot_AR_BC$box_BC <- new_a_exact_BC


####################################
####################################
#Fractional polynomial for the mean#
####################################
####################################

################
# ALL TOGETHER #
################

age_interval =  data_plot_AR_AC %>% group_by(New_Age_group) %>% summarise(Last_value_for_break = last(Age_M))

## Modify dataset
#1 AC
newdata_data_plot_AR_AC <- within(data_plot_AR_AC, {
  ## Change to lower cases
  x <- Age_M
  y <- Amplitude_Ratio_AC #box_AC
  ## Category indicator variable
  indicator <- cut(Age_M,
                   breaks = c(-Inf,age_interval$Last_value_for_break))
  ## Variables for spline
  s1        <- pmax(Age_M -  age_interval$Last_value_for_break[1], 0)
  s2        <- pmax(Age_M -  age_interval$Last_value_for_break[2], 0)
  s3        <- pmax(Age_M -  age_interval$Last_value_for_break[3], 0)
  s4        <- pmax(Age_M -  age_interval$Last_value_for_break[4], 0)
  s5        <- pmax(Age_M -  age_interval$Last_value_for_break[5], 0)
  s6        <- pmax(Age_M -  age_interval$Last_value_for_break[6], 0)
  s7        <- pmax(Age_M -  age_interval$Last_value_for_break[7], 0)
  s8        <- pmax(Age_M -  age_interval$Last_value_for_break[8], 0)
  s9        <- pmax(Age_M -  age_interval$Last_value_for_break[9], 0)
  s10        <- pmax(Age_M -  age_interval$Last_value_for_break[10], 0)
  s11        <- pmax(Age_M -  age_interval$Last_value_for_break[11], 0)
  s12        <- pmax(Age_M -  age_interval$Last_value_for_break[12], 0)
  s13        <- pmax(Age_M -  age_interval$Last_value_for_break[13], 0)
  s14        <- pmax(Age_M -  age_interval$Last_value_for_break[14], 0)
  s15        <- pmax(Age_M -  age_interval$Last_value_for_break[15], 0)
  s16        <- pmax(Age_M -  age_interval$Last_value_for_break[16], 0)
  s17        <- pmax(Age_M -  age_interval$Last_value_for_break[17], 0)
  s18        <- pmax(Age_M -  age_interval$Last_value_for_break[18], 0)
  s19        <- pmax(Age_M -  age_interval$Last_value_for_break[19], 0)
  s20        <- pmax(Age_M -  age_interval$Last_value_for_break[20], 0)
})


#2 BC
newdata_data_plot_AR_BC <- within(data_plot_AR_BC, {
  ## Change to lower cases
  x <- Age_M
  y <- Amplitude_Ratio_BC  #box_BC
  ## Category indicator variable
  indicator <- cut(Age_M,
                   breaks = c(-Inf,age_interval$Last_value_for_break))
  ## Variables for spline
  s1        <- pmax(Age_M -  age_interval$Last_value_for_break[1], 0)
  s2        <- pmax(Age_M -  age_interval$Last_value_for_break[2], 0)
  s3        <- pmax(Age_M -  age_interval$Last_value_for_break[3], 0)
  s4        <- pmax(Age_M -  age_interval$Last_value_for_break[4], 0)
  s5        <- pmax(Age_M -  age_interval$Last_value_for_break[5], 0)
  s6        <- pmax(Age_M -  age_interval$Last_value_for_break[6], 0)
  s7        <- pmax(Age_M -  age_interval$Last_value_for_break[7], 0)
  s8        <- pmax(Age_M -  age_interval$Last_value_for_break[8], 0)
  s9        <- pmax(Age_M -  age_interval$Last_value_for_break[9], 0)
  s10        <- pmax(Age_M -  age_interval$Last_value_for_break[10], 0)
  s11        <- pmax(Age_M -  age_interval$Last_value_for_break[11], 0)
  s12        <- pmax(Age_M -  age_interval$Last_value_for_break[12], 0)
  s13        <- pmax(Age_M -  age_interval$Last_value_for_break[13], 0)
  s14        <- pmax(Age_M -  age_interval$Last_value_for_break[14], 0)
  s15        <- pmax(Age_M -  age_interval$Last_value_for_break[15], 0)
  s16        <- pmax(Age_M -  age_interval$Last_value_for_break[16], 0)
  s17        <- pmax(Age_M -  age_interval$Last_value_for_break[17], 0)
  s18        <- pmax(Age_M -  age_interval$Last_value_for_break[18], 0)
  s19        <- pmax(Age_M -  age_interval$Last_value_for_break[19], 0)
  s20        <- pmax(Age_M -  age_interval$Last_value_for_break[20], 0)
})



####################
#Polynomial models #
####################
#1 AC
## Linear model
lmLinear_AC <- lm(formula = y ~ x, data = newdata_data_plot_AR_AC)
## Quadratic model
lmQuadratic_AC <- lm(formula = y ~ x + I(x^2), data = newdata_data_plot_AR_AC)
## Cubic model
lmCubic_AC <- lm(formula = y ~ x + I(x^2) + I(x^3), data = newdata_data_plot_AR_AC)
## Fractional model
lmFractional_AC <- lm(formula = y ~ x + I(x^2) + I(x^(1/2)), data = newdata_data_plot_AR_AC)


#1 BC
## Linear model
lmLinear_BC <- lm(formula = y ~ x, data = newdata_data_plot_AR_BC)
## Quadratic model
lmQuadratic_BC <- lm(formula = y ~ x + I(x^2), data = newdata_data_plot_AR_BC)
## Cubic model
lmCubic_BC <- lm(formula = y ~ x + I(x^2) + I(x^3), data = newdata_data_plot_AR_BC)
## Fractional model
lmFractional_BC <- lm(formula = y ~ x + I(x^2) + I(x^(1/2)), data = newdata_data_plot_AR_BC)


#################
## Create layers#
#################

layerPolyLinear_AC     <- stat_fun(fun = PredFun, 
                                args = list(model = lmLinear_AC, data = newdata_data_plot_AR_AC),     
                                mapping = aes(color = "Linear"))

layerPolyQuadratic_AC  <- stat_fun(fun = PredFun,
                                args = list(model = lmQuadratic_AC, data = newdata_data_plot_AR_AC), 
                                mapping = aes(color = "Quadratic"))

layerPolyCubic_AC      <- stat_fun(fun = PredFun, 
                                args = list(model = lmCubic_AC, data = newdata_data_plot_AR_AC),   
                                mapping = aes(color = "Cubic"))


layerPolyFractional_AC <- stat_fun(fun = PredFun,
                                args = list(model = lmFractional_AC, data = newdata_data_plot_AR_AC),
                                mapping = aes(color = "Fractional"))




layerPolyLinear_BC     <- stat_fun(fun = PredFun, 
                                   args = list(model = lmLinear_BC, data = newdata_data_plot_AR_BC),     
                                   mapping = aes(color = "Linear"))

layerPolyQuadratic_BC  <- stat_fun(fun = PredFun,
                                   args = list(model = lmQuadratic_BC, data = newdata_data_plot_AR_BC), 
                                   mapping = aes(color = "Quadratic"))

layerPolyCubic_BC      <- stat_fun(fun = PredFun, 
                                   args = list(model = lmCubic_BC, data = newdata_data_plot_AR_BC),   
                                   mapping = aes(color = "Cubic"))


layerPolyFractional_BC <- stat_fun(fun = PredFun,
                                   args = list(model = lmFractional_BC, data = newdata_data_plot_AR_BC),
                                   mapping = aes(color = "Fractional"))



## Create plot base
#1 AC
plotBasePoly_AC <- ggplot(data = newdata_data_plot_AR_AC,
                       mapping = aes(x = Age_M, y = Amplitude_Ratio_AC )) +
  layer(geom = "point",stat = "identity", position = "identity") +
  scale_x_continuous(breaks = seq(from = 0, 
                                  to = newdata_data_plot_AR_AC$Age_M[length(newdata_data_plot_AR_AC$Age_M)], 
                                  by = 120),
                     labels = paste(seq(from = 0, 
                                        to = newdata_data_plot_AR_BC$Age_M[length(newdata_data_plot_AR_BC$Age_M)], 
                                        by = 120)/12, sep = '') ) +     
  scale_color_discrete(name = "model",
                       breaks =  c("Linear","Quadratic","Cubic","Fractional")) +
  theme_bw()  +
  theme(legend.key = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.0, hjust=1),
        legend.position = 'none') + 
  ylab("Amplitude Ratio AC") + xlab("Age (years)")

#2 BC
plotBasePoly_BC <- ggplot(data = newdata_data_plot_AR_BC,
                          mapping = aes(x = Age_M, y = Amplitude_Ratio_BC )) +
  layer(geom = "point",stat = "identity", position = "identity") +
  scale_x_continuous(breaks = seq(from = 0, 
                                to = newdata_data_plot_AR_BC$Age_M[length(newdata_data_plot_AR_BC$Age_M)], 
                                by = 120),
                     labels = paste(seq(from = 0, 
                                        to = newdata_data_plot_AR_BC$Age_M[length(newdata_data_plot_AR_BC$Age_M)], 
                                        by = 120)/12, sep = '')) +     
  scale_color_discrete(name = "model",
                       breaks =  c("Linear","Quadratic","Cubic","Fractional")) +
  theme_bw()  +
  theme(legend.key = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.0, hjust=1),
        legend.position = 'none') + 
  ylab("Amplitude Ratio BC") + xlab("Age (years)")



## Plot together
#plotBasePoly_AC + layerPolyLinear_AC + layerPolyQuadratic_AC + layerPolyCubic_AC + layerPolyFractional_AC
plotBasePoly_AC + layerPolyFractional_AC

#plotBasePoly_BC + layerPolyLinear_BC + layerPolyQuadratic_BC + layerPolyCubic_BC + layerPolyFractional_BC
plotBasePoly_BC + layerPolyFractional_BC


figure <- ggarrange(plotBasePoly_AC + layerPolyFractional_AC, 
                    plotBasePoly_BC + layerPolyFractional_BC,
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1)
figure

###############
###############
###############
# Group by AGE#
###############
###############
###############


# REDEFINE PREDICTION FUNCTION FOR EACH CASE
PredFun_adult <- function(x, model, data) {
  
  Age_M = data$Age_M
  
  ## Category indicator variable    
  indicator <- cut(x, 
                   breaks = c(-Inf,age_interval_Age[[1]]$Last_value_for_break))
  ## Variables for spline
  s1        <- pmax(Age_M -  age_interval_Age[[1]]$Last_value_for_break[1], 0)
  s2        <- pmax(Age_M -  age_interval_Age[[1]]$Last_value_for_break[2], 0)
  s3        <- pmax(Age_M -  age_interval_Age[[1]]$Last_value_for_break[3], 0)
  
  ## Predict
  predict(object = model, newdata = data.frame(x = x,
                                               indicator = indicator,
                                               s1 = s1, s2 = s2, s3 = s3))
}


PredFun_child <- function(x, model, data) {
  
  Age_M = data$Age_M
  
  ## Category indicator variable    
  indicator <- cut(x, 
                   breaks = c(-Inf,age_interval_Age[[2]]$Last_value_for_break))
  ## Variables for spline
  s1        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[1], 0)
  s2        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[2], 0)
  s3        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[3], 0)
  s4        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[4], 0)
  s5        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[5], 0)
  s6        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[6], 0)
  s7        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[7], 0)
  s8        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[8], 0)
  s9        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[9], 0)
  s10        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[10], 0)
  s11        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[11], 0)
  s12        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[12], 0)
  s13        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[13], 0)
  s14        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[14], 0)
  s15        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[15], 0)
  s16        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[16], 0)
  
  ## Predict
  predict(object = model, newdata = data.frame(x = x,
                                               indicator = indicator,
                                               s1 = s1, s2 = s2, s3 = s3, s4 = s4, s5 = s5,
                                               s6 = s6, s7 = s7, s8 = s8, s9 = s9, s10 = s10,
                                               s11 = s11, s12 = s12, s13 = s13, s14 = s14, s15 = s15,
                                               s16 = s16))
}



data_amp_AC_Age <- data_plot_AR_AC %>% group_split(Age)
data_amp_BC_Age <- data_plot_AR_BC %>% group_split(Age)

age_interval_Age =  age_interval
age_interval_Age$Age = c(rep("child", 16), rep("adult", 3))
age_interval_Age = age_interval_Age  %>% group_split(Age)

################################################
################################################
# do the above for AC and BC divided by gender #
################################################
################################################

newdata_data_plot_AR_AC_Age_adult <- within(data_amp_AC_Age[[1]], {
  ## Change to lower cases
  x <- Age_M
  y <- Amplitude_Ratio_AC #box_AC
  ## Category indicator variable
  indicator <- cut(Age_M,
                   breaks = c(-Inf,age_interval_Age[[1]]$Last_value_for_break))
  ## Variables for spline
  s1        <- pmax(Age_M -  age_interval_Age[[1]]$Last_value_for_break[1], 0)
  s2        <- pmax(Age_M -  age_interval_Age[[1]]$Last_value_for_break[2], 0)
  s3        <- pmax(Age_M -  age_interval_Age[[1]]$Last_value_for_break[3], 0)
})


newdata_data_plot_AR_AC_Age_child <- within(data_amp_AC_Age[[2]], {
  ## Change to lower cases
  x <- Age_M
  y <- Amplitude_Ratio_AC #box_AC
  ## Category indicator variable
  indicator <- cut(Age_M,
                   breaks = c(-Inf,age_interval_Age[[2]]$Last_value_for_break))
  ## Variables for spline
  s1        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[1], 0)
  s2        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[2], 0)
  s3        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[3], 0)
  s4        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[4], 0)
  s5        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[5], 0)
  s6        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[6], 0)
  s7        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[7], 0)
  s8        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[8], 0)
  s9        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[9], 0)
  s10        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[10], 0)
  s11        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[11], 0)
  s12        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[12], 0)
  s13        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[13], 0)
  s14        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[14], 0)
  s15        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[15], 0)
  s16        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[16], 0)
})




newdata_data_plot_AR_BC_Age_adult <- within(data_amp_BC_Age[[1]], {
  ## Change to lower cases
  x <- Age_M
  y <- Amplitude_Ratio_BC #box_AC
  ## Category indicator variable
  indicator <- cut(Age_M,
                   breaks = c(-Inf,age_interval_Age[[1]]$Last_value_for_break))
  ## Variables for spline
  s1        <- pmax(Age_M -  age_interval_Age[[1]]$Last_value_for_break[1], 0)
  s2        <- pmax(Age_M -  age_interval_Age[[1]]$Last_value_for_break[2], 0)
  s3        <- pmax(Age_M -  age_interval_Age[[1]]$Last_value_for_break[3], 0)
})


newdata_data_plot_AR_BC_Age_child <- within(data_amp_BC_Age[[2]], {
  ## Change to lower cases
  x <- Age_M
  y <- Amplitude_Ratio_BC #box_AC
  ## Category indicator variable
  indicator <- cut(Age_M,
                   breaks = c(-Inf,age_interval_Age[[2]]$Last_value_for_break))
  ## Variables for spline
  s1        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[1], 0)
  s2        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[2], 0)
  s3        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[3], 0)
  s4        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[4], 0)
  s5        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[5], 0)
  s6        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[6], 0)
  s7        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[7], 0)
  s8        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[8], 0)
  s9        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[9], 0)
  s10        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[10], 0)
  s11        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[11], 0)
  s12        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[12], 0)
  s13        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[13], 0)
  s14        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[14], 0)
  s15        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[15], 0)
  s16        <- pmax(Age_M -  age_interval_Age[[2]]$Last_value_for_break[16], 0)
})






####################
#Polynomial models # this time just the fractional
####################

## Fractional model
lmFractional_AC_adult <- lm(formula = y ~ x + I(x^2) + I(x^(1/2)),
                            data = newdata_data_plot_AR_AC_Age_adult)
lmFractional_AC_child <- lm(formula = y ~ x + I(x^2) + I(x^(1/2)), 
                            data = newdata_data_plot_AR_AC_Age_child)
  
lmFractional_BC_adult <- lm(formula = y ~ x + I(x^2) + I(x^(1/2)), 
                            data = newdata_data_plot_AR_BC_Age_adult)
lmFractional_BC_child <- lm(formula = y ~ x + I(x^2) + I(x^(1/2)), 
                            data = newdata_data_plot_AR_BC_Age_child)
  

#################
## Create layers# his time just the fractional
#################

layerPolyFractional_AC_adult <- stat_fun(fun = PredFun_adult,
                                args = list(model = lmFractional_AC_adult, 
                                            data = newdata_data_plot_AR_AC_Age_adult),
                                mapping = aes(color = "Fractional"))

layerPolyFractional_AC_child <- stat_fun(fun = PredFun_child,
                                         args = list(model = lmFractional_AC_child, 
                                                     data = newdata_data_plot_AR_AC_Age_child),
                                         mapping = aes(color = "Fractional"))



layerPolyFractional_BC_adult <- stat_fun(fun = PredFun_adult,
                                         args = list(model = lmFractional_BC_adult, 
                                                     data = newdata_data_plot_AR_BC_Age_adult),
                                         mapping = aes(color = "Fractional"))

layerPolyFractional_BC_child <- stat_fun(fun = PredFun_child,
                                         args = list(model = lmFractional_BC_child, 
                                                     data = newdata_data_plot_AR_BC_Age_child),
                                         mapping = aes(color = "Fractional"))




## Create plot base
plotBasePoly_AC_adult <- ggplot(data = newdata_data_plot_AR_AC_Age_adult,
                       mapping = aes(x = Age_M, y = Amplitude_Ratio_AC )) +
  layer(geom = "point",stat = "identity", position = "identity") +
  scale_x_discrete(breaks = seq(from = 0, 
                                to = newdata_data_plot_AR_AC$Age_M[length(newdata_data_plot_AR_AC$Age_M)], 
                                by = 12)) +     
  scale_color_discrete(name = "model",
                       breaks =  c("Linear","Quadratic","Cubic","Fractional")) +
  theme_bw()  +
  theme(legend.key = element_blank())


## Plot together
plotBasePoly + layerPolyFractional

































