library(gamlss)
library(colorspace)
library(quantreg)
library(splines)
library(writexl)


###########################
#ROYSTON AND WRIGHT MODEL #
###########################

######
# AC #
######

#all together

gamlss_qsheet <- quantSheets(Amplitude_Ratio_AC, 
                             Age_Y, 
                             data = data_plot_AR_AC,
                             cent = c(25,50,95))



data_plot_AR_AC
mymodel<-gamlss(Amplitude_Ratio_AC ~ bfp(Age_Y, powers = c(2, 1/2)), 
          sigma.formula=~1,
          family=BCT, data=data_plot_AR_AC) 


# centiles(mymodel,
#          xvar=data_plot_AR_AC$Age_Y,
#          col.cent=1, cent=c(25,50,95))


plot(mymodel)
fitted(mymodel)
plot(fitted(mymodel))
summary(mymodel)

data_for_breaks<- data_plot_AR_AC  %>% 
                  group_by(New_Age_group) %>% 
                  filter(row_number()==1)

Q.stats(mymodel,
        xvar= data_plot_AR_AC$Age_Y,
        xcut.points =   (data_for_breaks$Age_Y + 0.001)[-1] )


Q.stats(mymodel,
        xvar=data_plot_AR_AC$Age_Y,
        xcut.points =   (data_for_breaks$Age_Y + 0.001)[-1],
        zvals=FALSE)


data_plot_AR_AC_gender = data_plot_AR_AC %>% group_split(gender)

########
#female#
########

#data_plot_AR_AC_gender[[1]]


gamlss_qsheet_f <- quantSheets(Amplitude_Ratio_AC, 
                               Age_Y, 
                               data = data_plot_AR_AC_gender[[1]],
                               cent = c(25,50,95))



mymodelf<-gamlss(Amplitude_Ratio_AC ~ bfp(Age_Y, powers = c(1/2)), 
                sigma.formula= ~ bfp(Age_Y, powers = 3 ),
                family=BCT, data=data_plot_AR_AC_gender[[1]]) 

# centiles(mymodelf,
#          xvar=data_plot_AR_AC_gender[[1]]$Age_Y,
#          col.cent=1, cent=c(25,50,95))



plot(mymodelf)
plot(fitted(mymodelf))

data_for_breaks_f<- data_plot_AR_AC_gender[[1]]  %>% 
  group_by(New_Age_group) %>% 
  filter(row_number()==1)


Q.stats(mymodelf,
        xvar=data_plot_AR_AC_gender[[1]]$Age_Y,
        xcut.points =   (data_for_breaks_f$Age_Y + 0.001)[-1] )

# Q.stats(mymodelf,
#         xvar=data_plot_AR_AC_gender[[1]]$Age_Y,
#         xcut.points =   (data_for_breaks_f$Age_Y + 0.001)[-1] ,
#         zvals=FALSE)



######
#male#
######



# Fit the quantile sheet
gamlss_qsheet_m <- quantSheets(Amplitude_Ratio_AC, 
                             Age_Y, 
                             data = data_plot_AR_AC_gender[[2]],
                             cent = c(25,50,95))



data_plot_AR_AC_gender[[2]]
mymodelm<-gamlss(Amplitude_Ratio_AC ~ bfp(Age_Y, powers = c(1/2)), 
                 sigma.formula = ~ bfp(Age_Y, powers = c(3)),
                 family = BCT, 
                 data = data_plot_AR_AC_gender[[2]] ) 

plot(mymodelm)
plot(fitted(mymodelm))


data_for_breaks_m<- data_plot_AR_AC_gender[[2]]  %>% 
                     group_by(New_Age_group) %>% 
                      filter(row_number()==1)


Q.stats(mymodelm,
        xvar = data_plot_AR_AC_gender[[2]]$Age_Y,
        xcut.points =   (data_for_breaks_m$Age_Y + 0.001)[-c(1,19)] )

# Q.stats(mymodelm,xvar=data_plot_AR_AC_gender[[2]]$Age_Y,
#         xcut.points =   (data_for_breaks_m$Age_Y + 0.001)[-c(1,19)] ,
#         zvals=FALSE)



######
# BC #
######


#all together

gamlss_qsheet_BC <- quantSheets(Amplitude_Ratio_BC, 
                             Age_Y, 
                             data = data_plot_AR_BC,
                             cent = c(25,50,95))

data_plot_AR_BC
mymodel_BC<-gamlss(Amplitude_Ratio_BC ~ bfp(Age_Y, powers = c(2, 1/2)), 
                sigma.formula=~1,
                family=BCT, data=data_plot_AR_BC) 


plot(mymodel_BC)
fitted(mymodel_BC)
plot(fitted(mymodel_BC))

data_for_breaks<- data_plot_AR_BC  %>% 
  group_by(New_Age_group) %>% 
  filter(row_number()==1)

Q.stats(mymodel,
        xvar= data_plot_AR_BC$Age_Y,
        xcut.points =   (data_for_breaks$Age_Y + 0.001)[-1] )


Q.stats(mymodel,
        xvar=data_plot_AR_BC$Age_Y,
        xcut.points =   (data_for_breaks$Age_Y + 0.001)[-1],
        zvals=FALSE)


data_plot_AR_BC_gender = data_plot_AR_BC %>% group_split(gender)

########
#female#
########

#data_plot_AR_AC_gender[[1]]


gamlss_qsheet_f_BC <- quantSheets(Amplitude_Ratio_BC, 
                               Age_Y, 
                               data = data_plot_AR_BC_gender[[1]],
                               cent = c(25,50,95))


mymodelf_BC<-gamlss(Amplitude_Ratio_BC ~ bfp(Age_Y, powers = c(1/2)), 
                 sigma.formula= ~ bfp(Age_Y, powers = 3 ),
                 family=BCT, data=data_plot_AR_BC_gender[[1]]) 


plot(mymodelf_BC)
plot(fitted(mymodelf_BC))

data_for_breaks_f<- data_plot_AR_BC_gender[[1]]  %>% 
  group_by(Age_group) %>% 
  filter(row_number()==1)


Q.stats(mymodelf_BC,
        xvar=data_plot_AR_BC_gender[[1]]$Age_Y,
        xcut.points =   (data_for_breaks_f$Age_Y + 0.001)[-1] )





######
#male#
######



# Fit the quantile sheet
gamlss_qsheet_m_BC <- quantSheets(Amplitude_Ratio_BC, 
                               Age_Y, 
                               data = data_plot_AR_BC_gender[[2]],
                               cent = c(25,50,95))



data_plot_AR_BC_gender[[2]]
mymodelm_BC<-gamlss(Amplitude_Ratio_BC ~ bfp(Age_Y, powers = c(1/2)), 
                 sigma.formula = ~ bfp(Age_Y, powers = c(3)),
                 family = BCT, 
                 data = data_plot_AR_BC_gender[[2]] ) 

plot(mymodelm_BC)
plot(fitted(mymodelm_BC))


data_for_breaks_m<- data_plot_AR_BC_gender[[2]]  %>% 
  group_by(Age_group) %>% 
  filter(row_number()==1)


Q.stats(mymodelm_BC,
        xvar = data_plot_AR_BC_gender[[2]]$Age_Y,
        xcut.points =   (data_for_breaks_m$Age_Y + 0.001)[-c(1,19)] )

# Q.stats(mymodelm,xvar=data_plot_AR_AC_gender[[2]]$Age_Y,
#         xcut.points =   (data_for_breaks_m$Age_Y + 0.001)[-c(1,19)] ,
#         zvals=FALSE)



###################
# GOODNESS OF FIT #
###################


AIC(mymodel, mymodelf, mymodelm)
AIC(mymodel_BC, mymodelf_BC, mymodelm_BC)

# BIC(mymodel, mymodelf, mymodelm)
# BIC(mymodel_BC, mymodelf_BC, mymodelm_BC)


perf_AC<- rbind(model_performance(mymodel, metrics = "all", verbose = TRUE), 
                model_performance(mymodelf, metrics = "all", verbose = TRUE),
                model_performance(mymodelm, metrics = "all", verbose = TRUE))


perf_BC<- rbind(model_performance(mymodel_BC, metrics = "all", verbose = TRUE),
                model_performance(mymodelf_BC, metrics = "all", verbose = TRUE),
                model_performance(mymodelm_BC, metrics = "all", verbose = TRUE))



rownames(perf_AC)<- c("All","Females","Males")
rownames(perf_BC)<- c("All","Females","Males")




write_xlsx(as.data.frame(perf_AC),
     "C:\\Users\\mcampi\\Desktop\\Hung&Sylvette\\Sylvette_final_reviews4\\perf_AC.xlsx")


write_xlsx(as.data.frame(perf_BC),
     "C:\\Users\\mcampi\\Desktop\\Hung&Sylvette\\Sylvette_final_reviews4\\perf_BC.xlsx")



