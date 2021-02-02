# code for combining the data together
# model rstpm2 code for flexible parametric model
# compare conventional KM model and flexible parametic model

library(tidyverse);library(tidylog);
library(survival);library(GGally);
library(survminer);library(MASS);
library(rstpm2);library(magrittr);
library(broom);library(GGally);library(haven)


path = "D:\\CABG_metaanalysis\\graphs\\"

buxton = read_csv(
  "D:\\CABG_metaanalysis\\graphs\\buxton_2012\\buxton_data.csv")

hoffman = read_csv(
  "D:\\CABG_metaanalysis\\graphs\\hoffman_2013\\hoffman_data.csv")

lin = read_csv(
  "D:\\CABG_metaanalysis\\graphs\\liin_2013\\lin_data.csv")

raza = read_csv(
  "D:\\CABG_metaanalysis\\graphs\\raza_2017\\raza_data.csv")

tatoulis = read_csv(
  "D:\\CABG_metaanalysis\\graphs\\tatoulis_2017\\tatoulis_data.csv")

yamaguchi = read_csv(
  "D:\\CABG_metaanalysis\\graphs\\yamaguchi_2017\\yamaguchi_data.csv")

dorman = read_csv(
  "D:\\CABG_metaanalysis\\graphs\\dorman_2012\\dorman_data.csv")


pevni = read_csv(
"D:\\CABG_metaanalysis\\graphs\\pevni_2017\\pevni_data.csv")

iribarne = read_csv(
  "D:\\CABG_metaanalysis\\graphs\\iribarne_2018\\iribarne_data.csv")

schwann = read_csv(
  "D:\\CABG_metaanalysis\\graphs\\schwann_2013\\schwann_data.csv")

# create a seperate survivor object for each data

buxton_surv = buxton %$% Surv(time, event)

hoffman_surv = hoffman %$% Surv(time, event)

lin_surv = lin %$% Surv(time, event)

raza_surv = raza %$% Surv(time, event)

tatoulis_surv = tatoulis %$% Surv(time, event)

yamaguchi_surv = yamaguchi %$% Surv(time, event)

pevni_surv = pevni %$% Surv(time, event)

dorman_surv = dorman %$% Surv(time, event)

iribarne_surv = iribarne %$% Surv(time, event)

schwann_surv = schwann %$% Surv(time, event)

# combine all data and then create a survivorship object

df = rbind(buxton, hoffman, lin, raza, tatoulis, 
           yamaguchi, dorman, pevni, iribarne, schwann)

str(df)

# write_csv(df, "D:\\CABG_metaanalysis\\graphs\\combineddata.csv")

df_surv = df %$% Surv(time, event)

df %>% count(study)

# create a non-parametric kaplan meier model 
# for the whole cohort

km_df = survfit(df_surv ~ 1, data = df)

km_df

w = ggsurv(km_df,plot.cens = F, CI = T)

km_arm = survfit(df_surv ~ arm, data = df)

ggsurv(km_arm, plot.cens = F)


# Compare BITA and LITA cohorts 

df2 = rbind(buxton, raza, tatoulis, 
      yamaguchi, dorman, pevni, iribarne)

# write_dta(df2, "D:\\CABG_metaanalysis\\graphs\\bita_lita_cohort.dta")

# create survivor object

df2_surv = survfit(Surv(time, event) ~ arm, data = df2)

df2_surv

survdiff(Surv(time, event) ~ arm, data = df2)

# create a parametric survival object and then compare
# curve for parametic survival object and km curve

flex_surv = stpm2(Surv(time, event)~ arm, 
                  data = df2,df = 4, frailty = T,cluster = study)

summary(flex_surv)

eform(flex_surv)[2,]


library(ggplot2)

predarm <- predict(flex_surv, newdata=data.frame(arm = 1:2),
  type="hazard", full = T, se.fit=TRUE, grid = T)

predarm = 
  transform(predarm, arm = factor(arm, labels = c("BITA", "LITA")))

ggplot(predarm,
       aes(x= time, y=Estimate, ymin=lower, ymax=upper, 
             color = arm)) +
  xlab("Time (Years)") +
  ylab("Hazard")+
  geom_line() +
  geom_ribbon(alpha=0.4) +
  geom_line()

# LITA/vein vs LITA/RA:


df_ra = rbind(hoffman, lin, 
           schwann)


km_ra = survfit(Surv(time, event) ~ arm, data = df_ra)

ggsurv(km_ra, plot.cens = F)

survdiff(Surv(time, event) ~ arm, data = df_ra)


flex_ra = stpm2(Surv(time, event)~ arm, 
                  data = df_ra,df = 4, frailty = T,cluster = study)

summary(flex_ra)

eform(flex_ra)[2,]

predra <- predict(flex_ra, newdata=data.frame(arm = 1:2),
        full = T, type="hazard", se.fit=TRUE, grid = T)

predra = 
  transform(predra, arm = factor(arm, labels = c("RA", "SVG")))

ggplot(predra,
       aes(x= time, y=Estimate, ymin=lower, ymax=upper, 
           color = arm)) +
  xlab("Time (Years)") +
  ylab("Hazard")+
  geom_line() +
  geom_ribbon(alpha=0.4) +
  geom_line()

