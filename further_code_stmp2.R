library(survival)
library(rstpm2)

library(tidyverse)
# example code for paper 
# using rstpm2 to model flexible parametric survival
# using functions from rstpm2 to create hazard models. 

library(rstpm2)

d = read_csv("D:\\CABG_metaanalysis\\graphs\\yamaguchi_2017\\yamaguchi_data.csv")

dk = stpm2(Surv(time, event)~ arm, data = d)

eform(dk)[2,]

library(ggplot2) 
preddk <- predict(dk, newdata=data.frame(arm=1:2), 
type="hazard", grid=TRUE, full=TRUE, se.fit=TRUE)


preddk <- transform(preddk,arm = factor(arm , labels=c("BITA","no-BITA")))

ggplot(preddk,aes(x=time,y=Estimate,ymin=lower,ymax=upper,fill=arm)) +
facet_grid(~arm) + xlab("Time since diagnosis (years)") + 
  ylab("Hazard") + geom_ribbon() + geom_line()



ggplot(preddk, aes(x= time ,y=Estimate, ymin=lower, ymax=upper,fill=arm)) +
xlab("Time since diagnosis (years)") + 
ylab("Hazard") + geom_ribbon(alpha=0.6) + geom_line()
