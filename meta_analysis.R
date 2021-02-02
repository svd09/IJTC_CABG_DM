library(meta)

library(tidyverse)

setwd("G:\\CABG_metaanalysis\\graphs\\results\\heart_lung_circ\\")





d = read_csv("G:\\CABG_metaanalysis\\graphs\\results\\heart_lung_circ\\art.csv")


tar = meta::metagen(TE = logodds,seTE = sei,
  sm = "OR",comb.random = T, comb.fixed = F, data = d)

forest(tar, studlab = d$Study, 
       col.diamond = "blue",
       col.square = "darkgray")


bita = read_csv("bita.csv")

bitar = meta::metagen(TE = logodds,
  seTE = sei,
  studlab = Study,sm = "OR",
  comb.random = T, comb.fixed = F, data = bita)

forest(bitar,studlab = bita$Study,
       col.diamond = "blue",
       col.square = "darkgray")
# sensitivity analysis removing Dorman 2012

bitas = read_csv("bitas.csv")

bitars = meta::metagen(TE = logodds,
                      seTE = sei,
                      studlab = Study,sm = "OR",
                      comb.random = T, comb.fixed = F, data = bitas)

forest(bitars,studlab = bitas$Study,
       col.diamond = "blue",
       col.square = "darkgray")

