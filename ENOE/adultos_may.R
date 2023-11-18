# Adultos mayores
library(descr)
t218.trc.am <- subset(t218.trc, eda >= 60)

tot <- 1122898

summary(as.numeric(t218.trc.am$eda))

wtd.table(t218.trc.am$eda7c, weights = t218.trc.am$fac)

descr::freq(t218.trc.am$eda)

table(t218.trc.am$eda, weight = t218.trc.am$fac)

crosstab(t218.trc.am$eda7c, t218.trc.am$clase1, weight = t218.trc.am$fac, prop.t = TRUE)
crosstab(t218.trc.am$clase1, t218.trc.am$sex, weight = t218.trc.am$fac, prop.c = TRUE)

crosstab(t218.trc.am$clase2, t218.trc.am$sex, weight = t218.trc.am$fac, prop.r = TRUE)

crosstab(t218.trc.am$clase3, t218.trc.am$sex, weight = t218.trc.am$fac, prop.r = TRUE)

  crosstab(t218.trc.am$niv_ins, t218.trc.am$sex, weight = t218.trc.am$fac, prop.c = TRUE)

am.ocup <- subset(t218.trc.am, clase2 == 1) # Población ocupada

crosstab(am.ocup$eda7c, am.ocup$pos_ocu, weight = am.ocup$fac, prop.t = TRUE)

crosstab(am.ocup$pos_ocu, am.ocup$sex, weight = am.ocup$fac, prop.c = TRUE)

crosstab(am.ocup$seg_soc, am.ocup$sex, weight = am.ocup$fac, prop.r = TRUE)

crosstab(am.ocup$ing7c, am.ocup$sex, weight = am.ocup$fac, prop.c = TRUE)

crosstab(am.ocup$medica5c, am.ocup$sex, weight = am.ocup$fac, prop.c = TRUE)

am.des <- subset(t218.trc.am, clase2 == 2) # Población desocupada

crosstab(am.des$d_ant_lab, am.des$sex , weight = am.des$fac)

crosstab(am.des$d_cexp_est, am.des$sex , weight = am.des$fac, prop.c = TRUE)

am.pnea <- subset(t218.trc.am, clase1 == 2) # PNEA

crosstab(am.pnea$pnea_est, am.pnea$sex , weight = am.pnea$fac, prop.r = TRUE)

library(stargazer)

sink("ie.txt", append = TRUE)

descr::freq(t218.trc$eda12c, weight = t218.trc$fac)
descr::freq(t218.trc$ing7c, weight = t218.trc$fac)

wtd.table(t218.trc$eda12c, t218.trc$sex, weights = t218.trc$fac)
wtd.table(t218.trc$ing7c, t218.trc$sex, weights = t218.trc$fac)
sink()



stargazer(ie)
