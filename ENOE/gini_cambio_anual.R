
############################################
#
#          Gini
#
############################################

library(reldist)
library(ineq)

t217.ing_x_hrs <- t217.sdem.trc$ing_x_hrs
t217.hrsocup <- t217.sdem.trc$hrsocup

t218.ing_x_hrs <- t218.sdem.trc$ING_X_HRS
t218.hrsocup <- t218.sdem.trc$HRSOCUP 


t318.ing_x_hrs <- t318.sdem.trc$ING_X_HRS
t318.hrsocup <- t318.sdem.trc$HRSOCUP 

t218.m.ixh <- t218.sdem.trc$ING_X_HRS[t218.sdem.trc$SEX == 2]
t218.m.hrs <- t218.sdem.trc$HRSOCUP[t218.sdem.trc$SEX == 2]


t218.h.ixh <- t218.sdem.trc$ING_X_HRS[t218.sdem.trc$SEX == 1]
t218.h.hrs <- t218.sdem.trc$HRSOCUP[t218.sdem.trc$SEX == 1]

head(t217.ing_x_hrs)

t217.semanal <- t217.ing_x_hrs * t217.hrsocup
t218.semanal <- t218.ing_x_hrs * t218.hrsocup
t318.semanal <- t318.ing_x_hrs * t318.hrsocup

t218.m.sem <- t218.m.ixh * t218.m.hrs
t218.h.sem <- t218.h.ixh * t218.h.hrs

head(t217.semanal)

t217.mensual <- t217.semanal * 4.3333
t218.mensual <- t218.semanal * 4.3333
t318.mensual <- t318.semanal * 4.3333

t218.m.mes <- t218.m.sem * 4.3333
t218.h.mes <- t218.h.sem * 4.3333

t217.mensual <- t217.mensual[!(t217.mensual == 0)]
t218.mensual <- t218.mensual[!(t218.mensual == 0)]
t318.mensual <- t318.mensual[!(t318.mensual == 0)]

t218.m.mes <- t218.m.mes[!(t218.m.mes == 0)]
t218.h.mes <- t218.h.mes[!(t218.h.mes == 0)]

summary(t217.mensual)

head(t218.mensual)

t217.w <- t217.sdem.trc$fac[!(t217.sdem.trc$ing_x_hrs == 0)]
t218.w <- t218.sdem.trc$FAC[!(t218.sdem.trc$ING_X_HRS == 0)]
t318.w <- t318.sdem.trc$FAC[!(t318.sdem.trc$ING_X_HRS == 0)]

t218.m.w <- t218.sdem.trc$FAC[(t218.sdem.trc$SEX == 2) & !(t218.sdem.trc$ING_X_HRS == 0)]
t218.h.w <- t218.sdem.trc$FAC[(t218.sdem.trc$SEX == 1) & !(t218.sdem.trc$ING_X_HRS == 0)]



gini(t217.mensual, weights = t217.w)
gini(t218.mensual, weights = t218.w) # General 2t 2018
gini(t318.mensual, weights = t318.w) # General 3t 2018

gini(t318.sdem.trc$INGOCUP, weights = t318.sdem.trc$FAC)

gini(t218.m.mes, weights = t218.m.w) # Mujeres
gini(t218.h.mes, weights = t218.h.w) # Hombres

t217.lc <- Lc(t217.mensual)
t218.lc <- Lc(t218.mensual)
t318.lc <- Lc(t318.mensual)

plot(t217.lc)
plot(t218.lc)
plot(t318.lc)

lines(t217.lc, col = 2)
lines(t218.lc, col = 2)
lines(t318.lc, col = 2)

ineq(t217.mensual)
ineq(t218.mensual)
ineq(t318.mensual)

hist(t217.mensual)
hist(t218.mensual)
hist(t318.mensual)

ks.test(t217.mensual, t218.mensual)

ks.test(t218.h.mes, t218.m.mes)

plot(density(t218.m.mes), col = "red")
lines(density(t218.h.mes), col = "blue")

library(ggplot2)

ing.dens <- cbind.data.frame(t217.mensual,t218.mensual)

conc(t217.mensual)
conc(t218.mensual)
conc(t318.mensual)

plot(t217.mensual, col = "red", lwd = 2)

p <- t318.lc[1]
L <- t318.lc[2]
t318.lorenz <- data.frame(p,L)

t318.curva <- ggplot(data = t318.lorenz)+
  geom_point(aes(x = p,y = L))+
  geom_line(aes(x = p,y = L), color = "red", size = 1, alpha = 0.5) +
  scale_x_continuous(name = "Proporción acumulada de la población", limits = c(0,1))+
  scale_y_continuous(name = "Proporción acumulada del ingreso", limits = c(0,1))+
  geom_abline()+
  ggtitle("Curva de Lorenz para la distribución de los ingresos en la ZML.")+
  theme_bw()

t318.curva

p_lab <- 152337/202616
q_lab <- 1 - p
z <- 1.96^2
e <- 0.05

n_0_lab <- (z*p_lab*q_lab)/(e^2)

n_lab <- n_0_lab / (1 + (n_0_lab/202616))
n_lab

p_mun <- 977/2853
q_mun <- 1-p
z <- 1.96^2
e <- 0.05
  
n_0_mun <- (z*p_mun*q_mun)/(e^2)

n_mun <- n_0_mun / (1 + (n_0_mun/2853))
n_mun


p_est <- 2391/12891
q_est <- 1-p
z <- 1.96^2
e <- 0.05

n_0_est <- (z*p_est*q_est)/(e^2)

n_est <- n_0_est / (1 + (n_0_est/12891))
n_est

p_c <- 10/18
q_c <- 1-p
z <- 1.96^2
e <- 0.05

n_0_c <- (z*0.5*0.5)/(e^2)

n_c <- n_0_c / (1 + (n_0_c/18))
n_c



