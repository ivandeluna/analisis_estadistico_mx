#------------------------------------------------------------------------------
#
#    Descomposición del Ingreso (Blinder-Oaxaca)
#
#------------------------------------------------------------------------------

library(oaxaca) # Librería para hacer análisis de descomposición
library(dummies) # Librería para hacer variables dummy

#
# Descomposición para el 2o Trimestre del 2017
#
t419.trc.ocup <- subset(t419.trc, clase1 == 1)

t419.ob <- t419.trc[c("sex", "eda", "n_hij", "e_con",
                      "niv_ins", "anios_esc","ing_x_hrs",
                      "hrsocup")]
t419.ob$eda <- as.integer(t419.ob$eda)
t419.ob$n_hij <- as.integer(t419.ob$n_hij)
t419.ob$n_hij[is.na(t419.ob$n_hij)] <- 0
t419.ob$sex <- factor(t419.ob$sex,
                        levels = c(1,2),
                        labels = c("Hombre", "Mujer"))
t419.ob$e_con <- factor(t419.ob$e_con,
                          levels = c(1,2,3,4,5,6),
                          labels = c("UnionLibre","Separada","Divorciada",
                                     "Viuda","Casada","Soltera"))
t419.ob$niv_ins <- factor(t419.ob$niv_ins,
                          levels = c(1,2,3,4,5),
                          labels = c("PrimariaIncompleta",
                                     "Primaria",
                                     "Secundaria",
                                     "MedioSuperior",
                                     "No especificado"))

t419.data <- dummy.data.frame(as.data.frame(t419.ob),
                              names = c("sex","niv_ins","e_con"))

# Decomposición según edad, años de escolaridad, horas de ocupación, estado conyugal casada,
# y variables dummies para nivel de instrucción de primaria incompleta hasta media superior y superior

t419.res <- oaxaca(formula = ing_x_hrs ~ eda + n_hij + anios_esc + hrsocup + e_conCasada +
                niv_insPrimariaIncompleta + niv_insPrimaria + niv_insSecundaria + niv_insMedioSuperior | sexMujer |
                niv_insPrimariaIncompleta + niv_insPrimaria + niv_insSecundaria + niv_insMedioSuperior,
              data = t419.data, R = 1000)

# Descomposición según edad, años de escolaridad, horas de ocupación, estado conyugal casada.
# 

t419.res <- oaxaca(formula = ing_x_hrs ~ eda + anios_esc + hrsocup + e_conCasada | sexMujer ,
                   data = t419.data, R = 1000)

t419.res$y

summary(t419.res$reg$reg.A)
summary(t419.res$reg$reg.B)

# La variable eda no es muy significante en este modelo, podemos checar sin ella como resulta
# la estimación

t419.res <- oaxaca(formula = ing_x_hrs ~ anios_esc + hrsocup + e_conCasada | sexMujer ,
                   data = t419.data, R = 1000)

t419.res$y

summary(t419.res$reg$reg.A)
summary(t419.res$reg$reg.B)

#
# Descomposición para el 2o Trimestre del 2018
#

t218.ob <- t218.trc[c("sex", "eda", "e_con",
                      "niv_ins", "anios_esc","ing_x_hrs",
                      "hrsocup")]
t218.ob$eda <- as.integer(t218.ob$eda)
t218.ob$sex <- factor(t218.ob$sex,
                      levels = c(1,2),
                      labels = c("Hombre", "Mujer"))
t218.ob$e_con <- factor(t218.ob$e_con,
                        levels = c(1,2,3,4,5,6),
                        labels = c("UnionLibre","Separada","Divorciada",
                                   "Viuda","Casada","Soltera"))
t218.ob$niv_ins <- factor(t218.ob$niv_ins,
                          levels = c(1,2,3,4,5),
                          labels = c("PrimariaIncompleta",
                                     "Primaria",
                                     "Secundaria",
                                     "MedioSuperior",
                                     "No especificado"))

t218.data <- dummy.data.frame(as.data.frame(t218.ob),
                              names = c("sex","niv_ins","e_con"))

# Decomposición según edad, años de escolaridad, horas de ocupación, estado conyugal casada,
# y variables dummies para nivel de instrucción de primaria incompleta hasta media superior y superior

t218.res <- oaxaca(formula = ing_x_hrs ~ eda + anios_esc + hrsocup + e_conCasada +
                     niv_insPrimariaIncompleta + niv_insPrimaria + niv_insSecundaria + niv_insMedioSuperior | sexMujer |
                     niv_insPrimariaIncompleta + niv_insPrimaria + niv_insSecundaria + niv_insMedioSuperior,
                   data = t218.data, R = 1000)
t218.res$y

# Descomposición según edad, años de escolaridad, horas de ocupación, estado conyugal casada.
# 

t218.res <- oaxaca(formula = ing_x_hrs ~ anios_esc + hrsocup + e_conCasada | sexMujer | e_conCasada,
                   data = t218.data, R = 1000)

t218.res$y

summary(t218.res$reg$reg.A)
summary(t218.res$reg$reg.B)

# Siguiendo el mismo modelo para el 2t2017, se estima sin la edad
t218.res <- oaxaca(formula = ing_x_hrs ~ anios_esc + hrsocup + e_conCasada | sexMujer | e_conCasada,
                   data = t218.data, R = 1000)

t218.res$y

summary(t218.res$reg$reg.A)
summary(t218.res$reg$reg.B)

#
#  Análisis y comparación de resultados 
#


t419.res$y
t218.res$y

t218.res$x$x.mean.diff
t419.res$x$x.mean.diff

summary(t419.res$reg$reg.A)
summary(t419.res$reg$reg.B)


summary(t218.res$reg$reg.A)
summary(t218.res$reg$reg.B)

plot(t218.res)
plot(t419.res)

summary(t218.res)
summary(t419.res)

#
#
#  Decomposició para estimar la diferencia en horas de ocupación
#
#

#t218.res2 <- oaxaca(formula = hrsocup ~ eda + anios_esc + ing_x_hrs + e_conCasada +
#                     niv_insPrimariaIncompleta + niv_insPrimaria + niv_insSecundaria + niv_insMedioSuperior | sexMujer |
#                     niv_insPrimariaIncompleta + niv_insPrimaria + niv_insSecundaria + niv_insMedioSuperior,
#                   data = t218.data, R = 1000)

t218.res2 <- oaxaca(formula = hrsocup ~ eda + anios_esc + e_conCasada +
                      niv_insPrimariaIncompleta + niv_insPrimaria + niv_insSecundaria + niv_insMedioSuperior | sexMujer |
                      niv_insPrimariaIncompleta + niv_insPrimaria + niv_insSecundaria + niv_insMedioSuperior,
                    data = t218.data, R = 1000)

t218.res2 <- oaxaca(formula = hrsocup ~ eda + anios_esc + e_conCasada | sexMujer,
                    data = t218.data, R = 1000)

t218.res2$y

summary(t218.res2$reg$reg.A)
summary(t218.res2$reg$reg.B)

summary(t218.res2)
