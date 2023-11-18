# Análisis de la ENOE 
library(dplyr)   # para manipular datos
library(tidyr)   # para manipular datos
library(questionr) # para hacer tablas con factores de expansión
library(descr)   # para hacer tablas con factores de expansión, cruzadas, etc..
library(dummies) # para crear variables dummy
library(oaxaca)  # para hacer análisis de descomposición/diferencias
library(ggplot2) # para generar gráficas

# Como los microdatos de de las tablas de la ENOE 2T 2018 tienen los nombres de las columnas en
# mayúsculas, será necesario o conveniente pasarlas a minúsculas

names(t419.sdem.trc) <- tolower(names(t419.sdem.trc))

#--------------------------------------------------------------------------------------------
#
#                          Análisis de las características sociodemográficas
#
#--------------------------------------------------------------------------------------------

#
#      Sexo y Edad     #
#

summary(as.numeric(t419.trc$eda))



boxplot(log(as.numeric(t419.trc$p6b2)) ~ t419.trc$eda, 
        main = "Población ocupada por ingreso y por edad.",
        xlab = "Edad",
        ylab = "Logaritmo de los ingresos")

t419.ing.edad.sexo <- t419.trc[c("eda", "p6b2","sex")]
t419.ing.edad.sexo$p6b2 <- as.numeric(t419.ing.edad.sexo$p6b2)
t419.ing.edad.sexo$sex <- factor(t419.ing.edad.sexo$sex, levels = c(1,2),
                                 labels = c("Hombre", "Mujer"))

t1.bping <- ggplot(na.omit(t419.ing.edad.sexo), aes(x = eda, y = log(p6b2), fill = sex))+
  geom_boxplot(alpha=0.7)
t1.bping + scale_x_discrete(name = "Edad") +
  scale_y_continuous(name = "Logaritmo de los ingresos")+
  ggtitle("Población ocupada por ingreso y por edad") +
  theme_bw()+
  labs(fill = "Sexo")


t1.ing.eda12c.sexo <- t419.trc[c("eda12c", "p6b2","sex")]
t1.ing.eda12c.sexo$p6b2 <- as.numeric(t1.ing.eda12c.sexo$p6b2)
t1.ing.eda12c.sexo$sex <- factor(t1.ing.eda12c.sexo$sex, levels = c(1,2),
                                 labels = c("Hombre", "Mujer"))
t1.ing.eda12c.sexo$eda12c <- factor(t1.ing.eda12c.sexo$eda12c, levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
                                    labels = c("15 a 19", "20 a 24", "25 a 29",
                                               "30 a 34", "35 a 39", "40 a 44",
                                               "45 a 49", "50 a 54", "55 a 59",
                                               "60 a 64", "65 y más", "No especificado"))

bpeda12c <- ggplot(na.omit(t1.ing.eda12c.sexo), aes(x = eda12c, y = log(p6b2), fill = sex))+
  geom_boxplot(alpha=0.7)
bpeda12c + scale_x_discrete(name = "Años de edad") +
  scale_y_continuous(name = "Logaritmo de los ingresos")+
  ggtitle("Población ocupada por ingreso y por edad. \n ENOE 4° Trim. 2019") +
  theme_bw()+
  labs(fill = "Sexo", caption = "\nGráfica elaborada por el Instituto Municipal de Planeación y Competitividad de Torreón. Febrero 2020")


# Gráfica de densidad ingresos salariales hombres y mujeres 4t2019
t1.ing.sexo <- t419.trc[c("ingocup","sex", "eda", "p6b2")]
t1.ing.sexo$ingocup <- as.numeric(t1.ing.sexo$ingocup)
t1.ing.sexo$p6b2 <- as.numeric(t1.ing.sexo$p6b2)
t1.ing.sexo$sex <- factor(t1.ing.sexo$sex, levels = c(1,2),
                          labels = c("Hombre", "Mujer"))
bpingocup1t <- ggplot(na.omit(t1.ing.sexo), aes(x = log(ingocup), fill = sex)) +
  geom_density(alpha = 0.25) +
  xlab("Logaritmo de los ingresos")+
  ylab("Densidad")+
  labs(fill = "Sexo")
bpingocup1t

# Histograma ingresos salariales hombrs y mujeres 2t2018
histingocup1t <- ggplot(na.omit(t1.ing.sexo), aes(x = log(ingocup), fill = sex)) +
  geom_histogram(position = "identity",alpha = 0.25) +
  xlab("Logaritmo de los ingresos")+
  ylab("Frecuencia")+
  labs(fill = "Sexo")
histingocup1t


library(gridExtra)

grid.arrange(bpingocup1t, histingocup1t, nrow=1)


#------------------------------------------------------------------------------
#
# Tablas cruzadas
#
#------------------------------------------------------------------------------

library(descr)

t419.fac <- t419.trc$fac
t217.fac <- t217.trc$fac

ing7c.lev <- c(1,2,3,4,5,6,7)
ing7c.lab <- c("Hasta 1 SM", "de 1 a 2 SM", "de 2 a 3 SM",
               "de 3 a 5 SM", "más de 5 SM", "No recibe ingresos",
               "No especificado")

eda7c.lev <- c(1,2,3,4,5,6,7)
eda7c.lab <- c("de 15 a 19 años", "de 20 a 29 años", "de 30 a 39 años",
               "de 40 a 49 años", "de 50 a 50 años", "de 60 años y más",
               "No especificado")


t419.trc$ing7c <- factor(t419.trc$ing7c, levels = ing7c.lev,
                         labels = ing7c.lab)

t419.trc$eda7c <- factor(t419.trc$eda7c, levels = eda7c.lev,
                         labels = eda7c.lab)

t217.trc$ing7c <- factor(t217.trc$ing7c, levels = ing7c.lev,
                         labels = ing7c.lab)

t217.trc$eda7c <- factor(t217.trc$eda7c, levels = eda7c.lev,
                         labels = eda7c.lab)

t419.m <- subset(t419.trc, sex == 2)
t419.h <- subset(t419.trc, sex == 1)


crosstab(t419.m$ing7c, t419.m$eda7c, weight = t419.m$fac, prop.t = TRUE)
crosstab(t419.h$ing7c, t419.h$eda7c, weight = t419.h$fac, prop.t = TRUE)

crosstab(t217.trc$ing7c, t217.trc$eda7c, weight = t217.fac, prop.t = TRUE)

crosstab(t419.trc$ing7c, t419.trc$eda7c, weight = t419.fac, prop.r = TRUE)

crosstab(t419.trc$sex, t419.trc$eda7c, weight = t419.fac, prop.r = TRUE)

CrossTable(t217.trc$ing7c, t217.trc$eda7c)

freq(t218.trc$ing7c)
freq(t218.trc$eda7c)

crosstab(t218.trc$scian, t218.trc$sex, weight = t218.fac)

crosstab(t218.trc$ma48me1sm, t218.trc$sex, weight = t218.fac)

crosstab(t218.trc$c_ocu11c, t218.trc$sex, weight = t218.fac)

crosstab(t419.m$c_ocu11c, t419.m$p6b2, weight = t419.m$fac)
crosstab(t419.h$c_ocu11c, t419.h$sex, weight = t419.h$fac)

t419.m %>% filter(clase2 == 1) %>% group_by(c_ocu11c) %>% summarise(sum(fac))

  crosstab(t419.m$c_ocu11c, t419.m$clase2, weight = t419.m$fac, prop.t = TRUE)

t419.h %>% group_by(c_ocu11c) %>% summarise(weighted.mean(ing_x_hrs, fac))
t419.m %>% group_by(c_ocu11c) %>% summarise(weighted.mean(ing_x_hrs, fac))

t419.h %>% group_by(c_ocu11c) %>% summarise(weighted.mean(hrsocup, fac))
t419.m %>% group_by(c_ocu11c) %>% summarise(weighted.mean(hrsocup, fac))


t419.h %>% group_by(rama_est2) %>% summarise(weighted.mean(ing_x_hrs, fac))
t419.m %>% group_by(rama_est2) %>% summarise(weighted.mean(ing_x_hrs, fac))

t419.h %>% group_by(rama_est2) %>% summarise(weighted.mean(hrsocup, fac))
t419.m %>% group_by(rama_est2) %>% summarise(weighted.mean(hrsocup, fac))
#------------------------------------------------------------------------------
#
# PNEA
#
#------------------------------------------------------------------------------

unique(t218.trc$CLASE1)

t218.trc.pnea <- subset(t218.trc, t218.trc$CLASE1 == 2)

freq(t218.trc.pnea$C_INAC5C, w = t218.trc.pnea$FAC)

crosstab(t218.trc.pnea$C_INAC5C, t218.trc.pnea$SEX, weight = t218.trc.pnea$FAC,
         prop.c = TRUE)

crosstab(t218.trc.pnea$SEX, t218.trc.pnea$EDA7C, weight = t218.trc.pnea$FAC,
         prop.r = TRUE)

freq(t218.trc.pnea$PNEA_EST, w = t218.trc.pnea$FAC)

crosstab(t218.trc.pnea$PNEA_EST, t218.trc.pnea$SEX, weight = t218.trc.pnea$FAC,
         prop.c = TRUE)

crosstab(t218.trc.pnea$NIV_INS, t218.trc.pnea$SEX, weight = t218.trc.pnea$FAC,
         prop.c = TRUE)

crosstab(t218.trc.pnea$EDA7C, t218.trc.pnea$SEX, weight = t218.trc.pnea$FAC,
         prop.c = TRUE)


crosstab(t218.trc.pnea$HIJ5C, t218.trc.pnea$SEX, weight = t218.trc.pnea$FAC,
         prop.c = TRUE)

crosstab(t218.trc.pnea$P2F, t218.trc.pnea$SEX, weight = t218.trc.pnea$FAC,
         prop.c = TRUE)

crosstab(t218.trc.pnea$P2G1, t218.trc.pnea$SEX, weight = t218.trc.pnea$FAC,
         prop.c = TRUE)

crosstab(t218.trc.pnea$P2G2, t218.trc.pnea$SEX, weight = t218.trc.pnea$FAC,
         prop.c = TRUE)

wtd.table(t317.sdem.coah$sex, weights = t317.sdem.coah$fac)
1500506+1533593

###############################################################################
# Tercer trimestre 2018
###############################################################################
t3.ing.eda12c.sexo <- t318.trc[c("EDA12C", "P6B2","SEX")]
t3.ing.eda12c.sexo$P6B2 <- as.numeric(t3.ing.eda12c.sexo$P6B2)
t3.ing.eda12c.sexo$SEX <- factor(t3.ing.eda12c.sexo$SEX, levels = c(1,2),
                                 labels = c("Hombre", "Mujer"))
t3.ing.eda12c.sexo$EDA12C <- factor(t3.ing.eda12c.sexo$EDA12C, levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
                                    labels = c("15 a 19", "20 a 24", "25 a 29",
                                               "30 a 34", "35 a 39", "40 a 44",
                                               "45 a 49", "50 a 54", "55 a 59",
                                               "60 a 64", "65 y más", "No especificado"))

bpeda12c <- ggplot(na.omit(t3.ing.eda12c.sexo), aes(x = EDA12C, y = log(P6B2), fill = SEX))+
  geom_boxplot(alpha=0.7)
bpeda12c + scale_x_discrete(name = "Años de edad") +
  scale_y_continuous(name = "Logaritmo de los ingresos")+
  ggtitle("Población ocupada por ingreso y por edad. \n ENOE 3 Trimestre 2018") +
  theme_bw()+
  labs(fill = "Sexo")

t318.ingreso <- t318.trc[c("ING_X_HRS","HRSOCUP", "SEX", "EDA12C", "FAC")]

t318.ingreso <- t318.ingreso %>% mutate (ing = ING_X_HRS * HRSOCUP)

t318.ingreso <- t318.ingreso %>% mutate(ingmens = ing * 4.3333)

t318.ingreso$SEX <- factor(t318.ingreso$SEX, levels = c(1,2),
                           labels = c("Hombre", "Mujer"))
t318.ingreso$EDA12C <- factor(t318.ingreso$EDA12C, levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
                              labels = c("15 a 19", "20 a 24", "25 a 29",
                                         "30 a 34", "35 a 39", "40 a 44",
                                         "45 a 49", "50 a 54", "55 a 59",
                                         "60 a 64", "65 y más", "No especificado"))

ingmens <- ggplot(na.omit(t318.ingreso), aes(x = EDA12C, y = log(ingmens), fill = SEX,
                                             weight = FAC))+
  geom_boxplot(alpha = 0.7)+
  scale_x_discrete(name = "Años de edad")+
  scale_y_continuous(name = "Logaritmo de los ingresos")+
  ggtitle("Población ocupada por ingreso mensual y por edad. \n ENOE 3er Trimestre 2018") +
  theme_bw()+
  labs(fill = "Sexo",
       caption = "Ingreso calculado con base en las horas trabajadas por semana e ingreso por hora.")
ingmens + coord_flip()

# Histograma ingresos salariales hombrs y mujeres 3t2018
hist318 <- ggplot(na.omit(t318.ingreso), aes(x = log(ingmens), 
                                             fill = SEX,
                                             weight = FAC)) +
  geom_histogram(position = "identity",alpha = 0.25) +
  xlab("Logaritmo de los ingresos")+
  ylab("Frecuencia")+
  labs(fill = "Sexo",
       caption  ="Ingreso calculado con base en las horas trabajadas por semana e ingreso por hora.")+
  ggtitle("Histograma de la población ocupada por ingreso mensual. \n ENOE 3er Trimestre 2018")+
  theme_bw()
hist318


#### Rama e ingreso 3t 2018
crosstab(t318.trc$RAMA, t318.trc$ING7C, weight = t318.trc$FAC)


###########################################################################
# T1 2019
###########################################################################

library(descr)
library(questionr)

wtd.table(t119.trc$CLASE2, t119.trc$EDA7C, weights = t119.trc$FAC)

wtd.table(t119.trc$CLASE2, t119.trc$NIV_INS, weights = t119.trc$FAC)

wtd.table(t217.sdem.trc$clase2, t217.sdem.trc$eda7c,
          weights = t217.sdem.trc$fac)

wtd.table(t217.sdem.stl$clase2, t217.sdem.stl$eda7c,
          weights = t217.sdem.stl$fac)
wtd.table(t119.sdem.stl$CLASE2, t119.sdem.stl$EDA7C,
          weights = t119.sdem.stl$FAC)

t119.trc.ocup  <- subset(t119.trc, CLASE2 == 1)

wtd.table(t119.trc.ocup$ING7C, t119.trc.ocup$RAMA_EST1,
          weights = t119.trc.ocup$FAC)

wtd.table(t119.trc.ocup$ING7C, t119.trc.ocup$NIV_INS,
          weights = t119.trc.ocup$FAC)

wtd.table(t119.trc.ocup$ING7C, t119.trc.ocup$POS_OCU,
          weights = t119.trc.ocup$FAC)

wtd.table(t119.trc.ocup$DUR9C, t119.trc.ocup$POS_OCU,
          weights = t119.trc.ocup$FAC)


wtd.table(t119.trc.ocup$RAMA_EST2, t119.trc.ocup$POS_OCU,
          weights = t119.trc.ocup$FAC)

wtd.table(t119.trc$TCCO, t119.trc$EDA7C,
          weights = t119.trc$FAC)

wtd.table(t217.sdem.trc$tcco, t217.sdem.trc$eda7c,
          weights = t217.sdem.trc$fac)

wtd.table(t119.sdem.stl$TCCO, t119.sdem.stl$EDA7C,
          weights = t119.sdem.stl$FAC)

wtd.table(t217.sdem.stl$tcco, t217.sdem.stl$eda7c,
          weights = t217.sdem.stl$fac)

eda7c1 <- subset(t119.trc, EDA7C = 1)
eda7c2 <- subset(t119.trc, EDA7C = 2)
eda7c3 <- subset(t119.trc, EDA7C = 3)
eda7c4 <- subset(t119.trc, EDA7C = 4)
eda7c5 <- subset(t119.trc, EDA7C = 5)
eda7c6 <- subset(t119.trc, EDA7C = 6)

#1 Ocupados que trabajan menos de 35hrs por razones de mercado
#2 ocupados que trabajan de 35 mas horas o más y ganan hasta un salario minimo
#3 ocupados que trabajan más de 48 horas con ingresos de mas de 1 hasta 2 s.m.

wtd.mean(t119.trc$HRSOCUP, weights = t119.trc$FAC, na.rm = TRUE)

rama2.lev <- c(1,2,3,4,5,6,7,8,9,10,11)
rama2.lab <- c("Agricultura",
               "Ind. Extractiva",
               "Ind. Manufacturera",
               "Construcción",
               "Comercio",
               "Restaurantes y alojamiento",
               "Transportes y comunicaciones",
               "Servicios profesionales",
               "Servicios sociales",
               "Servicios diversos",
               "Gobierno")
t119.trc$RAMA_EST2 <- factor(t119.trc$RAMA_EST2, levels = rama2.lev,
                             labels = rama2.lab)
t119.trc.rama2 <- t119.trc[!is.na(t119.trc$RAMA_EST2),]

p1 <- ggplot(t119.trc.rama2, aes(x = RAMA_EST2,
                                 weight = FAC,
                                 fill = RAMA_EST2))+
  geom_bar(stat = "count")
p1 + labs(x = "Sector de actividad económica",
          y = "Cantidad de personal ocupado",
          title = "Personal ocupado por actividad económica en Torreón \n ENOE 1T 2019")+
  scale_fill_discrete(name = "Sector de actividad") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        legend.position = "none")



######################################

# Análisis de la ENOE
library(readr)   # cargar datos
library(dplyr)   # para manipular datos
library(tidyr)   # para manipular datos
library(questionr) # para hacer tablas con factores de expansión
library(descr)   # para hacer tablas con factores de expansión, cruzadas, etc..
library(dummies) # para crear variables dummy
library(oaxaca)  # para hacer análisis de descomposición/diferencias
library(ggplot2) # para generar gráficas

t419.sdem <- read_csv("2019T4/SDEMT419.CSV")
t419.coe1 <- read_csv("2019T4/COE1T419.CSV")
t419.coe2 <- read_csv("2019T4/COE2T419.CSV")

# Se crea un df con los datos de Torreón, o Ciudad Autorepresentada 06

t419.sdem.trc <- subset(t419.sdem, t419.sdem$cd_a == "06")
t419.coe1.trc <- subset(t419.coe1, t419.coe1$cd_a == "06")
t419.coe2.trc <- subset(t419.coe2, t419.coe2$cd_a == "06")

# De acuerdo al documento "Conociendo la base de datos de la ENOE"
# es posible unir tablas siguiendo los siguientes pasos:

# 1. Eliminar todos los registros de SDEM que en el campo R_DEF sean diferente a 00 o Entrevista completa
unique(t419.sdem.trc$r_def)
t419.sdem.trc <- subset(t419.sdem.trc, r_def == "00")

# 2. Eliminar todos los registros de la tabla SDEM que en el campo C_RES sean iguales a 2 o Ausente Definitivo
unique(t419.sdem.trc$c_res)
t419.sdem.trc <- subset(t419.sdem.trc, c_res != 2)

# 3. Eliminar todos los registros de la tabla SDEM que en el campo EDA contengan los códigos de 00 a 14 a 99
unique(t419.sdem.trc$eda)
t419.sdem.trc <- t419.sdem.trc[!(t419.sdem.trc$eda %in%
                                   c("00","01","02","03",
                                     "04","05","06","07",
                                     "08","09","10","11", 
                                     "12","13", "14","99")),]

# 4. Registros finales de la tabla SDEMT
length(t419.sdem.trc)

# 5. Eliminar todos los registros de la tabla COE1 y COE2 que en el campo R_DEF sean diferentes de 00.
unique(t419.coe1.trc$r_def)
t419.coe1.trc <- t419.coe1.trc[!(t419.coe1.trc$r_def == "00")]


# 6. Unir las tres tablas con base a la clave principal
t419.trc <- merge(t419.sdem.trc, t419.coe1.trc) # Tabla SDEM con COE1

# Personas de Torreón, Económicamente Activas, Agrupadas por Sexo
t419.trc %>% filter(clase1 == 1) %>% group_by(sex) %>% summarise(n = sum(fac))

################ Personas Ocupadas #######################
# Personas hombres, ocupadas, por condición de ocupación
t419.trc %>% filter(sex == 1 & clase2 == 1) %>%
  group_by(c_ocu11c) %>%  summarise(n = sum(fac))

# Personas mujeres, ocupadas, por condición de ocupación
t419.trc %>% filter(sex == 2 & clase2 == 1) %>%
  group_by(c_ocu11c) %>%  summarise(n = sum(fac))

# Personas hombres, ocupados, por sector de actividad
t419.trc %>% filter(sex == 1 & clase2 == 1) %>%
  group_by(rama_est2) %>%  summarise(n = sum(fac))

# Personas hombres, ocupados, por sector de actividad
t419.trc %>% filter(sex == 2 & clase2 == 1) %>%
  group_by(rama_est2) %>%  summarise(n = sum(fac))

# Personas hombres, ocupados, por nivel de instrucción
t419.trc %>% filter(sex == 1 & clase2 == 1) %>%
  group_by(niv_ins) %>%  summarise(n = sum(fac))

# Personas hombres, ocupados, por nivel de instrucción e ingreso por hora
t419.trc %>% filter(sex == 1 & clase2 == 1) %>%
  group_by(niv_ins) %>%  summarise(ingreso = weighted.mean(ing_x_hrs, fac))

# Personas mujeres, ocupados, por nivel de instrucción e ingreso por hora
t419.trc %>% filter(sex == 2 & clase2 == 1) %>%
  group_by(niv_ins) %>%  summarise(ingreso = weighted.mean(ing_x_hrs, fac))

# Personas hombres, ocupados, por nivel de instrucción y horas ocupadas
t419.trc %>% filter(sex == 1 & clase2 == 1) %>%
  group_by(niv_ins) %>%  summarise(ingreso = weighted.mean(hrsocup, fac))

# Personas mujeres, ocupados, por nivel de instrucción  y horas ocupadas
t419.trc %>% filter(sex == 2 & clase2 == 1) %>%
  group_by(niv_ins) %>%  summarise(ingreso = weighted.mean(hrsocup, fac))

# Personas ocupadas, por años de escolaridad
t419.trc %>% filter(clase2 == 1) %>%
  group_by(sex, seg_soc) %>% summarise(n = sum(fac))

### Personas subordinadas y remuneradas

# Personas ocupadas, por prestaciones
t419.trc %>% filter(clase2 == 1 & pos_ocu == 1) %>%
  group_by(sex, pre_asa) %>% summarise(n = sum(fac))


# Personas ocupadas, por tipo de contrato
t419.trc %>% filter(clase2 == 1 & pos_ocu == 1) %>%
  group_by(sex, tip_con) %>% summarise(n = sum(fac))

# Personas ocupadas, por tipo de trabajo de la primera actividad
t419.trc %>% filter(clase2 == 1 & pos_ocu == 1) %>%
  group_by(emp_ppal, sex) %>% summarise(n = sum(fac))


################ Personas Desocupadas ##################
t419.trc %>% filter(clase2 == 2) %>% group_by(sex) %>% summarise(n = sum(fac))

# Personas desocupadas por edad y sexo
t419.trc %>% filter(clase2 == 2 & sex == 1) %>% group_by(eda7c) %>% summarise(n = sum(fac))

# Personas desocupadas por edad y sexo
t419.trc %>% filter(clase2 == 2 & sex == 2) %>% group_by(eda7c) %>% summarise(n = sum(fac))

# Personas desocupadas por edad, sexo y experiencia
t419.trc %>% filter(clase2 == 2 & sex == 1) %>% group_by(eda7c, d_ant_lab) %>% summarise(n = sum(fac))

# Personas desocupadas por edad, sexo y experiencia
t419.trc %>% filter(clase2 == 2 & sex == 2) %>% group_by(eda7c, d_ant_lab) %>% summarise(n = sum(fac))


# Personas desocupadas por edad, sexo y duración del desempleo
t419.trc %>% filter(clase2 == 2 & sex == 1) %>% group_by(eda7c, dur_des) %>% summarise(n = sum(fac))

# Personas desocupadas por edad, sexo y duración del desempleo
t419.trc %>% filter(clase2 == 2 & sex == 2) %>% group_by(eda7c, dur_des) %>% summarise(n = sum(fac))

# Personas desocupadas por nivel de instrucción
t419.trc %>% filter(clase2 == 2 & sex == 1) %>% group_by(niv_ins) %>% summarise(n = sum(fac))

# Personas desocupadas por edad, sexo y duración del desempleo
t419.trc %>% filter(clase2 == 2 & sex == 2) %>% group_by(niv_ins) %>% summarise(n = sum(fac))


# Población No Económicamente Activa

# PNEA Hombres, por condición de inactividad
t419.trc %>% filter(clase1 == 2 & sex == 1) %>% group_by(c_inac5c) %>% summarise(n = sum(fac))

# PNEA Mujeres, por condición de inactividad
t419.trc %>% filter(clase1 == 2 & sex == 2) %>% group_by(c_inac5c) %>% summarise(n = sum(fac))

# PNEA Hombres, por composición
t419.trc %>% filter(clase1 == 2 & sex == 1) %>% group_by(pnea_est) %>% summarise(n = sum(fac))

# PNEA Mujeres, por composición
t419.trc %>% filter(clase1 == 2 & sex == 2) %>% group_by(pnea_est) %>% summarise(n = sum(fac))

# PNEA Hombres, por condición de inactividad y nivel de instruccion
t419.trc %>% filter(clase1 == 2 & sex == 1) %>% group_by(niv_ins, c_inac5c) %>% summarise(n = sum(fac))

# PNEA Mujeres, por condición de inactividad y nivel de instrucción
t419.trc %>% filter(clase1 == 2 & sex == 2) %>% group_by(niv_ins, c_inac5c) %>% summarise(n = sum(fac))


 
                                            