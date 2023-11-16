# Encuesta Nacional sobre la Discriminación en México (ENADIS) 2017

library(foreign)
library(questionr)
library(descr)
library(dplyr)
library(ggplot2)

# See leen los archivos dbf para cargarlos en el Environment

vivienda <- read.dbf("tvivienda.DBF")
hogar <- read.dbf("thogar.DBF")
sdem <- read.dbf("tsdem.DBF")
indigena <- read.dbf("tindigena.DBF")
indigena_v <- read.dbf("tindigena_v.DBF")
disc <- read.dbf("tdiscapacidad.DBF")
disc_v <- read.dbf("tdiscapacidad_v.DBF")
religion <- read.dbf("treligion.DBF")
religion_v <- read.dbf("treligion_V.DBF")
adulto <- read.dbf("tadulto.DBF")
nino <- read.dbf("tnino.DBF")
adol <- read.dbf("tadolescente.DBF")
mujer <- read.dbf("tmujer.DBF")
coe <- read.dbf("tcoe.DBF")

# Se filtran los valores por los que tengan ENT == "05"

unique(vivienda$ENT)
coah.viv <- subset(vivienda, ENT == "05")
coah.sdem <- subset(sdem, ENT == "05")
coah.ind <- subset(indigena, ENT == "05")
coah.indv <- subset(indigena_v, ENT == "05")
coah.disc <- subset(disc, ENT == "05")
coah.discv <- subset(disc_v, ENT == "05")
coah.rel <- subset(religion, ENT == "05")
coah.relv <- subset(religion_v, ENT == "05")
coah.adul <- subset(adulto, ENT == "05")
coah.nino <- subset(nino, ENT == "05")
coah.ado <- subset(adol, ENT == "05")
coah.muj <- subset(mujer, ENT == "05")
coah.coe <- subset(coe, ENT == "05")

#-----------------------------------------------------------
# Tabla Sociodemografía
#-----------------------------------------------------------
# Se extrae el factor de expansión de la tabla
sdem.fac <- coah.sdem$FACTOR

# Hombre (1) o mujer (2)
p3.4.lev <- c(1,2)
p3.4.lab <- c("Hombre", "Mujer")
p3.4 <- factor(coah.sdem$SEXO, levels = p3.4.lev,
               labels = p3.4.lab)
wtd.table(p3.4, weights = sdem.fac)

# 3.8 Se considera negro afromexicano
p3.8.lev <- c(1,2,9)
p3.8.lab <- c("Si", "No", "No sabe")
p3.8 <- factor(coah.sdem$P3_8, levels = p3.8.lev,
               labels = p3.8.lab)
wtd.table(p3.8, weights = sdem.fac)
CrossTable(p3.8)

# 3.9 Habla alguna lengua indígena
p3.9.lev <- c(1,2,9)
p3.9.lab <- c("Si", "No", "No sabe")
p3.9 <- factor(coah.sdem$P3_9, levels = p3.9.lev,
               labels = p3.9.lab)
wtd.table(p3.9, weights = sdem.fac)
CrossTable(p3.9)

# 3.11 Cuál es su religión
p3.11.lev <- c(1,2,3,4,5,6,9)
p3.11.lab <- c("Católica", "Cristiana",
               "Testigos de Jehová", "Pentecostés o Protestante",
               "Otra", "No tiene religión", "No sabe")
p3.11 <- factor(coah.sdem$P3_11, levels = p3.11.lev,
                labels = p3.11.lab)
wtd.table(p3.11, weights = sdem.fac)

# P5.1 Identificación de grupos
sino.lev <- c(1,2)
sino.lab <- c("Si", "No")

# Indígena

p5.1.indigena <- factor(sdem$P5_1_1, levels = sino.lev,
                        labels = sino.lab)
wtd.table(p5.1.indigena)

# Discapacidad
p5.1.discapacidad <- factor(sdem$P5_1_2, levels = sino.lev,
                        labels = sino.lab)
wtd.table(p5.1.discapacidad)
# Diversidad religiosa
p5.1.divrel <- factor(sdem$P5_1_3, levels = sino.lev,
                        labels = sino.lab)
wtd.table(p5.1.divrel)
# Adulta mayor
p5.1.mayor <- factor(sdem$P5_1_4, levels = sino.lev,
                        labels = sino.lab)
wtd.table(p5.1.mayor)
# Niña o Niño
p5.1.nino <- factor(sdem$P5_1_5, levels = sino.lev,
                        labels = sino.lab)
wtd.table(p5.1.nino)
# Adolescente o joven
p5.1.ado <- factor(sdem$P5_1_6, levels = sino.lev,
                        labels = sino.lab)
wtd.table(p5.1.ado)
# Mujer
p5.1.mujer <- factor(sdem$P5_1_7, levels = sino.lev,
                        labels = sino.lab)
wtd.table(p5.1.mujer)


#-----------------------------------------------------------
# Tabla Indígenas
#-----------------------------------------------------------
ind.fac <- coah.ind$FACTOR_PER
# usted se considera indígena
wtd.table(coah.ind$PMVI_1, weights = ind.fac)
# se considera indígena porque
wtd.table(coah.ind$PMVI_2, weights = ind.fac)
# la persona que no responde el módulo poque
wtd.table(coah.ind$PMVI_3, weights = ind.fac)

# 1.1 en su opinión, los indígenas se respetan
wtd.table(coah.ind$PM1_1, weights = ind.fac)
# 1.2 cuál consideras que es el principal problema para las personas indígenas
wtd.table(coah.ind$PM1_2, weights = ind.fac)
# 1.3 cuál consideras que sea el principal problema de los pueblos indígenas
wtd.table(coah.ind$PM1_3, weights = ind.fac)
# 1.4 usted considera que conservar costumbres y tradiciones indígenas es una ventaja
wtd.table(coah.ind$PM1_4, weights = ind.fac)

# 1.5 dígame si está de acuerdo con las siguientes frases
# las personas indígenas valoran poco el seguir estudiando
wtd.table(coah.ind$PM1_5_1, weights = ind.fac)
# 2 Las personas indígenas rechazan la tecnología (internet, computadora, celular)
wtd.table(coah.ind$PM1_5_2, weights = ind.fac)
# Las personas indígenas son poco valoradas por la mayoría de la gente
wtd.table(coah.ind$PM1_5_3, weights = ind.fac)
# 4 Las personas indígenas son consideradas poco trabajadoras por la mayoría de la gente
wtd.table(coah.ind$PM1_5_4, weights = ind.fac)


# 1.6 De agosto de 2016 a la fecha, ¿cuáles son los problemas que ha tenido cuando ha querido buscar información sobre algún trámite, servicio o programa de gobierno?
# No entiende lo que dice porque sólo está en español
wtd.table(coah.ind$PM1_6_1, weights = ind.fac)
# Sólo está en Internet (carece de acceso)
wtd.table(coah.ind$PM1_6_2, weights = ind.fac)
# se le dificulta usar el equipo
wtd.table(coah.ind$PM1_6_3, weights = ind.fac)
# las oficinas quedan lejos
wtd.table(coah.ind$PM1_6_4, weights = ind.fac)
# le niegan información, no le explican
wtd.table(coah.ind$PM1_6_5, weights = ind.fac)
# desconoce dónde buscarla
wtd.table(coah.ind$PM1_6_6, weights = ind.fac)
# otra
wtd.table(coah.ind$PM1_6_7, weights = ind.fac)
# no ha buscado
wtd.table(coah.ind$PM1_6_8, weights = ind.fac)
# no ha tenido problemas
wtd.table(coah.ind$PM1_6_9, weights = ind.fac)

# Experiencias de discriminación
# En los últimos cinco años, ¿le han negado injustificadamente
# 1 la atención médica o medicamentos?
wtd.table(coah.ind$PM8_1_1, weights = ind.fac)
# 2 la atención o servicios en alguna oficina de gobierno?
wtd.table(coah.ind$PM8_1_2, weights = ind.fac)
# 3 la entrada o permanencia en algún negocio, centro comercial o banco?
wtd.table(coah.ind$PM8_1_3, weights = ind.fac)
# 4 recibir apoyos de programas sociales (becas, PROSPERA, etcétera)?
wtd.table(coah.ind$PM8_1_4, weights = ind.fac)
# 5 la posibilidad de estudiar o seguir estudiando?
wtd.table(coah.ind$PM8_1_5, weights = ind.fac)
# 6 la oportunidad de trabajar u obtener un ascenso?
wtd.table(coah.ind$PM8_1_6, weights = ind.fac)
# 7 algún crédito de vivienda, préstamo o tarjeta?
wtd.table(coah.ind$PM8_1_7, weights = ind.fac)

# 8.2 Debido a esta situación, ¿acudió a denunciarlo ante...
wtd.table(coah.ind$PM8_2, weights = ind.fac)
# 8.3 ¿Por qué no lo hizo?
wtd.table(coah.ind$PM8_3, weights = ind.fac)

# 8.4 En los últimos años...¿le ha sucedido que...
# 1 lo(a) rechacen o excluyan de actividades sociales?
wtd.table(coah.ind$PM8_4_1, weights = ind.fac)

# 8.5 El que lo(a) hayan (ADECUAR RESPUESTA DE 8.2), ¿considera que le ocurrió por…"
# 1 ser persona indígena?
wtd.table(coah.ind$PM8_5_1_1, weights = ind.fac)
# 4 su edad?
wtd.table(coah.ind$PM8_5_1_4, weights = ind.fac)
# 5 ser mujer?
wtd.table(coah.ind$PM8_5_1_5, weights = ind.fac)
# 6 ¿Otra situación?
wtd.table(coah.ind$PM8_5_1_6, weights = ind.fac)

#



#-----------------------------------------------------------
# Tabla Discapacidad
#-----------------------------------------------------------


#-----------------------------------------------------------
# Tabla Religión
#-----------------------------------------------------------
rel.factor <- coah.rel$FACTOR_PER
rel.3.3.lev <- c(1,2,9)
rel.3.3.lab <- c("Si-de acuerdo","No-en desacuerdo", "No sabe")

# Las personas con religión distinta a la católica son más confiables que el resto de la población
rel.3.3 <- factor(coah.rel$PM3_3_1, levels = rel.3.3.lev,
                  labels = rel.3.3.lab)
wtd.table(rel.3.3, weights = rel.factor)

#-----------------------------------------------------------
# Tabla Niñas y Niños
#-----------------------------------------------------------



#-----------------------------------------------------------
# Tabla Adolescente
#-----------------------------------------------------------
ado.fac <- coah.ado$FACTOR_PER

# 6.2 cual consideras que es el principal problema para ados
ado.6.2.lev <- c(1,2,3,4,5,6,7,8)
ado.6.2.lab <- c("Falta de empleo",
                 "Falta de oportunidades para seguir estudiando",
                 "Adicciones",
                 "Violencia e inseguridad",
                 "Embarazo en la adolescencia",
                 "Acoso escolar o bullying",
                 "Problemas personas o familiares",
                 "Otro")
ado.6.2 <- factor(coah.ado$PM6_2, levels = ado.6.2.lev,
                  labels = ado.6.2.lab)
wtd.table(ado.6.2, weights = ado.fac)

# 6.4 de agosto 2016 a la fecha, te han hecho bullying?

ado.6.4 <- factor(coah.ado$PM6_4, levels = sino.lev,
                  labels = sino.lab)
wtd.table(ado.6.4, weights = ado.fac)

# 6.6 Cuál es el motivo por el que no trabajas
wtd.table(coah.ado$PM6_6, weights = ado.fac)

# 8.6 Le han menospreciado por
# tono de piel
wtd.table(coah.ado$PM8_6_01, weights = ado.fac)
# manera de hablar
wtd.table(coah.ado$PM8_6_02, weights = ado.fac)
# peso o estatura
wtd.table(coah.ado$PM8_6_03, weights = ado.fac)
# forma de vestir o arreglarse
wtd.table(coah.ado$PM8_6_04, weights = ado.fac)
# su clase social
wtd.table(coah.ado$PM8_6_05, weights = ado.fac)
# lugar donde vive
wtd.table(coah.ado$PM8_6_06, weights = ado.fac)
# creencias religiosas
wtd.table(coah.ado$PM8_6_07, weights = ado.fac)
# ser mujer (hombre)
wtd.table(coah.ado$PM8_6_08, weights = ado.fac)
# su edad
wtd.table(coah.ado$PM8_6_09, weights = ado.fac)
#
wtd.table(coah.ado$PM8_6_10, weights = ado.fac)

# 8.7 lo han discriminado o menospreciado en...
sino2.lev <- c(1,2,3)
sino2.lab <- c("Si", "No", "No aplica")

# trabajo o escuela
wtd.table(coah.ado$PM8_7_1, weights = ado.fac)
# su familia
wtd.table(coah.ado$PM8_7_2, weights = ado.fac)
# servicios médicos u hospitales
wtd.table(coah.ado$PM8_7_3, weights = ado.fac)
# alguna oficina de gobierno
wtd.table(coah.ado$PM8_7_4, weights = ado.fac)
# negocio, centro comercial, o banco
wtd.table(coah.ado$PM8_7_5, weights = ado.fac)
# calle o transporte público
wtd.table(coah.ado$PM8_7_6, weights = ado.fac)
# redes sociales
wtd.table(coah.ado$PM8_7_7, weights = ado.fac)
# otro
wtd.table(coah.ado$PM8_7_8, weights = ado.fac)




#-------------------
# Tabla Mujer
#-------------------
muj.fac <- coah.muj$FACTOR_PER

# 7.1
wtd.table(coah.muj$PM7_1, weights = muj.fac)
# 7.2
wtd.table(coah.muj$PM7_2, weights = muj.fac)
# 7.3 Dígame si está de acuerdo con las siguientes frases
wtd.table(coah.muj$PM7_3_1, weights = muj.fac)
wtd.table(coah.muj$PM7_3_2, weights = muj.fac)
wtd.table(coah.muj$PM7_3_3, weights = muj.fac)
wtd.table(coah.muj$PM7_3_4, weights = muj.fac)
wtd.table(coah.muj$PM7_3_5, weights = muj.fac)

# 7.4 En su hogar, ¿quién decide la mayor parte de las veces...
wtd.table(coah.muj$PM7_4_1, weights = muj.fac)
wtd.table(coah.muj$PM7_4_2, weights = muj.fac)
wtd.table(coah.muj$PM7_4_3, weights = muj.fac)
wtd.table(coah.muj$PM7_4_4, weights = muj.fac)
wtd.table(coah.muj$PM7_4_5, weights = muj.fac)

# 7.5 le ha sucedido que le den menor paga con respecto a un hombre por realizar el mismo trabajo
wtd.table(coah.muj$PM7_5, weights = muj.fac)

# 7.6 Por qué no trabaja
wtd.table(coah.muj$PM7_6, weights = muj.fac)

# 7.7 Realizó actividades domésticas a cambio de un pago
wtd.table(coah.muj$PM7_7, weights = muj.fac)

# 7.8 los derechos de las empleadas domésticas se respetan
wtd.table(coah.muj$PM7_8, weights = muj.fac)

# 7.9 cúal considera que es el principal problema de las empeladas domésticas
wtd.table(coah.muj$PM7_9, weights = muj.fac)

# 7.10 Dígame si está de acuerdo con las siguientes frases
wtd.table(coah.muj$PM7_10_1, weights = muj.fac)
wtd.table(coah.muj$PM7_10_2, weights = muj.fac)
wtd.table(coah.muj$PM7_10_3, weights = muj.fac)

# 7.11 en su actual trabajo como empleada doméstica
wtd.table(coah.muj$PM7_11_1, weights = muj.fac)
wtd.table(coah.muj$PM7_11_2, weights = muj.fac)

# 7.12 en esa casa duerme
wtd.table(coah.muj$PM7_12, weights = muj.fac)

# 7.13 en esa casa
# comia de los mismos alimentos que la familia
wtd.table(coah.muj$PM7_13_1, weights = muj.fac)
# tiene baño
wtd.table(coah.muj$PM7_13_2, weights = muj.fac)
# trabaja máximo 8h al día
wtd.table(coah.muj$PM7_13_3, weights = muj.fac)
# le dan permiso de salir si tiene alguna necesidad
wtd.table(coah.muj$PM7_13_4, weights = muj.fac)

# 7.14 en este mismo trabajo, quién cubrió los gastos del doctor y las medicinas
wtd.table(coah.muj$PM7_14, weights = muj.fac)

# 7.15 le ha sucedido que no le paguen por su trabajo
wtd.table(coah.muj$PM7_15, weights = muj.fac)

# 8.1 en los últimos cinco años le han negado injustificadamente
# atención médica
wtd.table(coah.muj$PM8_1_1, weights = muj.fac)
# atención o servicios en oficina de gobierno
wtd.table(coah.muj$PM8_1_2, weights = muj.fac)
# la entrada o permanencia en algún negocio, centro comercial o banco
wtd.table(coah.muj$PM8_1_3, weights = muj.fac)
# recibir apoyos de programas sociales
wtd.table(coah.muj$PM8_1_4, weights = muj.fac)
# la posibilidad de estudiar
wtd.table(coah.muj$PM8_1_5, weights = muj.fac)
# la oportunidad de trabajar u obtener un ascenso
wtd.table(coah.muj$PM8_1_6, weights = muj.fac)
# algún crédito de vivienda, préstamo o tarjeta
wtd.table(coah.muj$PM8_1_7, weights = muj.fac)

# 8.2 debido a esta situacion, usted acudio a denunciar...
wtd.table(coah.muj$PM8_2, weights = muj.fac)

# 8.3 por qué no lo hizo
wtd.table(coah.muj$PM8_3, weights = muj.fac)

# 8.4 en los últimos cinco años le ha sucedido que
# 8.4.1lo rechacen o exluyan de actividades sociales
wtd.table(coah.muj$PM8_4_1, weights = muj.fac)

#8.5.1 considera que le ocurrió por:
wtd.table(coah.muj$PM8_5_1_4, weights = muj.fac)
wtd.table(coah.muj$PM8_5_1_5, weights = muj.fac)
wtd.table(coah.muj$PM8_5_1_6, weights = muj.fac)

# 8.4.2 lo hagan sentir o miren de forma incómoda
wtd.table(coah.muj$PM8_4_2, weights = muj.fac)

wtd.table(coah.muj$PM8_5_2_4, weights = muj.fac)
wtd.table(coah.muj$PM8_5_2_5, weights = muj.fac)
wtd.table(coah.muj$PM8_5_2_6, weights = muj.fac)

# 8.4.3 lo insulten, se burlen o dicho cosas que le molestaran
wtd.table(coah.muj$PM8_4_3, weights = muj.fac)

wtd.table(coah.muj$PM8_5_3_4, weights = muj.fac)
wtd.table(coah.muj$PM8_5_3_5, weights = muj.fac)
wtd.table(coah.muj$PM8_5_3_6, weights = muj.fac)

# 8.4.4 lo amenacen, empujen o jaloneen
wtd.table(coah.muj$PM8_4_4, weights = muj.fac)

wtd.table(coah.muj$PM8_5_4_4, weights = muj.fac)
wtd.table(coah.muj$PM8_5_4_5, weights = muj.fac)
wtd.table(coah.muj$PM8_5_4_6, weights = muj.fac)

#8.4.5 lo obliguen a salir de alguna comunidad
wtd.table(coah.muj$PM8_4_5, weights = muj.fac)

wtd.table(coah.muj$PM8_5_5_4, weights = muj.fac)
wtd.table(coah.muj$PM8_5_5_5, weights = muj.fac)
wtd.table(coah.muj$PM8_5_5_6, weights = muj.fac)

# 8.6 De agosto 2016 a la fecha, ha sido discriminado por
# su tono de piel
wtd.table(coah.muj$PM8_6_01, weights = muj.fac)
# su manera de hablar
wtd.table(coah.muj$PM8_6_02, weights = muj.fac)
# su peso o estatura
wtd.table(coah.muj$PM8_6_03, weights = muj.fac)
# su forma de vesitor o arreglarse
wtd.table(coah.muj$PM8_6_04, weights = muj.fac)
# su clase social
wtd.table(coah.muj$PM8_6_05, weights = muj.fac)
# lugar dónde vive
wtd.table(coah.muj$PM8_6_06, weights = muj.fac)
# sus creencias religiosas
wtd.table(coah.muj$PM8_6_07, weights = muj.fac)
# ser mujer
wtd.table(coah.muj$PM8_6_08, weights = muj.fac)
# su edad
wtd.table(coah.muj$PM8_6_09, weights = muj.fac)
# su preferencia sexual
wtd.table(coah.muj$PM8_6_10, weights = muj.fac)

# 8.7 en el útlimo año, lo han discriminado en...
# trabajo o escuela
wtd.table(coah.muj$PM8_7_1, weights = muj.fac)
# su familia
wtd.table(coah.muj$PM8_7_2, weights = muj.fac)
# en los servicios médicos
wtd.table(coah.muj$PM8_7_3, weights = muj.fac)
# alguna oficina de gobierno
wtd.table(coah.muj$PM8_7_4, weights = muj.fac)
# en un negocio, centro comercial o banco
wtd.table(coah.muj$PM8_7_5, weights = muj.fac)
# calle o transporte público
wtd.table(coah.muj$PM8_7_6, weights = muj.fac)
# en las redes sociales
wtd.table(coah.muj$PM8_7_7, weights = muj.fac)
# otro
wtd.table(coah.muj$PM8_7_8, weights = muj.fac)


# variables uso general
wtd.table(coah.muj$SEXO, weights = muj.fac)
wtd.table(coah.muj$SEXO)


#-----------------------------------------
# Cuestionario de Opinión y Experiencias
#-----------------------------------------

coe.fac <- coah.coe$FACTOR_PER

# 1.1 si pudiera rentar un cuarto de su casa, se lo rentaría a
# adulto mayor
wtd.table(coah.coe$PO1_1_01, weights = coe.fac)
# joven
wtd.table(coah.coe$PO1_1_02, weights = coe.fac)
# con discapacidad
wtd.table(coah.coe$PO1_1_03, weights = coe.fac)
# indígena
wtd.table(coah.coe$PO1_1_04, weights = coe.fac)
# extranjera
wtd.table(coah.coe$PO1_1_05, weights = coe.fac)
# de una religión distinta
wtd.table(coah.coe$PO1_1_06, weights = coe.fac)
# negro afromexicano
wtd.table(coah.coe$PO1_1_07, weights = coe.fac)
# gay o lesbiana
wtd.table(coah.coe$PO1_1_08, weights = coe.fac)
# sida o vih
wtd.table(coah.coe$PO1_1_09, weights = coe.fac)
# trans
wtd.table(coah.coe$PO1_1_10, weights = coe.fac)


# 1.2 estaría de acuerdo en que su hija o hijo se casara con una persona
# pobre
wtd.table(coah.coe$PO1_2_1, weights = coe.fac)
# indígena
wtd.table(coah.coe$PO1_2_2, weights = coe.fac)
# con discapacidad
wtd.table(coah.coe$PO1_2_3, weights = coe.fac)
# negra afrodescendiente
wtd.table(coah.coe$PO1_2_4, weights = coe.fac)
# extranjera
wtd.table(coah.coe$PO1_2_5, weights = coe.fac)
# de religión distinta
wtd.table(coah.coe$PO1_2_6, weights = coe.fac)
# del mismo sexo
wtd.table(coah.coe$PO1_2_7, weights = coe.fac)
# con sida o vih
wtd.table(coah.coe$PO1_2_8, weights = coe.fac)

# 1.3 cuánto le gustaría que se eligiera para la Presidencia de la repúiblica a una persona...
# adulta mayor
wtd.table(coah.coe$PO1_3_1, weights = coe.fac)
# mujer
wtd.table(coah.coe$PO1_3_2, weights = coe.fac)
# persona con discapacidad
wtd.table(coah.coe$PO1_3_3, weights = coe.fac)
# indígena
wtd.table(coah.coe$PO1_3_4, weights = coe.fac)
# persona nacida en el extranjero
wtd.table(coah.coe$PO1_3_5, weights = coe.fac)
# persona negra afrodescendiente
wtd.table(coah.coe$PO1_3_6, weights = coe.fac)
# gay o lesbiana
wtd.table(coah.coe$PO1_3_7, weights = coe.fac)

# 1.4 cuánto se justifica que
# un hombre le pegue a una mujer
wtd.table(coah.coe$PO1_4_1, weights = coe.fac)
# llamar a la policía cuando hay jovenes reunidos en una esquina
wtd.table(coah.coe$PO1_4_2, weights = coe.fac)
# negarle un empleo a una persona adulta mayor
wtd.table(coah.coe$PO1_4_3, weights = coe.fac)
# burlarse de alguién en público por su tono de piel
wtd.table(coah.coe$PO1_4_4, weights = coe.fac)
# pegarle a un niño o una niña para que obedezca
wtd.table(coah.coe$PO1_4_5, weights = coe.fac)
# que dos personas del mismo sexo vivan juntas como pareja
wtd.table(coah.coe$PO1_4_6, weights = coe.fac)
# que las personas practiquen costumbres o tradiciones diferentes a las mexicanas
wtd.table(coah.coe$PO1_4_7, weights = coe.fac)



# Percepciones

# 2.1 Cuando hay conflictos entre la gente de un mismo vecindario,
# colonia o localidad
# cuánto considera se deba a diferencias en...

# niveles educativos
wtd.table(coah.coe$PO2_1_1, weights = coe.fac)
# clases sociales
wtd.table(coah.coe$PO2_1_2, weights = coe.fac)
# costumbres o tradiciones
wtd.table(coah.coe$PO2_1_3, weights = coe.fac)
# creencias religiosas
wtd.table(coah.coe$PO2_1_4, weights = coe.fac)
# ideas políticas
wtd.table(coah.coe$PO2_1_5, weights = coe.fac)
# ser originario de otro lugar
wtd.table(coah.coe$PO2_1_6, weights = coe.fac)
# los valores familiares
wtd.table(coah.coe$PO2_1_7, weights = coe.fac)

# 2.2 en su opinión, cuánto se respetan en el país los derechos de...
# las personas adultas
wtd.table(coah.coe$PO2_2_01, weights = coe.fac)
# las mujeres
wtd.table(coah.coe$PO2_2_02, weights = coe.fac)
# las niñas y los niños
wtd.table(coah.coe$PO2_2_03, weights = coe.fac)
# las y los adolescnetes
wtd.table(coah.coe$PO2_2_04, weights = coe.fac)
# las personas con discapacidad
wtd.table(coah.coe$PO2_2_05, weights = coe.fac)
# las personas indígenas
wtd.table(coah.coe$PO2_2_06, weights = coe.fac)
# las personas con religión distinta a la católica
wtd.table(coah.coe$PO2_2_07, weights = coe.fac)
# las personas negras
wtd.table(coah.coe$PO2_2_08, weights = coe.fac)
# las personas gays o lesbianas
wtd.table(coah.coe$PO2_2_09, weights = coe.fac)
# las personas extranjeras
wtd.table(coah.coe$PO2_2_10, weights = coe.fac)
# las empleadas domésticas
wtd.table(coah.coe$PO2_2_11, weights = coe.fac)
# las personas trans
wtd.table(coah.coe$PO2_2_12, weights = coe.fac)

# respecto a las decisiones del gobierno, cuánto debe tomarse en cuenta la opinión de
# personas adultas mayores
wtd.table(coah.coe$PO2_3_1, weights = coe.fac)
# las mujeres
wtd.table(coah.coe$PO2_3_2, weights = coe.fac)
# las niñas y los niños
wtd.table(coah.coe$PO2_3_3, weights = coe.fac)
# los adolescentes
wtd.table(coah.coe$PO2_3_4, weights = coe.fac)
# las personas con discapacidad
wtd.table(coah.coe$PO2_3_5, weights = coe.fac)
# personas indígenas
wtd.table(coah.coe$PO2_3_6, weights = coe.fac)
# personas negras afrodescendientes
wtd.table(coah.coe$PO2_3_7, weights = coe.fac)
# personas gays o lesbianas
wtd.table(coah.coe$PO2_3_8, weights = coe.fac)


# Prejuicios, estigmas sociales y estereotipos
#3.1 está de acuerdo con las siguientes frases
# algunas mujeres que son violadas es porque provocan a los hombres
wtd.table(coah.coe$PO3_1_01, weights = coe.fac)
# las mujeres debe ayudar en los quehaceres del hogar más que los hombres
wtd.table(coah.coe$PO3_1_02, weights = coe.fac)
# la mayoría de las y los jóvenes son irresponsables
wtd.table(coah.coe$PO3_1_03, weights = coe.fac)
# la pobreza de las personas indígenas se debe a su cultura
wtd.table(coah.coe$PO3_1_04, weights = coe.fac)
# a las parejas del mismo sexo se les debería permitir adoptar niños
wtd.table(coah.coe$PO3_1_05, weights = coe.fac)
# las personas con discapacidad son de poca ayuda en el trabajo
wtd.table(coah.coe$PO3_1_06, weights = coe.fac)
# los pobres se esfuerzan poco por salir de su pobreza
wtd.table(coah.coe$PO3_1_07, weights = coe.fac)
# cuando hay desempleo, debe negarse el trabajo a personas extranjeras
wtd.table(coah.coe$PO3_1_08, weights = coe.fac)
# convivir con personas con sida siempre es un riesgo
wtd.table(coah.coe$PO3_1_09, weights = coe.fac)
# mientras más religiones se permitan en el país, habrá más conflictos sociales
wtd.table(coah.coe$PO3_1_10, weights = coe.fac)

# 3.2 estás de acuerdo en que...
# en las elecciones haya el mismo número de hombres y mujeres como candidatos
wtd.table(coah.coe$PO3_2_1, weights = coe.fac)
# las personas del mismo sexo puedan contraer matrimonio civil
wtd.table(coah.coe$PO3_2_2, weights = coe.fac)
# existan los mismos derechos laborales para decidir si el hombre o mujer se quedan en la casa con el recién nacido
wtd.table(coah.coe$PO3_2_3, weights = coe.fac)

# 3.3 respecto a las personas centroamericanas, el gobierno debería...
wtd.table(coah.coe$PO3_3, weights = coe.fac)

# experiencias
#4.1 en los últimos doce meses, ha sido discriminado o menospreciado por
# su tono de piel
wtd.table(coah.coe$PO4_1_01, weights = coe.fac)
# su manera de hablar
wtd.table(coah.coe$PO4_1_02, weights = coe.fac)
# su peso o estatura
wtd.table(coah.coe$PO4_1_03, weights = coe.fac)
# su forma de vesitor o arreglarse
wtd.table(coah.coe$PO4_1_04, weights = coe.fac)
# su clase social
wtd.table(coah.coe$PO4_1_05, weights = coe.fac)
# lugar dónde vive
wtd.table(coah.coe$PO4_1_06, weights = coe.fac)
# sus creencias religiosas
wtd.table(coah.coe$PO4_1_07, weights = coe.fac)
# ser mujer
wtd.table(coah.coe$PO4_1_08, weights = coe.fac)
# su edad
wtd.table(coah.coe$PO4_1_09, weights = coe.fac)
# su preferencia sexual
wtd.table(coah.coe$PO4_1_10, weights = coe.fac)

# 4.2 le han negado injustificadamente
# atención médica
wtd.table(coah.coe$PO4_2_1, weights = coe.fac)
# atención o servicios en gobierno
wtd.table(coah.coe$PO4_2_2, weights = coe.fac)
# entrada o permanencia en algún negocio, centro comercial o banco
wtd.table(coah.coe$PO4_2_3, weights = coe.fac)
# apoyos de programas sociales
wtd.table(coah.coe$PO4_2_4, weights = coe.fac)
# crédito de vivienda
wtd.table(coah.coe$PO4_2_5, weights = coe.fac)
# posiblidad de estudiar
wtd.table(coah.coe$PO4_2_6, weights = coe.fac)

# 4.3 usted denunció ante
wtd.table(coah.coe$PO4_3, weights = coe.fac)

# 4.4 por qué no lo hizo
wtd.table(coah.coe$PO4_4, weights = coe.fac)

# 4.5 en los últimos cinco años, buscó empleo y se lo negaron
wtd.table(coah.coe$PO4_5, weights = coe.fac)

# 4.6 cuál fue el motivo
wtd.table(coah.coe$PO4_6, weights = coe.fac)

# reconocimiento personal e institucional
#5.1 de la siguiente tarjeta, cual diría que se parece más a su tono de piel
wtd.table(coah.coe$PO5_1, weights = coe.fac)

# 5.2 de la siguiente tarjeta, con cuál se identifica mejor
wtd.table(coah.coe$PO5_2, weights = coe.fac)

# 5.3 actualmente, conoce o tiene trato con alguna persona
# religión distinta a la católica
wtd.table(coah.coe$PO5_3_1, weights = coe.fac)
# indígena
wtd.table(coah.coe$PO5_3_2, weights = coe.fac)
# con discapacidad
wtd.table(coah.coe$PO5_3_3, weights = coe.fac)
# extranjera
wtd.table(coah.coe$PO5_3_4, weights = coe.fac)
# gay o lesbiana
wtd.table(coah.coe$PO5_3_5, weights = coe.fac)
# negra o afrodescendiente
wtd.table(coah.coe$PO5_3_6, weights = coe.fac)
# con sida o vih
wtd.table(coah.coe$PO5_3_7, weights = coe.fac)

# ha oido hablar del CONAPRED
wtd.table(coah.coe$PO5_4, weights = coe.fac)

# 6.1 en los últimos 5 años, estando acompañado o por su relación con lás personas de su hogar con discapacidad
# le ha sucedido que..
# lo rechacen o expluyan
wtd.table(coah.coe$PO6_1_1, weights = coe.fac)
# lo hagan sentir o miren de forma incomoda
wtd.table(coah.coe$PO6_1_2, weights = coe.fac)
# lo insulten, se burlen...
wtd.table(coah.coe$PO6_1_3, weights = coe.fac)
# le nieguen el acceso a algún lugar o servicio
wtd.table(coah.coe$PO6_1_4, weights = coe.fac)


# sexo
wtd.table(coah.coe$SEXO, weights = coe.fac)
wtd.table(coah.coe$SEXO)



#--------------------------
# Tablas unidas sdem + coe
#--------------------------

sdem.coe <- merge(coah.sdem, coah.coe)

crosstab(sdem.coe$NIV, sdem.coe$PO5_1, prop.r = TRUE, weight = sdem.coe$FACTOR)
