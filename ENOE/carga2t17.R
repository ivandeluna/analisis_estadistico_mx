
#--------------------------------------------------------------------------------------------
#
#                  Carga de archivos para la ENOE 2T 2017
#
#--------------------------------------------------------------------------------------------

library(readr)

#--------------------------------------------------------------------------------------------
#
#                  Leer archivos e importarlos a R, crear data frames
#
#--------------------------------------------------------------------------------------------

# Se leen los archivos csv de la ENOE o en su caso en otro formato
t217.coe1 <- read_csv("2017T2/coe1t217.CSV")
t217.coe2 <- read_csv("2017T2/coe2t217.CSV")
t217.hog <- read_csv("2017T2/hogt217.CSV")
t217.sdem <- read_csv("2017T2/sdemt217.CSV")
t217.viv <- read_csv("2017T2/vivt217.CSV")

# Se filtra para contener solo a Torreón, como ciudad autorepresentada

t217.coe1.trc <- subset(t217.coe1, t217.coe1$cd_a == "06")
t217.coe2.trc <- subset(t217.coe2, t217.coe2$cd_a == "06")
t217.hog.trc <- subset(t217.hog, t217.hog$cd_a == 6) # en esta tabla el valor es númerico y no caracter
t217.sdem.trc <- subset(t217.sdem, t217.sdem$cd_a == "06")

t217.sdem.stl <- subset(t217.sdem, t217.sdem$cd_a == "17")

# De acuerdo al documento "Conociendo la base de datos de la ENOE"
# es posible unir tablas siguiendo los siguientes pasos:

# 1. Eliminar todos los registros de SDEM que en el campo R_DEF sean diferente a 00 o Entrevista completa
unique(t217.sdem.trc$r_def)
t217.sdem.trc <- subset(t217.sdem.trc, r_def == "00")

# 2. Eliminar todos los registros de la tabla SDEM que en el campo C_RES sean iguales a 2 o Ausente Definitivo
unique(t217.sdem.trc$c_res)
t217.sdem.trc <- subset(t217.sdem.trc, c_res != 2)

# 3. Eliminar todos los registros de la tabla SDEM que en el campo EDA contengan los cógidos de 00 a 11 a 99
unique(t217.sdem.trc$eda)
t217.sdem.trc <- t217.sdem.trc[!(t217.sdem.trc$eda %in%
                               c("00","01","02","03",
                                 "04","05","06","07",
                                 "08","09","10","11","99")),]

# 4. Registros finales de la tabla SDEMT
length(t217.sdem.trc)

# 5. Elminar todos los registros de la tabla COE1 y COE2 que en el campo R_DEF sean diferentes de 00.
unique(t217.coe1.trc$r_def)
t217.coe1.trc <- t217.coe1.trc[!(t217.coe1.trc$r_def == "00")]

# 6. Unir las tres tablas con base a la clave principal
t217.trc1 <- merge(t217.sdem.trc, t217.coe1.trc) # Tabla SDEM con COE1
t217.trc2 <- merge(t217.sdem.trc, t217.coe2.trc) # Tabla SDEM con COE2, no es necesario hacerla
t217.trc <- merge(t217.trc1, t217.coe2.trc)      # Tabla SDEM con COE1 y COE2

rm(t217.trc1)
rm(t217.trc2)

