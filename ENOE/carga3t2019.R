
#--------------------------------------------------------------------------------------------
#
#                  Carga de archivos para la ENOE 3T 2019
#
#--------------------------------------------------------------------------------------------

library(readr)

#--------------------------------------------------------------------------------------------
#
#                  Leer archivos e importarlos a R, crear data frames
#
#--------------------------------------------------------------------------------------------


# Se leen los archivos csv de la ENOE o en su caso en otro formato
t319.coe1 <- read_csv("2019T3/COE1T319.csv.CSV")
t319.coe2 <- read_csv("2019T3/COE2T319.csv.CSV")
t319.hog <- read_csv("2019T3/HOGT319.csv.CSV")
t319.sdem <- read_csv("2019T3/SDEMT319.csv.CSV")
t319.viv <- read_csv("2019T3/VIVT319.csv.CSV")

# Se filtra para contener solo a Torreón, como ciudad autorepresentada

t319.coe1.trc <- subset(t319.coe1, t319.coe1$cd_a == "06")
t319.coe2.trc <- subset(t319.coe2, t319.coe2$cd_a == "06")
t319.hog.trc <- subset(t319.hog, t319.hog$cd_a == 6) # en esta tabla el valor es númerico y no caracter
t319.sdem.trc <- subset(t319.sdem, t319.sdem$cd_a == "06")

t319.sdem.stl <- subset(t319.sdem, t319.sdem$cd_a == "17")

# De acuerdo al documento "Conociendo la base de datos de la ENOE"
# es posible unir tablas siguiendo los siguientes pasos:

# 1. Eliminar todos los registros de SDEM que en el campo R_DEF sean diferente a 00 o Entrevista completa
unique(t319.sdem.trc$r_def)
t319.sdem.trc <- subset(t319.sdem.trc, r_def == "00")

# 2. Eliminar todos los registros de la tabla SDEM que en el campo C_RES sean iguales a 2 o Ausente Definitivo
unique(t319.sdem.trc$c_res)
t319.sdem.trc <- subset(t319.sdem.trc, c_res != 2)

# 3. Eliminar todos los registros de la tabla SDEM que en el campo EDA contengan los códigos de 00 a 11 a 99
unique(t319.sdem.trc$eda)
t319.sdem.trc <- t319.sdem.trc[!(t319.sdem.trc$eda %in%
                                   c("00","01","02","03",
                                     "04","05","06","07",
                                     "08","09","10","11","99")),]

# 4. Registros finales de la tabla SDEMT
length(t319.sdem.trc)

# 5. Eliminar todos los registros de la tabla COE1 y COE2 que en el campo R_DEF sean diferentes de 00.
unique(t319.coe1.trc$r_def)
t319.coe1.trc <- t319.coe1.trc[!(t319.coe1.trc$r_def == "00")]

# 6. Unir las tres tablas con base a la clave principal
t319.trc1 <- merge(t319.sdem.trc, t319.coe1.trc) # Tabla SDEM con COE1
t319.trc2 <- merge(t319.sdem.trc, t319.coe2.trc) # Tabla SDEM con COE2, no es necesario hacerla
t319.trc <- merge(t319.trc1, t319.coe2.trc)      # Tabla SDEM con COE1 y COE2
