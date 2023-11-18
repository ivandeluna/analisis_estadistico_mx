# Análisis de base de datos ENSU

# Se lee archivo dbf
library(foreign)

cb <- read.dbf("ENSU_CB_0618.dbf")
cs <- read.dbf("ENSU_CS_0618.dbf")
viv <- read.dbf("ENSU_VIV_0618.dbf")
