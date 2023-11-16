# Jovenes 3er trim 2019

library(foreign)
library(dplyr)
library(tidyr)
library(questionr)
library(descr)
library(dummies)

sdem.319 <- read.csv("sdemt319.csv")


nam <- names(sdem.319)
names(sdem.319) <- tolower(nam)

zml319 <- subset(sdem.319, cd_a == 6)

head(zml319)

# Eliminar los resultados que no sean entrevista completa
zml319 <- subset(zml319, r_def == 0)

# Eliminar los resultados que sean ausente definitivo
zml319 <- subset(zml319, c_res != 2)

# Eliminar los registros que tengan edad de 0 a 14 y 99
zml319 <- zml319[!(zml319$eda %in% c(0,1,2,3,4,5,6,7,8,9,10,11, 12, 13, 14,99)),]

# Comprobar que la edad mínima sea de 15 años

min(zml319$eda)

# Clasificar por condición de ocupación
# 1 Población económicamente activa
# 2 Población no económicamente activa
zml319.pea <- subset(zml319, clase1 == 1)
zml319.pnea <- subset(zml319, clase1 == 2)

# Crear df con personas de edad entre 15 y 29 años
zml319.pea.j <- subset(zml319, eda7c %in% c(1,2))
zml319.pnea.j <- subset(zml319, eda7c %in% c(1,2))

# Tabla de población ocupada y desocupada según edad
wtd.table(zml319.pea$clase2, zml319.pea$eda7c, weights = zml.pea$fac)

# Tabla de población clasificada en ocupación plena, sin pago, etc.
wtd.table(zml.pea$clase3, zml.pea$eda7c, weights = zml.pea$fac)

# Tabla de población clasificadad por edad e ingreso
wtd.table(zml319.pea$ing7c, zml319.pea$eda7c, weights = zml319.pea$fac)

# Tabla de población joven por condicion de ocupación y nivel de instrucción
wtd.table(zml.pea.j$clase2, zml.pea.j$niv_ins, weights = zml.pea.j$fac)




############################################
## Adultos Mayores
# Tabla de población clasificadad por edad e ingreso
wtd.table(zml.pea$ing7c, zml.pea$eda7c, weights = zml.pea$fac)
