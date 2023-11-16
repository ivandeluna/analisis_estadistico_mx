library(foreign)
library(tidyr)
library(readr)
library(ggplot2)
library(descr)

# Cargar los archivos csv

mar2019 <- read_delim("asg-2019-03-31.csv", 
                      "|", escape_double = FALSE, 
                      trim_ws = TRUE)

mar2019 <- read_delim("asg-2019-03-31.csv", 
                      "|", escape_double = FALSE, 
                      col_types = cols(sector_economico_1 = col_factor(levels = c("0","1", "2", "3", "4", "5", "6", "7","8", "9")),
                                       sector_economico_2 = col_factor(levels = c("0","1", "2", "3", "4", "5", "6", "7","8", "9")), 
                                       sector_economico_4 = col_factor(levels = c("0","1", "2", "3", "4", "5", "6", "7","8", "9", 
                                                                                  "10", "11", "12", "13","14", "15", "16", "17")),
                                       sexo = col_factor(levels = c("1","2")),
                                       tam_patron = col_factor(levels = c("S1","S2","S3","S4","S5","S6","S7")),
                                       rango_edad = col_factor(levels = c("E1","E2","E3","E4","E5","E6","E7","E8","E9","E10",
                                                                          "E11","E12","E13","E14")),
                                       rango_salarial = col_factor(levels = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10",
                                                                              "W11","W12","W13","W14","W15","W16","W17","W18","W19","W20",
                                                                              "W21","W22","W23","W24","W25")),
                                       rango_uma = col_factor(levels = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10",
                                                                         "W11","W12","W13","W14","W15","W16","W17","W18","W19","W20",
                                                                         "W21","W22","W23","W24","W25"))
                      ),
                      trim_ws = TRUE)

ene2019 <- read_delim("31-ene-2019.csv", 
                      "|", escape_double = FALSE, 
                      col_types = cols(sector_economico_1 = col_factor(levels = c("0","1", "2", "3", "4", "5", "6", "7","8", "9")),
                                       sector_economico_2 = col_factor(levels = c("0","1", "2", "3", "4", "5", "6", "7","8", "9")), 
                                       sector_economico_4 = col_factor(levels = c("0","1", "2", "3", "4", "5", "6", "7","8", "9", 
                                                                                  "10", "11", "12", "13","14", "15", "16", "17")),
                                       sexo = col_factor(levels = c("1","2")),
                                       tam_patron = col_factor(levels = c("S1","S2","S3","S4","S5","S6","S7")),
                                       rango_edad = col_factor(levels = c("E1","E2","E3","E4","E5","E6","E7","E8","E9","E10",
                                                                          "E11","E12","E13","E14")),
                                       rango_salarial = col_factor(levels = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10",
                                                                              "W11","W12","W13","W14","W15","W16","W17","W18","W19","W20",
                                                                              "W21","W22","W23","W24","W25")),
                                       rango_uma = col_factor(levels = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10",
                                                                         "W11","W12","W13","W14","W15","W16","W17","W18","W19","W20",
                                                                         "W21","W22","W23","W24","W25"))
                      ),
                      trim_ws = TRUE)

dic2018 <- read_delim("31-diciembre-2018.csv", 
                      "|", escape_double = FALSE, 
                      col_types = cols(sector_economico_1 = col_factor(levels = c("0","1", "2", "3", "4", "5", "6", "7","8", "9")),
                                       sector_economico_2 = col_factor(levels = c("0","1", "2", "3", "4", "5", "6", "7","8", "9")), 
                                       sector_economico_4 = col_factor(levels = c("0","1", "2", "3", "4", "5", "6", "7","8", "9", 
                                                                                  "10", "11", "12", "13","14", "15", "16", "17")),
                                       sexo = col_factor(levels = c("1","2")),
                                       tam_patron = col_factor(levels = c("S1","S2","S3","S4","S5","S6","S7")),
                                       rango_edad = col_factor(levels = c("E1","E2","E3","E4","E5","E6","E7","E8","E9","E10",
                                                                          "E11","E12","E13","E14")),
                                       rango_salarial = col_factor(levels = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10",
                                                                              "W11","W12","W13","W14","W15","W16","W17","W18","W19","W20",
                                                                              "W21","W22","W23","W24","W25")),
                                       rango_uma = col_factor(levels = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10",
                                                                              "W11","W12","W13","W14","W15","W16","W17","W18","W19","W20",
                                                                              "W21","W22","W23","W24","W25"))
                                       ),
                      trim_ws = TRUE)

dic2017 <- read_delim("31-diciembre-17_2.csv", 
                      "|", escape_double = FALSE, 
                      col_types = cols(sector_economico_1 = col_factor(levels = c("0","1", "2", "3", "4", "5", "6", "7","8", "9")),
                                       sector_economico_2 = col_factor(levels = c("0","1", "2", "3", "4", "5", "6", "7","8", "9")), 
                                       sector_economico_4 = col_factor(levels = c("0","1", "2", "3", "4", "5", "6", "7","8", "9", 
                                                                                  "10", "11", "12", "13","14", "15", "16", "17")),
                                       sexo = col_factor(levels = c("1","2")),
                                       tam_patron = col_factor(levels = c("S1","S2","S3","S4","S5","S6","S7")),
                                       rango_edad = col_factor(levels = c("E1","E2","E3","E4","E5","E6","E7","E8","E9","E10",
                                                                          "E11","E12","E13","E14")),
                                       rango_salarial = col_factor(levels = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10",
                                                                              "W11","W12","W13","W14","W15","W16","W17","W18","W19","W20",
                                                                              "W21","W22","W23","W24","W25")),
                                       rango_uma = col_factor(levels = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10",
                                                                         "W11","W12","W13","W14","W15","W16","W17","W18","W19","W20",
                                                                         "W21","W22","W23","W24","W25"))
                      ),
                      trim_ws = TRUE)

dic2016 <- read_delim("31-dic-16_.csv", 
                      "|", escape_double = FALSE, 
                      col_types = cols(sector_economico_1 = col_factor(levels = c("0","1", "2", "3", "4", "5", "6", "7","8", "9")),
                                       sector_economico_2 = col_factor(levels = c("0","1", "2", "3", "4", "5", "6", "7","8", "9")), 
                                       sector_economico_4 = col_factor(levels = c("0","1", "2", "3", "4", "5", "6", "7","8", "9", 
                                                                                  "10", "11", "12", "13","14", "15", "16", "17")),
                                       sexo = col_factor(levels = c("1","2")),
                                       tam_patron = col_factor(levels = c("S1","S2","S3","S4","S5","S6","S7")),
                                       rango_edad = col_factor(levels = c("E1","E2","E3","E4","E5","E6","E7","E8","E9","E10",
                                                                          "E11","E12","E13","E14")),
                                       rango_salarial = col_factor(levels = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10",
                                                                              "W11","W12","W13","W14","W15","W16","W17","W18","W19","W20",
                                                                              "W21","W22","W23","W24","W25")),
                                       rango_uma = col_factor(levels = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10",
                                                                         "W11","W12","W13","W14","W15","W16","W17","W18","W19","W20",
                                                                         "W21","W22","W23","W24","W25"))
                      ),
                      trim_ws = TRUE)

dic2015 <- read_delim("31-dic-15_.csv", 
                      "|", escape_double = FALSE, 
                      col_types = cols(sector_economico_1 = col_factor(levels = c("0","1", "2", "3", "4", "5", "6", "7","8", "9")),
                                       sector_economico_2 = col_factor(levels = c("0","1", "2", "3", "4", "5", "6", "7","8", "9")), 
                                       sector_economico_4 = col_factor(levels = c("0","1", "2", "3", "4", "5", "6", "7","8", "9", 
                                                                                  "10", "11", "12", "13","14", "15", "16", "17")),
                                       sexo = col_factor(levels = c("1","2")),
                                       tam_patron = col_factor(levels = c("S1","S2","S3","S4","S5","S6","S7")),
                                       rango_edad = col_factor(levels = c("E1","E2","E3","E4","E5","E6","E7","E8","E9","E10",
                                                                          "E11","E12","E13","E14")),
                                       rango_salarial = col_factor(levels = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10",
                                                                              "W11","W12","W13","W14","W15","W16","W17","W18","W19","W20",
                                                                              "W21","W22","W23","W24","W25")),
                                       rango_uma = col_factor(levels = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10",
                                                                         "W11","W12","W13","W14","W15","W16","W17","W18","W19","W20",
                                                                         "W21","W22","W23","W24","W25"))
                      ),
                      trim_ws = TRUE)

dic2014 <- read_delim("31-dic-14_.csv", 
                      "|", escape_double = FALSE, 
                      col_types = cols(sector_economico_1 = col_factor(levels = c("0","1", "2", "3", "4", "5", "6", "7","8", "9")),
                                       sector_economico_2 = col_factor(levels = c("0","1", "2", "3", "4", "5", "6", "7","8", "9")), 
                                       sector_economico_4 = col_factor(levels = c("0","1", "2", "3", "4", "5", "6", "7","8", "9", 
                                                                                  "10", "11", "12", "13","14", "15", "16", "17")),
                                       sexo = col_factor(levels = c("1","2")),
                                       tam_patron = col_factor(levels = c("S1","S2","S3","S4","S5","S6","S7")),
                                       rango_edad = col_factor(levels = c("E1","E2","E3","E4","E5","E6","E7","E8","E9","E10",
                                                                          "E11","E12","E13","E14")),
                                       rango_salarial = col_factor(levels = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10",
                                                                              "W11","W12","W13","W14","W15","W16","W17","W18","W19","W20",
                                                                              "W21","W22","W23","W24","W25")),
                                       rango_uma = col_factor(levels = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10",
                                                                         "W11","W12","W13","W14","W15","W16","W17","W18","W19","W20",
                                                                         "W21","W22","W23","W24","W25"))
                      ),
                      trim_ws = TRUE)

dic2013 <- read_delim("30-nov-13_.csv", 
                      "|", escape_double = FALSE, 
                      col_types = cols(sector_economico_1 = col_factor(levels = c("0","1", "2", "3", "4", "5", "6", "7","8", "9")),
                                       sector_economico_2 = col_factor(levels = c("0","1", "2", "3", "4", "5", "6", "7","8", "9")), 
                                       sector_economico_4 = col_factor(levels = c("0","1", "2", "3", "4", "5", "6", "7","8", "9", 
                                                                                  "10", "11", "12", "13","14", "15", "16", "17")),
                                       sexo = col_factor(levels = c("1","2")),
                                       tam_patron = col_factor(levels = c("S1","S2","S3","S4","S5","S6","S7")),
                                       rango_edad = col_factor(levels = c("E1","E2","E3","E4","E5","E6","E7","E8","E9","E10",
                                                                          "E11","E12","E13","E14")),
                                       rango_salarial = col_factor(levels = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10",
                                                                              "W11","W12","W13","W14","W15","W16","W17","W18","W19","W20",
                                                                              "W21","W22","W23","W24","W25")),
                                       rango_uma = col_factor(levels = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10",
                                                                         "W11","W12","W13","W14","W15","W16","W17","W18","W19","W20",
                                                                         "W21","W22","W23","W24","W25"))
                      ),
                      trim_ws = TRUE)

dic2012 <- read_delim("31-dic-12_.csv", 
                      "|", escape_double = FALSE, 
                      col_types = cols(sector_economico_1 = col_factor(levels = c("0","1", "2", "3", "4", "5", "6", "7","8", "9")),
                                       sector_economico_2 = col_factor(levels = c("0","1", "2", "3", "4", "5", "6", "7","8", "9")), 
                                       sector_economico_4 = col_factor(levels = c("0","1", "2", "3", "4", "5", "6", "7","8", "9", 
                                                                                  "10", "11", "12", "13","14", "15", "16", "17")),
                                       sexo = col_factor(levels = c("1","2")),
                                       tam_patron = col_factor(levels = c("S1","S2","S3","S4","S5","S6","S7")),
                                       rango_edad = col_factor(levels = c("E1","E2","E3","E4","E5","E6","E7","E8","E9","E10",
                                                                          "E11","E12","E13","E14")),
                                       rango_salarial = col_factor(levels = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10",
                                                                              "W11","W12","W13","W14","W15","W16","W17","W18","W19","W20",
                                                                              "W21","W22","W23","W24","W25")),
                                       rango_uma = col_factor(levels = c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W10",
                                                                         "W11","W12","W13","W14","W15","W16","W17","W18","W19","W20",
                                                                         "W21","W22","W23","W24","W25"))
                      ),
                      trim_ws = TRUE)

# Variable de salario mínimo
uma.diario.2018 <- 80.60
uma.mensual.2018 <- 2450.24

# Substraer de la base de datos los datos para la ciudad

# Coahuila

coa.mar2019 <- subset(mar2019, cve_entidad == 5)

# Durango
dgo.mar2019 <- subset(mar2019, cve_entidad == 10)

sum(dgo.mar2019$ta)

sum(mar2019$ta)

# Torreón
trc.ene2019 <- subset(ene2019, cve_municipio == "A40")
trc.dic2018 <- subset(dic2018, dic2018$cve_municipio == "A40")
trc.dic2017 <- subset(dic2017, dic2017$cve_municipio == "A40")
trc.dic2016 <- subset(dic2016, dic2016$cve_municipio == "A40")
trc.dic2015 <- subset(dic2015, dic2015$cve_municipio == "A40")
trc.dic2014 <- subset(dic2014, dic2014$cve_municipio == "A40")
trc.dic2013 <- subset(dic2013, dic2013$cve_municipio == "A40")
trc.dic2012 <- subset(dic2012, dic2012$cve_municipio == "A40")


trc.dic2018 <- subset(dic2018, dic2018$cve_municipio == "A40")
trc.mar2019 <- subset(mar2019, cve_municipio == "A40")
sum(trc.mar2019$ta)
sum(trc.mar2019$asegurados)
sum(trc.mar2019$no_trabajadores)

# Gómez Palacio
gp.dic2018 <- subset(dic2018, dic2018$cve_municipio == "B28")
gp.mar2019 <- subset(mar2019, cve_municipio == "B28")
sum(gp.mar2019$ta)
sum(gp.mar2019$asegurados)

# Matamoros
mat.dic2018 <- subset(dic2018, dic2018$cve_municipio == "A26")
mat.mar2019 <- subset(mar2019, cve_municipio == "A26")
sum(mat.mar2019$ta)
# Lerdo
lrd.dic2018 <- subset(dic2018, dic2018$cve_municipio == "B31")
lrd.mar2019 <- subset(mar2019, cve_municipio == "B31")
sum(lrd.mar2019$ta)
# Saltillo
stl.dic2018 <- subset(dic2018, cve_municipio == "A36")
stl.mar2019 <- subset(mar2019, cve_municipio == "A36")
sum(stl.mar2019$ta)

# Resumen del data frame
summary(trc.dic2018)


# Graficar edades
plot(trc.dic2018$rango_edad)
# Tabla de frecuencia
freq(trc.dic2018$rango_edad, weights )

# Frecuencia
freq(trc.dic2018$rango_uma)

# Sumar la columna x
sum(trc.dic2018$asegurados)
sum(trc.dic2018$no_trabajadores)

# Sumar columna de trabajadores asegurados
sum(mar2019$ta) # Trabajadores asegurados Nacional
sum(coa.mar2019$ta)  # Trabajadores asegurados Coahuila



sum(trc.dic2018$ta)

sum(mat.dic2018$ta)
sum(gp.dic2018$ta)
sum(lrd.dic2018$ta)

sum(trc.ene2019$asegurados)
sum(ene2019$ta)

names(trc.dic2018)


freq(trc.dic2018$sector_economico_1)

crosstab(trc.dic2018$ta, trc.dic2018$sector_economico_1,
         prop.r = TRUE)

bw <- 2 * IQR(trc.dic2018$ta) / length(trc.dic2018$ta) ^ (1/3)

p1 <- ggplot(trc.dic2018, aes(sector_economico_1, ta, color = rango_salarial))+
  geom_jitter() +
  labs(title = "Rango salarial según sector económico. \n Diciembre 2018",
          x = "Sector económico",
          y = "Cantidad de trabajadores asegurados",
          caption = "Datos Abiertos IMSS") + 
  scale_fill_discrete(name = "Rango Salarial")
p1

p2 <- ggplot(trc.dic2017, aes(sector_economico_1, ta, color = rango_salarial))+
  geom_jitter() +
  labs(title = "Rango salarial según sector económico. \n Diciembre 2017",
       x = "Sector económico",
       y = "Cantidad de trabajadores asegurados",
       caption = "Datos Abiertos IMSS") + 
  scale_fill_discrete(name = "Rango Salarial")
p2

p3 <- ggplot(trc.dic2016, aes(sector_economico_1, ta, color = rango_salarial))+
  geom_jitter() +
  labs(title = "Rango salarial según sector económico. \n Diciembre 2016",
       x = "Sector económico",
       y = "Cantidad de trabajadores asegurados",
       caption = "Datos Abiertos IMSS") + 
  scale_fill_discrete(name = "Rango Salarial")
p3

p4 <- ggplot(trc.dic2016, aes(rango_edad, masa_sal_tpu, color = sector_economico_1))+
  geom_jitter() +
  labs(title = "Rango salarial según sector económico. \n Diciembre 2016",
       x = "Rango de edad",
       y = "Masa salarial asociada",
       caption = "Datos Abiertos IMSS") + 
  scale_fill_discrete(name = "Rango Salarial")
p4

freq(trc.dic2012$rango_salarial)
freq(trc.dic2013$rango_salarial)
freq(trc.dic2014$rango_salarial)
freq(trc.dic2015$rango_salarial)
freq(trc.dic2016$rango_salarial)
freq(trc.dic2017$rango_salarial)
freq(trc.dic2018$rango_salarial)

freq(trc.dic2012$rango_edad)
freq(trc.dic2013$rango_edad)
freq(trc.dic2014$rango_edad)
freq(trc.dic2015$rango_edad)
freq(trc.dic2016$rango_edad)
freq(trc.dic2017$rango_edad)
freq(trc.dic2018$rango_edad)

freq(trc.dic2012$sector_economico_1)
freq(trc.dic2013$sector_economico_1)
freq(trc.dic2014$sector_economico_1)
freq(trc.dic2015$sector_economico_1)
freq(trc.dic2016$sector_economico_1)
freq(trc.dic2017$sector_economico_1)
freq(trc.dic2018$sector_economico_1)
freq(trc.mar2019$sector_economico_1)

hist(trc.dic2012$tpu)
freq(trc.dic2013$tpu)
freq(trc.dic2014$tpu)
freq(trc.dic2015$tpu)
freq(trc.dic20162$tpu)
freq(trc.dic2017$tpu)
freq(trc.dic2018$tpu)
freq(trc.mar2019$tpu)


density(trc.dic2013$ta)


freq(trc.dic2016$ta)
freq(trc.dic2017$ta)
freq(trc.dic2018$ta)

freq(trc.dic2012$masa_sal_ta)

crosstab(trc.mar2019$sector_economico_1, trc.mar2019$ta)
