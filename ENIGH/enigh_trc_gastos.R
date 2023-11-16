#este codigo calcula la distribucion de gastos por decil para Torreon
#los datos son extraidos de la base concentrada de la ENIGH 2016, 2018 y 2020 del INEGI
#las bases utilizadas fueron extraidas mediante codigos anteriores dispobibles en el drive
#las bases resultantes estan exportadas a archivos xlsx

## cargamos los paquetes
library(ggplot2)
library(doBy)
library(reldist)
library(writexl)
library(readxl)
library(tidyverse)

# cargamos los datos
BD_16<-read_excel("D:/pavil/Documents/IMPLAN/Datos/ENIGH/2016/conc_trc_16.xlsx")
BD_18<-read_excel("D:/pavil/Documents/IMPLAN/Datos/ENIGH/2018/conc_trc_18.xlsx")
BD_20<-read_excel("D:/pavil/Documents/IMPLAN/Datos/ENIGH/2020/conc_trc_20.xlsx")

#calculamos la poblacion total y la poblacion en cada decil para cada año

tot_hog_16<-tapply(BD_16$factor,BD_16$Nhog,sum)
tot_dec_16<-tapply(BD_16$factor,BD_16$DECIL,sum)

tot_hog_18<-tapply(BD_18$factor,BD_18$Nhog,sum)
tot_dec_18<-tapply(BD_18$factor,BD_18$DECIL,sum)

tot_hog_20<-tapply(BD_20$factor,BD_20$Nhog,sum)
tot_dec_20<-tapply(BD_20$factor,BD_20$DECIL,sum)

#se calcula el promedio (gasto entre los hogares) tanto para el total como para cada uno de los 
#deciles para cada año

Numdec<-c("Total", "I", "II", "III","IV", "V", "VI", "VII", "VIII", "IX","X")

##alimentos

alimentos_tot_16<-tapply(BD_16$factor*BD_16$alimentos,BD_16$Nhog,sum)/tot_hog_16
alimentos_dec_16<-tapply(BD_16$factor*BD_16$alimentos,BD_16$DECIL,sum)/tot_dec_16
prom_ali_16 <- data.frame (c(alimentos_tot_16,alimentos_dec_16))
row.names(prom_ali_16)<-Numdec
names(prom_ali_16)[1]="gasto_prom_ali_16"

alimentos_tot_18<-tapply(BD_18$factor*BD_18$alimentos,BD_18$Nhog,sum)/tot_hog_18
alimentos_dec_18<-tapply(BD_18$factor*BD_18$alimentos,BD_18$DECIL,sum)/tot_dec_18
prom_ali_18 <- data.frame (c(alimentos_tot_18,alimentos_dec_18))
row.names(prom_ali_18)<-Numdec
names(prom_ali_18)[1]="gasto_prom_ali_18"

alimentos_tot_20<-tapply(BD_18$factor*BD_20$alimentos,BD_20$Nhog,sum)/tot_hog_20
alimentos_dec_20<-tapply(BD_18$factor*BD_20$alimentos,BD_20$DECIL,sum)/tot_dec_20
prom_ali_20 <- data.frame (c(alimentos_tot_20,alimentos_dec_20))
row.names(prom_ali_20)<-Numdec
names(prom_ali_20)[1]="gasto_prom_ali_20"

prom_ali<-data.frame(c(prom_ali_16, prom_ali_18, prom_ali_20))
row.names(prom_ali)<-Numdec


##vestido y calzado
vesycalz_tot_16<-tapply(BD_16$factor*BD_16$vesti_calz,BD_16$Nhog,sum)/tot_hog_16
vesycalz_dec_16<-tapply(BD_16$factor*BD_16$vesti_calz,BD_16$DECIL,sum)/tot_dec_16
prom_vyc_16 <- data.frame (c(vesycalz_tot_16,vesycalz_dec_16))
row.names(prom_vyc_16)<-Numdec
names(prom_vyc_16)[1]="gasto_prom_vyc_16"

vesycalz_tot_18<-tapply(BD_18$factor*BD_18$vesti_calz,BD_18$Nhog,sum)/tot_hog_18
vesycalz_dec_18<-tapply(BD_18$factor*BD_18$vesti_calz,BD_18$DECIL,sum)/tot_dec_18
prom_vyc_18 <- data.frame (c(vesycalz_tot_18,vesycalz_dec_18))
row.names(prom_vyc_18)<-Numdec
names(prom_vyc_18)[1]="gasto_prom_vyc_18"

vesycalz_tot_20<-tapply(BD_18$factor*BD_20$vesti_calz,BD_20$Nhog,sum)/tot_hog_20
vesycalz_dec_20<-tapply(BD_18$factor*BD_20$vesti_calz,BD_20$DECIL,sum)/tot_dec_20
prom_vyc_20 <- data.frame (c(vesycalz_tot_20,vesycalz_dec_20))
row.names(prom_vyc_20)<-Numdec
names(prom_vyc_20)[1]="gasto_prom_vyc_20"

prom_vyc<-data.frame(c(prom_vyc_16, prom_vyc_18, prom_vyc_20))
row.names(prom_vyc)<-Numdec

##salud
salud_tot_16<-tapply(BD_16$factor*BD_16$salud,BD_16$Nhog,sum)/tot_hog_16
salud_dec_16<-tapply(BD_16$factor*BD_16$salud,BD_16$DECIL,sum)/tot_dec_16
prom_salud_16 <- data.frame (c(salud_tot_16,salud_dec_16))
row.names(prom_salud_16)<-Numdec
names(prom_salud_16)[1]="gasto_prom_salud_16"

salud_tot_18<-tapply(BD_18$factor*BD_18$salud,BD_18$Nhog,sum)/tot_hog_18
salud_dec_18<-tapply(BD_18$factor*BD_18$salud,BD_18$DECIL,sum)/tot_dec_18
prom_salud_18 <- data.frame (c(salud_tot_18,salud_dec_18))
row.names(prom_salud_18)<-Numdec
names(prom_salud_18)[1]="gasto_prom_salud_18"

salud_tot_20<-tapply(BD_18$factor*BD_20$salud,BD_20$Nhog,sum)/tot_hog_20
salud_dec_20<-tapply(BD_18$factor*BD_20$salud,BD_20$DECIL,sum)/tot_dec_20
prom_salud_20 <- data.frame (c(salud_tot_20,salud_dec_20))
row.names(prom_salud_20)<-Numdec
names(prom_salud_20)[1]="gasto_prom_salud_20"

prom_salud<-data.frame(c(prom_salud_16, prom_salud_18, prom_salud_20))
row.names(prom_salud)<-Numdec

##transporte
transporte_tot_16<-tapply(BD_16$factor*BD_16$transporte,BD_16$Nhog,sum)/tot_hog_16
transporte_dec_16<-tapply(BD_16$factor*BD_16$transporte,BD_16$DECIL,sum)/tot_dec_16
prom_transporte_16 <- data.frame (c(transporte_tot_16,transporte_dec_16))
row.names(prom_transporte_16)<-Numdec
names(prom_transporte_16)[1]="gasto_prom_transporte_16"

transporte_tot_18<-tapply(BD_18$factor*BD_18$transporte,BD_18$Nhog,sum)/tot_hog_18
transporte_dec_18<-tapply(BD_18$factor*BD_18$transporte,BD_18$DECIL,sum)/tot_dec_18
prom_transporte_18 <- data.frame (c(transporte_tot_18,transporte_dec_18))
row.names(prom_transporte_18)<-Numdec
names(prom_transporte_18)[1]="gasto_prom_transporte_18"

transporte_tot_20<-tapply(BD_18$factor*BD_20$transporte,BD_20$Nhog,sum)/tot_hog_20
transporte_dec_20<-tapply(BD_18$factor*BD_20$transporte,BD_20$DECIL,sum)/tot_dec_20
prom_transporte_20 <- data.frame (c(transporte_tot_20,transporte_dec_20))
row.names(prom_transporte_20)<-Numdec
names(prom_transporte_20)[1]="gasto_prom_transporte_20"

prom_transporte<-data.frame(c(prom_transporte_16, prom_transporte_18, prom_transporte_20))
row.names(prom_transporte)<-Numdec

###transporte publico
publico_tot_16<-tapply(BD_16$factor*BD_16$publico,BD_16$Nhog,sum)/tot_hog_16
publico_dec_16<-tapply(BD_16$factor*BD_16$publico,BD_16$DECIL,sum)/tot_dec_16
prom_publico_16 <- data.frame (c(publico_tot_16,publico_dec_16))
row.names(prom_publico_16)<-Numdec
names(prom_publico_16)[1]="gasto_prom_publico_16"

publico_tot_18<-tapply(BD_18$factor*BD_18$publico,BD_18$Nhog,sum)/tot_hog_18
publico_dec_18<-tapply(BD_18$factor*BD_18$publico,BD_18$DECIL,sum)/tot_dec_18
prom_publico_18 <- data.frame (c(publico_tot_18,publico_dec_18))
row.names(prom_publico_18)<-Numdec
names(prom_publico_18)[1]="gasto_prom_publico_18"

publico_tot_20<-tapply(BD_18$factor*BD_20$publico,BD_20$Nhog,sum)/tot_hog_20
publico_dec_20<-tapply(BD_18$factor*BD_20$publico,BD_20$DECIL,sum)/tot_dec_20
prom_publico_20 <- data.frame (c(publico_tot_20,publico_dec_20))
row.names(prom_publico_20)<-Numdec
names(prom_publico_20)[1]="gasto_prom_publico_20"

prom_publico<-data.frame(c(prom_publico_16, prom_publico_18, prom_publico_20))
row.names(prom_publico)<-Numdec

##adquisicion de vehiculos
adqui_vehi_tot_16<-tapply(BD_16$factor*BD_16$adqui_vehi,BD_16$Nhog,sum)/tot_hog_16
adqui_vehi_dec_16<-tapply(BD_16$factor*BD_16$adqui_vehi,BD_16$DECIL,sum)/tot_dec_16
prom_adqui_vehi_16 <- data.frame (c(adqui_vehi_tot_16,adqui_vehi_dec_16))
row.names(prom_adqui_vehi_16)<-Numdec
names(prom_adqui_vehi_16)[1]="gasto_prom_adqui_vehi_16"

adqui_vehi_tot_18<-tapply(BD_18$factor*BD_18$adqui_vehi,BD_18$Nhog,sum)/tot_hog_18
adqui_vehi_dec_18<-tapply(BD_18$factor*BD_18$adqui_vehi,BD_18$DECIL,sum)/tot_dec_18
prom_adqui_vehi_18 <- data.frame (c(adqui_vehi_tot_18,adqui_vehi_dec_18))
row.names(prom_adqui_vehi_18)<-Numdec
names(prom_adqui_vehi_18)[1]="gasto_prom_adqui_vehi_18"

adqui_vehi_tot_20<-tapply(BD_18$factor*BD_20$adqui_vehi,BD_20$Nhog,sum)/tot_hog_20
adqui_vehi_dec_20<-tapply(BD_18$factor*BD_20$adqui_vehi,BD_20$DECIL,sum)/tot_dec_20
prom_adqui_vehi_20 <- data.frame (c(adqui_vehi_tot_20,adqui_vehi_dec_20))
row.names(prom_adqui_vehi_20)<-Numdec
names(prom_adqui_vehi_20)[1]="gasto_prom_adqui_vehi_20"

prom_adqui_vehi<-data.frame(c(prom_adqui_vehi_16, prom_adqui_vehi_18, prom_adqui_vehi_20))
row.names(prom_adqui_vehi)<-Numdec

##combustible
combus_tot_16<-tapply(BD_16$factor*BD_16$combus,BD_16$Nhog,sum)/tot_hog_16
combus_dec_16<-tapply(BD_16$factor*BD_16$combus,BD_16$DECIL,sum)/tot_dec_16
prom_combus_16 <- data.frame (c(combus_tot_16,combus_dec_16))
row.names(prom_combus_16)<-Numdec
names(prom_combus_16)[1]="gasto_prom_combus_16"

combus_tot_18<-tapply(BD_18$factor*BD_18$combus,BD_18$Nhog,sum)/tot_hog_18
combus_dec_18<-tapply(BD_18$factor*BD_18$combus,BD_18$DECIL,sum)/tot_dec_18
prom_combus_18 <- data.frame (c(combus_tot_18,combus_dec_18))
row.names(prom_combus_18)<-Numdec
names(prom_combus_18)[1]="gasto_prom_combus_18"

combus_tot_20<-tapply(BD_18$factor*BD_20$combus,BD_20$Nhog,sum)/tot_hog_20
combus_dec_20<-tapply(BD_18$factor*BD_20$combus,BD_20$DECIL,sum)/tot_dec_20
prom_combus_20 <- data.frame (c(combus_tot_20,combus_dec_20))
row.names(prom_combus_20)<-Numdec
names(prom_combus_20)[1]="gasto_prom_combus_20"

prom_combus<-data.frame(c(prom_combus_16, prom_combus_18, prom_combus_20))
row.names(prom_combus)<-Numdec

##educacion
educacion_tot_16<-tapply(BD_16$factor*BD_16$educacion,BD_16$Nhog,sum)/tot_hog_16
educacion_dec_16<-tapply(BD_16$factor*BD_16$educacion,BD_16$DECIL,sum)/tot_dec_16
prom_educacion_16 <- data.frame (c(educacion_tot_16,educacion_dec_16))
row.names(prom_educacion_16)<-Numdec
names(prom_educacion_16)[1]="gasto_prom_educacion_16"

educacion_tot_18<-tapply(BD_18$factor*BD_18$educacion,BD_18$Nhog,sum)/tot_hog_18
educacion_dec_18<-tapply(BD_18$factor*BD_18$educacion,BD_18$DECIL,sum)/tot_dec_18
prom_educacion_18 <- data.frame (c(educacion_tot_18,educacion_dec_18))
row.names(prom_educacion_18)<-Numdec
names(prom_educacion_18)[1]="gasto_prom_educacion_18"

educacion_tot_20<-tapply(BD_18$factor*BD_20$educacion,BD_20$Nhog,sum)/tot_hog_20
educacion_dec_20<-tapply(BD_18$factor*BD_20$educacion,BD_20$DECIL,sum)/tot_dec_20
prom_educacion_20 <- data.frame (c(educacion_tot_20,educacion_dec_20))
row.names(prom_educacion_20)<-Numdec
names(prom_educacion_20)[1]="gasto_prom_educacion_20"

prom_educacion<-data.frame(c(prom_educacion_16, prom_educacion_18, prom_educacion_20))
row.names(prom_educacion)<-Numdec

##gastos personales
personales_tot_16<-tapply(BD_16$factor*BD_16$personales,BD_16$Nhog,sum)/tot_hog_16
personales_dec_16<-tapply(BD_16$factor*BD_16$personales,BD_16$DECIL,sum)/tot_dec_16
prom_personales_16 <- data.frame (c(personales_tot_16,personales_dec_16))
row.names(prom_personales_16)<-Numdec
names(prom_personales_16)[1]="gasto_prom_personales_16"

personales_tot_18<-tapply(BD_18$factor*BD_18$personales,BD_18$Nhog,sum)/tot_hog_18
personales_dec_18<-tapply(BD_18$factor*BD_18$personales,BD_18$DECIL,sum)/tot_dec_18
prom_personales_18 <- data.frame (c(personales_tot_18,personales_dec_18))
row.names(prom_personales_18)<-Numdec
names(prom_personales_18)[1]="gasto_prom_personales_18"

personales_tot_20<-tapply(BD_18$factor*BD_20$personales,BD_20$Nhog,sum)/tot_hog_20
personales_dec_20<-tapply(BD_18$factor*BD_20$personales,BD_20$DECIL,sum)/tot_dec_20
prom_personales_20 <- data.frame (c(personales_tot_20,personales_dec_20))
row.names(prom_personales_20)<-Numdec
names(prom_personales_20)[1]="gasto_prom_personales_20"

prom_personales<-data.frame(c(prom_personales_16, prom_personales_18, prom_personales_20))
row.names(prom_personales)<-Numdec

#agregamos todo en un solo data frame
gastos_promedio<-data.frame(c(prom_ali, prom_vyc, prom_salud, prom_transporte, 
                              prom_publico, prom_adqui_vehi, prom_combus, prom_educacion, 
                              prom_personales))
gastos_promedio<-gastos_promedio%>%mutate(decil=Numdec)

#exportamos el agregado
##write_xlsx(gastos_promedio,"D:/pavil/Documents/IMPLAN/Datos/ENIGH/gastos_trc.xlsx")
