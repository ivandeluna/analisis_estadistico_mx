#este codigo calcula la distribucion de ingresos per capita por decil para Torreon
#los datos son extraidos de la base concentrada de la ENIGH 2016, 2018 y 2020 del INEGI
#las bases utilizadas fueron extraidas mediante codigos anteriores dispobibles en el drive
#las bases resultantes estan exportadas a archivos xlsx

## cargamos los paquetes
library(ggplot2)
library(doBy)
library(reldist)
library(writexl)
library(tidyverse)

# cargamos los datos
datos_16<-read.csv("D:/pavil/Documents/IMPLAN/Datos/ENIGH/2016/concentradohogar_16.csv")
datos_18<-read.csv("D:/pavil/Documents/IMPLAN/Datos/ENIGH/2018/concentradohogar_18.csv")
datos_20<-read.csv("D:/pavil/Documents/IMPLAN/Datos/ENIGH/2020/concentradohogar_20.csv")

#filtramos datos para Torreon

datos_16<-datos_16%>%filter(ubica_geo == 50350000)
datos_18<-datos_18%>%filter(ubica_geo == 5035)
datos_20<-datos_20%>%filter(ubica_geo == 5035)

#corregimos el nombre de la variable folioviv

names(datos_16)[1]="folioviv"
names(datos_18)[1]="folioviv"
names(datos_20)[1]="folioviv"

#seleccionamos varibales de interes

Conc_16 <- datos_16[c("folioviv", "foliohog", "tot_integ", "ing_cor", "ingtrab", "trabajo", "negocio", "otros_trab", "rentas", "utilidad",
                "arrenda", "transfer", "jubilacion", "becas", "donativos", "remesas", "transf_hog", "trans_inst",
                "estim_alqu", "otros_ing", "alimentos", "vesti_calz", "salud", "transporte", "publico", 
                "adqui_vehi", "combus", "educacion", "personales", "factor")]

Conc_18 <- datos_18[c("folioviv", "foliohog", "tot_integ", "ing_cor", "ingtrab", "trabajo", "negocio", "otros_trab", "rentas", "utilidad",
                   "arrenda", "transfer", "jubilacion", "becas", "donativos", "remesas", "transf_hog", "trans_inst",
                   "estim_alqu", "otros_ing", "alimentos", "vesti_calz", "salud", "transporte", "publico", 
                   "adqui_vehi", "combus", "educacion", "personales", "factor")]

Conc_20 <- datos_20[c("folioviv", "foliohog", "tot_integ", "ing_cor", "ingtrab", "trabajo", "negocio", "otros_trab", "rentas", "utilidad",
                   "arrenda", "transfer", "jubilacion", "becas", "donativos", "remesas", "transf_hog", "trans_inst",
                   "estim_alqu", "otros_ing", "alimentos", "vesti_calz", "salud", "transporte", "publico", 
                   "adqui_vehi", "combus", "educacion", "personales", "factor")]

#calculamos las variables per capita

Conc_16[4:29]<-Conc_16[4:29]/Conc_16$tot_integ
Conc_18[4:29]<-Conc_18[4:29]/Conc_18$tot_integ
Conc_20[4:29]<-Conc_20[4:29]/Conc_20$tot_integ

#definimos el nombre de los deciles

Numdec<-c("Total", "I", "II", "III","IV", "V", "VI", "VII", "VIII", "IX","X")

#se crea una bandera para numerar a los hogares
Conc_16$Nhog <- 1

#ordenamos Concs de acuerdo a ing_cor, folioviv, foliohog.
Conc_16<- orderBy (~+ing_cor+folioviv+foliohog, data=Conc_16)

#suma todos los factores y guarda el valor en el vector tot_hogares.
tot_hogares_16 <- sum(Conc_16$factor, to.data.frame = TRUE)

#se divide la suma de factores entre diez para sacar el tamaño del decil
#se debe de truncar el resultado quitando los decimales.
tam_dec_16<-trunc(tot_hogares_16/10)

#muestra la suma del factor en variable hog.
Conc_16$tam_dec_16=tam_dec_16

############### CREACION DE DECILES DE INGRESO #######################

#se renombra la tabla concentrado a BD
BD_16 <- Conc_16

#dentro de la tabla BD_18 se crea la variable MAXT y se le asigna los valores que tienen el ing_cor.
BD_16$MAXT<-BD_16$ing_cor

#se ordena de menor a mayor según la variable MAXT.
BD_16<-BD_16[with(BD_16, order(rank(MAXT))),]

#se aplica la función cumsum, suma acumulada a la variable factor.
BD_16$ACUMULA<-cumsum(BD_16$factor)

#entra a un ciclo donde iremos generando los deciles 1 a 10.
for(i in 1:9)
{
  a16<-BD_16[dim(BD_16[BD_16$ACUMULA<tam_dec_16*i,])[1]+1,]$factor
  BD_16<-rbind(BD_16[1:(dim(BD_16[BD_16$ACUMULA<tam_dec_16*i,])[1]+1),],
               BD_16[(dim(BD_16[BD_16$ACUMULA<tam_dec_16*i,])[1]+1):dim(BD_16[1])[1],])
  b16<-tam_dec_16*i-BD_16[dim(BD_16[BD_16$ACUMULA<tam_dec_16*i,])[1],]$ACUMULA
  BD_16[(dim(BD_16[BD_16$ACUMULA<tam_dec_16*i,])[1]+1),]$factor<-b1
  BD_16[(dim(BD_16[BD_16$ACUMULA<tam_dec_16*i,])[1]+2),]$factor<-(a1-b1)
}

BD_16$ACUMULA2<-cumsum(BD_16$factor)

BD_16$DECIL<-0

BD_16[(BD_16$ACUMULA2<=tam_dec_16),]$DECIL<-1

for(i in 1:9)
{
  BD_16[((BD_16$ACUMULA2>tam_dec_16*i)&(BD_16$ACUMULA2<=tam_dec_16*(i+1))),]$DECIL<-(i+1)
}

BD_16[BD_16$DECIL%in%"0",]$DECIL<-10

##################################################################

## TOTAL HOGARES
x_16<-tapply(BD_16$factor,BD_16$Nhog,sum)

## DECILES
y_16<-tapply(BD_16$factor,BD_16$DECIL,sum)

## se calcula el promedio (ingreso entre los hogares) tanto para el total como para cada uno de los deciles
ing_cormed_t_16<-tapply(BD_16$factor*BD_16$ing_cor,BD_16$Nhog,sum)/x_16
ing_cormed_d_16<-tapply(BD_16$factor*BD_16$ing_cor,BD_16$DECIL,sum)/y_16


########################## C U A D R O S #################################

### guardamos los resultados en un data frame
prom_rub_16 <- data.frame (c(ing_cormed_t_16,ing_cormed_d_16))

### agregamos el nombre a las filas
row.names(prom_rub_16)<-Numdec


############### Cálculo del GINI #############

### GINI Nacional (sobre los 10 deciles) por hogar usando el promedio del ingreso corriente (ingcor)
deciles_hog_ingcor_16 <- data.frame(hogaresxdecil=c(x_16,x_16,x_16,x_16,x_16,x_16,x_16,x_16,x_16,x_16),
                                    ingreso=c(ing_cormed_d_16[1],ing_cormed_d_16[2],ing_cormed_d_16[3],
                                              ing_cormed_d_16[4],ing_cormed_d_16[5],ing_cormed_d_16[6],
                                              ing_cormed_d_16[7],ing_cormed_d_16[8],ing_cormed_d_16[9],
                                              ing_cormed_d_16[10]))

### se efectua la función Gini y se guarda en nuestro vector a.
gini_16<-gini(deciles_hog_ingcor_16$ingreso,weights=deciles_hog_ingcor_16$hogares)

### se renombran las variables (columnas)
names(prom_rub_16)=c("INGRESO_CORRIENTE")
names(gini_16)="GINI"

##### Mostramos el resultado en pantalla #####
round(prom_rub_16)
round(gini_16,3)


prom_rub_plot_16<-as.data.frame(prom_rub_16[2:11, 1])
prom_rub_plot_16$decil<-c("I", "II", "III","IV", "V", "VI", "VII", "VIII", "IX","X")
names(prom_rub_plot_16)=c("ing_prom_trim", "decil")
ggplot(prom_rub_plot_16, aes(x=reorder(decil, sort(as.roman(decil))), y=ing_prom_trim/3))+geom_bar(stat = "identity", col="black", fill = "grey", alpha = 0.5)+theme_minimal()+labs(x="Decil", y="Ingreso Promedio Mensual")

prom_rub_plot_16<-prom_rub_plot_16%>%mutate(ing_prom_men=ing_prom_trim/3)

#hacemos un xlsx con la base resultante
##write_xlsx(BD_16,"D:/pavil/Documents/IMPLAN/Datos/ENIGH/2016/conc_trc_pc_16.xlsx")


########## HACEMOS LO MISMO PARA 2018 y 2020 ###############

#### 2018 #####
Conc_18$Nhog <- 1

Conc_18<- orderBy (~+ing_cor+folioviv+foliohog, data=Conc_18)

tot_hogares_18 <- sum(Conc_18$factor, to.data.frame = TRUE)

tam_dec_18<-trunc(tot_hogares_18/10)

Conc_18$tam_dec_18=tam_dec_18

BD_18 <- Conc_18

BD_18$MAXT<-BD_18$ing_cor

BD_18<-BD_18[with(BD_18, order(rank(MAXT))),]

BD_18$ACUMULA<-cumsum(BD_18$factor)

for(i in 1:9)
{
  a18<-BD_18[dim(BD_18[BD_18$ACUMULA<tam_dec_18*i,])[1]+1,]$factor
  BD_18<-rbind(BD_18[1:(dim(BD_18[BD_18$ACUMULA<tam_dec_18*i,])[1]+1),],
               BD_18[(dim(BD_18[BD_18$ACUMULA<tam_dec_18*i,])[1]+1):dim(BD_18[1])[1],])
  b18<-tam_dec_18*i-BD_18[dim(BD_18[BD_18$ACUMULA<tam_dec_18*i,])[1],]$ACUMULA
  BD_18[(dim(BD_18[BD_18$ACUMULA<tam_dec_18*i,])[1]+1),]$factor<-b1
  BD_18[(dim(BD_18[BD_18$ACUMULA<tam_dec_18*i,])[1]+2),]$factor<-(a1-b1)
}

BD_18$ACUMULA2<-cumsum(BD_18$factor)

BD_18$DECIL<-0

BD_18[(BD_18$ACUMULA2<=tam_dec_18),]$DECIL<-1

for(i in 1:9)
{
  BD_18[((BD_18$ACUMULA2>tam_dec_18*i)&(BD_18$ACUMULA2<=tam_dec_18*(i+1))),]$DECIL<-(i+1)
}

BD_18[BD_18$DECIL%in%"0",]$DECIL<-10

x_18<-tapply(BD_18$factor,BD_18$Nhog,sum)

y_18<-tapply(BD_18$factor,BD_18$DECIL,sum)

ing_cormed_t_18<-tapply(BD_18$factor*BD_18$ing_cor,BD_18$Nhog,sum)/x_18
ing_cormed_d_18<-tapply(BD_18$factor*BD_18$ing_cor,BD_18$DECIL,sum)/y_18

prom_rub_18 <- data.frame (c(ing_cormed_t_18,ing_cormed_d_18))

row.names(prom_rub_18)<-Numdec

deciles_hog_ingcor_18 <- data.frame(hogaresxdecil=c(x_18,x_18,x_18,x_18,x_18,x_18,x_18,x_18,x_18,x_18),
                                    ingreso=c(ing_cormed_d_18[1],ing_cormed_d_18[2],ing_cormed_d_18[3],
                                              ing_cormed_d_18[4],ing_cormed_d_18[5],ing_cormed_d_18[6],
                                              ing_cormed_d_18[7],ing_cormed_d_18[8],ing_cormed_d_18[9],
                                              ing_cormed_d_18[10]))

gini_18<-gini(deciles_hog_ingcor_18$ingreso,weights=deciles_hog_ingcor_18$hogares)

names(prom_rub_18)=c("INGRESO_CORRIENTE")
names(gini_18)="GINI"

round(prom_rub_18)
round(gini_18,3)

prom_rub_plot_18<-as.data.frame(prom_rub_18[2:11, 1])
prom_rub_plot_18$decil<-c("I", "II", "III","IV", "V", "VI", "VII", "VIII", "IX","X")
names(prom_rub_plot_18)=c("ing_prom_trim", "decil")
ggplot(prom_rub_plot_18, aes(x=reorder(decil, sort(as.roman(decil))), y=ing_prom_trim/3))+geom_bar(stat = "identity", col="black", fill = "grey", alpha = 0.5)+theme_minimal()+labs(x="Decil", y="Ingreso Promedio Mensual")

prom_rub_plot_18<-prom_rub_plot_18%>%mutate(ing_prom_men=ing_prom_trim/3)

##write_xlsx(BD_18,"D:/pavil/Documents/IMPLAN/Datos/ENIGH/2018/conc_trc_pc_18.xlsx")

#### 2020 #####
Conc_20$Nhog <- 1

Conc_20<- orderBy (~+ing_cor+folioviv+foliohog, data=Conc_20)

tot_hogares_20 <- sum(Conc_20$factor, to.data.frame = TRUE)

tam_dec_20<-trunc(tot_hogares_20/10)

Conc_20$tam_dec_20=tam_dec_20

BD_20 <- Conc_20

BD_20$MAXT<-BD_20$ing_cor

BD_20<-BD_20[with(BD_20, order(rank(MAXT))),]

BD_20$ACUMULA<-cumsum(BD_20$factor)

for(i in 1:9)
{
  a20<-BD_20[dim(BD_20[BD_20$ACUMULA<tam_dec_20*i,])[1]+1,]$factor
  BD_20<-rbind(BD_20[1:(dim(BD_20[BD_20$ACUMULA<tam_dec_20*i,])[1]+1),],
               BD_20[(dim(BD_20[BD_20$ACUMULA<tam_dec_20*i,])[1]+1):dim(BD_20[1])[1],])
  b20<-tam_dec_20*i-BD_20[dim(BD_20[BD_20$ACUMULA<tam_dec_20*i,])[1],]$ACUMULA
  BD_20[(dim(BD_20[BD_20$ACUMULA<tam_dec_20*i,])[1]+1),]$factor<-b1
  BD_20[(dim(BD_20[BD_20$ACUMULA<tam_dec_20*i,])[1]+2),]$factor<-(a1-b1)
}

BD_20$ACUMULA2<-cumsum(BD_20$factor)

BD_20$DECIL<-0

BD_20[(BD_20$ACUMULA2<=tam_dec_20),]$DECIL<-1

for(i in 1:9)
{
  BD_20[((BD_20$ACUMULA2>tam_dec_20*i)&(BD_20$ACUMULA2<=tam_dec_20*(i+1))),]$DECIL<-(i+1)
}

BD_20[BD_20$DECIL%in%"0",]$DECIL<-10

x_20<-tapply(BD_20$factor,BD_20$Nhog,sum)

y_20<-tapply(BD_20$factor,BD_20$DECIL,sum)

ing_cormed_t_20<-tapply(BD_20$factor*BD_20$ing_cor,BD_20$Nhog,sum)/x_20
ing_cormed_d_20<-tapply(BD_20$factor*BD_20$ing_cor,BD_20$DECIL,sum)/y_20

prom_rub_20 <- data.frame (c(ing_cormed_t_20,ing_cormed_d_20))

row.names(prom_rub_20)<-Numdec

deciles_hog_ingcor_20 <- data.frame(hogaresxdecil=c(x_20,x_20,x_20,x_20,x_20,x_20,x_20,x_20,x_20,x_20),
                                    ingreso=c(ing_cormed_d_20[1],ing_cormed_d_20[2],ing_cormed_d_20[3],
                                              ing_cormed_d_20[4],ing_cormed_d_20[5],ing_cormed_d_20[6],
                                              ing_cormed_d_20[7],ing_cormed_d_20[8],ing_cormed_d_20[9],
                                              ing_cormed_d_20[10]))

gini_20<-gini(deciles_hog_ingcor_20$ingreso,weights=deciles_hog_ingcor_20$hogares)

names(prom_rub_20)=c("INGRESO_CORRIENTE")
names(gini_20)="GINI"

round(prom_rub_20)
round(gini_20,3)

prom_rub_plot_20<-as.data.frame(prom_rub_20[2:11, 1])
prom_rub_plot_20$decil<-c("I", "II", "III","IV", "V", "VI", "VII", "VIII", "IX","X")
names(prom_rub_plot_20)=c("ing_prom_trim", "decil")
ggplot(prom_rub_plot_20, aes(x=reorder(decil, sort(as.roman(decil))), y=ing_prom_trim/3))+geom_bar(stat = "identity", col="black", fill = "grey", alpha = 0.5)+theme_minimal()+labs(x="Decil", y="Ingreso Promedio Mensual")

prom_rub_plot_20<-prom_rub_plot_20%>%mutate(ing_prom_men=ing_prom_trim/3)

##write_xlsx(BD_20,"D:/pavil/Documents/IMPLAN/Datos/ENIGH/2020/conc_trc_pc_20.xlsx")

####### AHORA CALCULAMOS LOS GASTOS PROMEDIO ###########

##alimentos

alimentos_tot_16<-tapply(BD_16$factor*BD_16$alimentos,BD_16$Nhog,sum)/x_16
alimentos_dec_16<-tapply(BD_16$factor*BD_16$alimentos,BD_16$DECIL,sum)/y_16
prom_ali_16 <- data.frame (c(alimentos_tot_16,alimentos_dec_16))
row.names(prom_ali_16)<-Numdec
names(prom_ali_16)[1]="gasto_prom_ali_16"

alimentos_tot_18<-tapply(BD_18$factor*BD_18$alimentos,BD_18$Nhog,sum)/x_18
alimentos_dec_18<-tapply(BD_18$factor*BD_18$alimentos,BD_18$DECIL,sum)/y_18
prom_ali_18 <- data.frame (c(alimentos_tot_18,alimentos_dec_18))
row.names(prom_ali_18)<-Numdec
names(prom_ali_18)[1]="gasto_prom_ali_18"

alimentos_tot_20<-tapply(BD_18$factor*BD_20$alimentos,BD_20$Nhog,sum)/x_20
alimentos_dec_20<-tapply(BD_18$factor*BD_20$alimentos,BD_20$DECIL,sum)/y_20
prom_ali_20 <- data.frame (c(alimentos_tot_20,alimentos_dec_20))
row.names(prom_ali_20)<-Numdec
names(prom_ali_20)[1]="gasto_prom_ali_20"

prom_ali<-data.frame(c(prom_ali_16, prom_ali_18, prom_ali_20))
row.names(prom_ali)<-Numdec


##vestido y calzado
vesycalz_tot_16<-tapply(BD_16$factor*BD_16$vesti_calz,BD_16$Nhog,sum)/x_16
vesycalz_dec_16<-tapply(BD_16$factor*BD_16$vesti_calz,BD_16$DECIL,sum)/y_16
prom_vyc_16 <- data.frame (c(vesycalz_tot_16,vesycalz_dec_16))
row.names(prom_vyc_16)<-Numdec
names(prom_vyc_16)[1]="gasto_prom_vyc_16"

vesycalz_tot_18<-tapply(BD_18$factor*BD_18$vesti_calz,BD_18$Nhog,sum)/x_18
vesycalz_dec_18<-tapply(BD_18$factor*BD_18$vesti_calz,BD_18$DECIL,sum)/y_18
prom_vyc_18 <- data.frame (c(vesycalz_tot_18,vesycalz_dec_18))
row.names(prom_vyc_18)<-Numdec
names(prom_vyc_18)[1]="gasto_prom_vyc_18"

vesycalz_tot_20<-tapply(BD_18$factor*BD_20$vesti_calz,BD_20$Nhog,sum)/x_20
vesycalz_dec_20<-tapply(BD_18$factor*BD_20$vesti_calz,BD_20$DECIL,sum)/y_20
prom_vyc_20 <- data.frame (c(vesycalz_tot_20,vesycalz_dec_20))
row.names(prom_vyc_20)<-Numdec
names(prom_vyc_20)[1]="gasto_prom_vyc_20"

prom_vyc<-data.frame(c(prom_vyc_16, prom_vyc_18, prom_vyc_20))
row.names(prom_vyc)<-Numdec

##salud
salud_tot_16<-tapply(BD_16$factor*BD_16$salud,BD_16$Nhog,sum)/x_16
salud_dec_16<-tapply(BD_16$factor*BD_16$salud,BD_16$DECIL,sum)/y_16
prom_salud_16 <- data.frame (c(salud_tot_16,salud_dec_16))
row.names(prom_salud_16)<-Numdec
names(prom_salud_16)[1]="gasto_prom_salud_16"

salud_tot_18<-tapply(BD_18$factor*BD_18$salud,BD_18$Nhog,sum)/x_18
salud_dec_18<-tapply(BD_18$factor*BD_18$salud,BD_18$DECIL,sum)/y_18
prom_salud_18 <- data.frame (c(salud_tot_18,salud_dec_18))
row.names(prom_salud_18)<-Numdec
names(prom_salud_18)[1]="gasto_prom_salud_18"

salud_tot_20<-tapply(BD_18$factor*BD_20$salud,BD_20$Nhog,sum)/x_20
salud_dec_20<-tapply(BD_18$factor*BD_20$salud,BD_20$DECIL,sum)/y_20
prom_salud_20 <- data.frame (c(salud_tot_20,salud_dec_20))
row.names(prom_salud_20)<-Numdec
names(prom_salud_20)[1]="gasto_prom_salud_20"

prom_salud<-data.frame(c(prom_salud_16, prom_salud_18, prom_salud_20))
row.names(prom_salud)<-Numdec

##transporte
transporte_tot_16<-tapply(BD_16$factor*BD_16$transporte,BD_16$Nhog,sum)/x_16
transporte_dec_16<-tapply(BD_16$factor*BD_16$transporte,BD_16$DECIL,sum)/y_16
prom_transporte_16 <- data.frame (c(transporte_tot_16,transporte_dec_16))
row.names(prom_transporte_16)<-Numdec
names(prom_transporte_16)[1]="gasto_prom_transporte_16"

transporte_tot_18<-tapply(BD_18$factor*BD_18$transporte,BD_18$Nhog,sum)/x_18
transporte_dec_18<-tapply(BD_18$factor*BD_18$transporte,BD_18$DECIL,sum)/y_18
prom_transporte_18 <- data.frame (c(transporte_tot_18,transporte_dec_18))
row.names(prom_transporte_18)<-Numdec
names(prom_transporte_18)[1]="gasto_prom_transporte_18"

transporte_tot_20<-tapply(BD_18$factor*BD_20$transporte,BD_20$Nhog,sum)/x_20
transporte_dec_20<-tapply(BD_18$factor*BD_20$transporte,BD_20$DECIL,sum)/y_20
prom_transporte_20 <- data.frame (c(transporte_tot_20,transporte_dec_20))
row.names(prom_transporte_20)<-Numdec
names(prom_transporte_20)[1]="gasto_prom_transporte_20"

prom_transporte<-data.frame(c(prom_transporte_16, prom_transporte_18, prom_transporte_20))
row.names(prom_transporte)<-Numdec

###transporte publico
publico_tot_16<-tapply(BD_16$factor*BD_16$publico,BD_16$Nhog,sum)/x_16
publico_dec_16<-tapply(BD_16$factor*BD_16$publico,BD_16$DECIL,sum)/y_16
prom_publico_16 <- data.frame (c(publico_tot_16,publico_dec_16))
row.names(prom_publico_16)<-Numdec
names(prom_publico_16)[1]="gasto_prom_publico_16"

publico_tot_18<-tapply(BD_18$factor*BD_18$publico,BD_18$Nhog,sum)/x_18
publico_dec_18<-tapply(BD_18$factor*BD_18$publico,BD_18$DECIL,sum)/y_18
prom_publico_18 <- data.frame (c(publico_tot_18,publico_dec_18))
row.names(prom_publico_18)<-Numdec
names(prom_publico_18)[1]="gasto_prom_publico_18"

publico_tot_20<-tapply(BD_18$factor*BD_20$publico,BD_20$Nhog,sum)/x_20
publico_dec_20<-tapply(BD_18$factor*BD_20$publico,BD_20$DECIL,sum)/y_20
prom_publico_20 <- data.frame (c(publico_tot_20,publico_dec_20))
row.names(prom_publico_20)<-Numdec
names(prom_publico_20)[1]="gasto_prom_publico_20"

prom_publico<-data.frame(c(prom_publico_16, prom_publico_18, prom_publico_20))
row.names(prom_publico)<-Numdec

##adquisicion de vehiculos
adqui_vehi_tot_16<-tapply(BD_16$factor*BD_16$adqui_vehi,BD_16$Nhog,sum)/x_16
adqui_vehi_dec_16<-tapply(BD_16$factor*BD_16$adqui_vehi,BD_16$DECIL,sum)/y_16
prom_adqui_vehi_16 <- data.frame (c(adqui_vehi_tot_16,adqui_vehi_dec_16))
row.names(prom_adqui_vehi_16)<-Numdec
names(prom_adqui_vehi_16)[1]="gasto_prom_adqui_vehi_16"

adqui_vehi_tot_18<-tapply(BD_18$factor*BD_18$adqui_vehi,BD_18$Nhog,sum)/x_18
adqui_vehi_dec_18<-tapply(BD_18$factor*BD_18$adqui_vehi,BD_18$DECIL,sum)/y_18
prom_adqui_vehi_18 <- data.frame (c(adqui_vehi_tot_18,adqui_vehi_dec_18))
row.names(prom_adqui_vehi_18)<-Numdec
names(prom_adqui_vehi_18)[1]="gasto_prom_adqui_vehi_18"

adqui_vehi_tot_20<-tapply(BD_18$factor*BD_20$adqui_vehi,BD_20$Nhog,sum)/x_20
adqui_vehi_dec_20<-tapply(BD_18$factor*BD_20$adqui_vehi,BD_20$DECIL,sum)/y_20
prom_adqui_vehi_20 <- data.frame (c(adqui_vehi_tot_20,adqui_vehi_dec_20))
row.names(prom_adqui_vehi_20)<-Numdec
names(prom_adqui_vehi_20)[1]="gasto_prom_adqui_vehi_20"

prom_adqui_vehi<-data.frame(c(prom_adqui_vehi_16, prom_adqui_vehi_18, prom_adqui_vehi_20))
row.names(prom_adqui_vehi)<-Numdec

##combustible
combus_tot_16<-tapply(BD_16$factor*BD_16$combus,BD_16$Nhog,sum)/x_16
combus_dec_16<-tapply(BD_16$factor*BD_16$combus,BD_16$DECIL,sum)/y_16
prom_combus_16 <- data.frame (c(combus_tot_16,combus_dec_16))
row.names(prom_combus_16)<-Numdec
names(prom_combus_16)[1]="gasto_prom_combus_16"

combus_tot_18<-tapply(BD_18$factor*BD_18$combus,BD_18$Nhog,sum)/x_18
combus_dec_18<-tapply(BD_18$factor*BD_18$combus,BD_18$DECIL,sum)/y_18
prom_combus_18 <- data.frame (c(combus_tot_18,combus_dec_18))
row.names(prom_combus_18)<-Numdec
names(prom_combus_18)[1]="gasto_prom_combus_18"

combus_tot_20<-tapply(BD_18$factor*BD_20$combus,BD_20$Nhog,sum)/x_20
combus_dec_20<-tapply(BD_18$factor*BD_20$combus,BD_20$DECIL,sum)/y_20
prom_combus_20 <- data.frame (c(combus_tot_20,combus_dec_20))
row.names(prom_combus_20)<-Numdec
names(prom_combus_20)[1]="gasto_prom_combus_20"

prom_combus<-data.frame(c(prom_combus_16, prom_combus_18, prom_combus_20))
row.names(prom_combus)<-Numdec

##educacion
educacion_tot_16<-tapply(BD_16$factor*BD_16$educacion,BD_16$Nhog,sum)/x_16
educacion_dec_16<-tapply(BD_16$factor*BD_16$educacion,BD_16$DECIL,sum)/y_16
prom_educacion_16 <- data.frame (c(educacion_tot_16,educacion_dec_16))
row.names(prom_educacion_16)<-Numdec
names(prom_educacion_16)[1]="gasto_prom_educacion_16"

educacion_tot_18<-tapply(BD_18$factor*BD_18$educacion,BD_18$Nhog,sum)/x_18
educacion_dec_18<-tapply(BD_18$factor*BD_18$educacion,BD_18$DECIL,sum)/y_18
prom_educacion_18 <- data.frame (c(educacion_tot_18,educacion_dec_18))
row.names(prom_educacion_18)<-Numdec
names(prom_educacion_18)[1]="gasto_prom_educacion_18"

educacion_tot_20<-tapply(BD_18$factor*BD_20$educacion,BD_20$Nhog,sum)/x_20
educacion_dec_20<-tapply(BD_18$factor*BD_20$educacion,BD_20$DECIL,sum)/y_20
prom_educacion_20 <- data.frame (c(educacion_tot_20,educacion_dec_20))
row.names(prom_educacion_20)<-Numdec
names(prom_educacion_20)[1]="gasto_prom_educacion_20"

prom_educacion<-data.frame(c(prom_educacion_16, prom_educacion_18, prom_educacion_20))
row.names(prom_educacion)<-Numdec

##gastos personales
personales_tot_16<-tapply(BD_16$factor*BD_16$personales,BD_16$Nhog,sum)/x_16
personales_dec_16<-tapply(BD_16$factor*BD_16$personales,BD_16$DECIL,sum)/y_16
prom_personales_16 <- data.frame (c(personales_tot_16,personales_dec_16))
row.names(prom_personales_16)<-Numdec
names(prom_personales_16)[1]="gasto_prom_personales_16"

personales_tot_18<-tapply(BD_18$factor*BD_18$personales,BD_18$Nhog,sum)/x_18
personales_dec_18<-tapply(BD_18$factor*BD_18$personales,BD_18$DECIL,sum)/y_18
prom_personales_18 <- data.frame (c(personales_tot_18,personales_dec_18))
row.names(prom_personales_18)<-Numdec
names(prom_personales_18)[1]="gasto_prom_personales_18"

personales_tot_20<-tapply(BD_18$factor*BD_20$personales,BD_20$Nhog,sum)/x_20
personales_dec_20<-tapply(BD_18$factor*BD_20$personales,BD_20$DECIL,sum)/y_20
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
##write_xlsx(gastos_promedio,"D:/pavil/Documents/IMPLAN/Datos/ENIGH/gastos_trc_pc.xlsx")
