#este codigo calcula el gasto en salud de los hogares en Torreon
#los datos son extraidos de la base concentrada de la ENIGH  2018 y 2020 del INEGI

## cargamos los paquetes
library(ggplot2)
library(doBy)
library(reldist)
library(readxl)
library(tidyverse)
library(scales)

## cargamos los datos
datos_18<-read_excel("D:/pavil/Documents/IMPLAN/Datos/enigh_2018_trc.xlsx", sheet = 1)
datos_20<-read_excel("D:/pavil/Documents/IMPLAN/Datos/enigh_2020_trc.xlsx", sheet = 1)

### seleccionamos variables de interes
gasto_salud_18<- datos_18[c("folioviv", "foliohog","ing_cor", "salud", "atenc_ambu", "hospital", "medicinas", "factor","upm","est_dis")]
gasto_salud_20 <- datos_20[c("folioviv", "foliohog","ing_cor", "salud", "atenc_ambu", "hospital", "medicinas", "factor","upm","est_dis")]

###obtendremos el gasto por decil
###primero2018

Numdec<-c("Total", "I", "II", "III","IV", "V", "VI", "VII", "VIII", "IX","X")

### se crea una bandera para numerar a los hogares
gasto_salud_18$Nhog <- 1

### ordena Conc de acuerdo a ing_cor, folioviv, foliohog.
attach(gasto_salud_18)
gasto_salud_18<- orderBy (~ing_cor+folioviv+foliohog, data=gasto_salud_18)

### suma todos los factores y guarda el valor en el vector tot_hogares.
tot_hogares_18 <- sum(factor, to.data.frame = TRUE)

### otra opcion sin attach tot_hogares_1 <- sum(Conc$factor, to.data.frame = TRUE)

### se divide la suma de factores entre diez para sacar el tamaño del decil
### se debe de truncar el resultado quitando los decimales.
tam_dec_18<-trunc(tot_hogares_18/10)

### muestra la suma del factor en variable hog.
gasto_salud_18$tam_dec_18=tam_dec_18

############### CREACION DE DECILES DE INGRESO #######################

### se renombra la tabla concentrado a BD_18.
BD_18 <- gasto_salud_18

### dentro de la tabla BD_18 se crea la variable MAXT y se le asigna los valores que tienen el ing_cor.
BD_18$MAXT<-BD_18$ing_cor

### se ordena de menor a mayor según la variable MAXT.
BD_18<-BD_18[with(BD_18, order(rank(MAXT))),]

### se aplica la función cumsum, suma acumulada a la variable factor.
BD_18$ACUMULA<-cumsum(BD_18$factor)

### entra a un ciclo donde iremos generando los deciles 1 a 10.
for(i in 1:9)
{
  a1<-BD_18[dim(BD_18[BD_18$ACUMULA<tam_dec_18*i,])[1]+1,]$factor
  BD_18<-rbind(BD_18[1:(dim(BD_18[BD_18$ACUMULA<tam_dec_18*i,])[1]+1),],
             BD_18[(dim(BD_18[BD_18$ACUMULA<tam_dec_18*i,])[1]+1):dim(BD_18[1])[1],])
  b1<-tam_dec_18*i-BD_18[dim(BD_18[BD_18$ACUMULA<tam_dec_18*i,])[1],]$ACUMULA
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

##################################################################

## TOTAL HOGARES
x<-tapply(BD_18$factor,BD_18$Nhog,sum)

## DECILES
y<-tapply(BD_18$factor,BD_18$DECIL,sum)

## se calcula el promedio (ingreso entre los hogares) tanto para el total como para cada uno de los deciles
g_salud_med_t<-tapply(BD_18$factor*BD_18$salud,BD_18$Nhog,sum)/x
g_salud_med_d<-tapply(BD_18$factor*BD_18$salud,BD_18$DECIL,sum)/y


########################## C U A D R O S #################################

### guardamos los resultados en un data frame
prom_18 <- data.frame (c(g_salud_med_t,g_salud_med_d))

### agregamos el nombre a las filas
row.names(prom_18)<-Numdec

### se renombran las variables (columnas)
names(prom_18)=c("gasto_salud")


##hacemos lo mismo para 2020

gasto_salud_20$Nhog <- 1

### ordena Conc de acuerdo a ing_cor, folioviv, foliohog.
attach(gasto_salud_20)
gasto_salud_20<- orderBy (~ing_cor+folioviv+foliohog, data=gasto_salud_20)

### suma todos los factores y guarda el valor en el vector tot_hogares.
tot_hogares_20 <- sum(factor, to.data.frame = TRUE)

### otra opcion sin attach tot_hogares_1 <- sum(Conc$factor, to.data.frame = TRUE)

### se divide la suma de factores entre diez para sacar el tamaño del decil
### se debe de truncar el resultado quitando los decimales.
tam_dec_20<-trunc(tot_hogares_20/10)

### muestra la suma del factor en variable hog.
gasto_salud_20$tam_dec_20=tam_dec_20

############### CREACION DE DECILES DE INGRESO #######################

### se renombra la tabla concentrado a BD_20.
BD_20 <- gasto_salud_20

### dentro de la tabla BD_20 se crea la variable MAXT y se le asigna los valores que tienen el ing_cor.
BD_20$MAXT<-BD_20$ing_cor

### se ordena de menor a mayor según la variable MAXT.
BD_20<-BD_20[with(BD_20, order(rank(MAXT))),]

### se aplica la función cumsum, suma acumulada a la variable factor.
BD_20$ACUMULA<-cumsum(BD_20$factor)

### entra a un ciclo donde iremos generando los deciles 1 a 10.
for(i in 1:9)
{
  a1<-BD_20[dim(BD_20[BD_20$ACUMULA<tam_dec_20*i,])[1]+1,]$factor
  BD_20<-rbind(BD_20[1:(dim(BD_20[BD_20$ACUMULA<tam_dec_20*i,])[1]+1),],
               BD_20[(dim(BD_20[BD_20$ACUMULA<tam_dec_20*i,])[1]+1):dim(BD_20[1])[1],])
  b1<-tam_dec_20*i-BD_20[dim(BD_20[BD_20$ACUMULA<tam_dec_20*i,])[1],]$ACUMULA
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

##################################################################

## TOTAL HOGARES
x<-tapply(BD_20$factor,BD_20$Nhog,sum)

## DECILES
y<-tapply(BD_20$factor,BD_20$DECIL,sum)

## se calcula el promedio (ingreso entre los hogares) tanto para el total como para cada uno de los deciles
g_salud_med_t<-tapply(BD_20$factor*BD_20$salud,BD_20$Nhog,sum)/x
g_salud_med_d<-tapply(BD_20$factor*BD_20$salud,BD_20$DECIL,sum)/y


########################## C U A D R O S #################################

### guardamos los resultados en un data frame
prom_20 <- data.frame (c(g_salud_med_t,g_salud_med_d))

### agregamos el nombre a las filas
row.names(prom_20)<-Numdec

### se renombran las variables (columnas)
names(prom_20)=c("gasto_salud")

###pasamos los datos a data frame para graficar en ggplot

prom_plot<-as.data.frame(prom_18[2:11, 1])
prom_plot<-prom_plot%>%mutate(prom_20[2:11, 1])
prom_plot$decil<-c("I", "II", "III","IV", "V", "VI", "VII", "VIII", "IX","X")
names(prom_plot)=c("gasto_salud_trim_18", "gasto_salud_trim_20", "decil")

ggplot(prom_plot, postition = "stack")+geom_bar(stat = "identity", col="red", fill=2018, alpha = 1, aes(x=reorder(decil, sort(as.roman(decil))), y=gasto_salud_trim_18/3), position = "stack")+geom_bar(stat = "identity", col="blue", fill=2020, alpha = 0.5, aes(x=reorder(decil, sort(as.roman(decil))), y=gasto_salud_trim_20/3), position = "stack")+theme_minimal()+labs(x="Decil", y="Gasto en Salud Promedio Mensual")

ggplot(prom_plot)+geom_point(col="red", aes(x=reorder(decil, sort(as.roman(decil))), y=gasto_salud_trim_18/3))+geom_point(col="blue", aes(x=reorder(decil, sort(as.roman(decil))), y=gasto_salud_trim_20/3))+theme_minimal()+labs(x="Decil", y="Gasto en Salud Promedio Mensual")
