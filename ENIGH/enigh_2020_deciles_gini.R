# este codigo calcula la distribucion de ingresos por decil para Torreon
# tambien calcularemos el coeficiente de GINI
#los datos son extraidos de la base concentrada de la ENIGH 2020 del INEGI
#la base resultante es exportada a un archivo xlsx

## cargamos los paquetes
library(ggplot2)
library(doBy)
library(reldist)
library(writexl)
library(tidyverse)

## cargamos los datos
datos<-read.csv("D:/pavil/Documents/IMPLAN/Datos/ENIGH/2020/concentradohogar_20.csv")

###filtramos para torreon 

datos<-datos%>%filter(ubica_geo == 5035)

###por alguna razon folioviv tiene un nombre extraño 
names(datos)[1]="folioviv"

### seleccionamos variables de interes

Conc <- datos[c("folioviv", "foliohog", "ing_cor", "ingtrab", "trabajo", "negocio", "otros_trab", "rentas", "utilidad",
                "arrenda", "transfer", "jubilacion", "becas", "donativos", "remesas", "transf_hog", "trans_inst",
                "estim_alqu", "otros_ing","factor","upm","est_dis", "alimentos", "vesti_calz", "salud", "transporte", "publico", 
                "adqui_vehi", "combus", "educacion", "personales")]

### se define la columna con el nombre de los deciles
Numdec<-c("Total", "I", "II", "III","IV", "V", "VI", "VII", "VIII", "IX","X")

### se crea una bandera para numerar a los hogares
Conc$Nhog <- 1

### ordena Conc de acuerdo a ing_cor, folioviv, foliohog.
attach(Conc)
Conc<- orderBy (~+ing_cor+folioviv+foliohog, data=Conc)

### suma todos los factores y guarda el valor en el vector tot_hogares.
tot_hogares_20 <- sum(factor, to.data.frame = TRUE)

### otra opcion sin attach tot_hogares_1 <- sum(Conc$factor, to.data.frame = TRUE)

### se divide la suma de factores entre diez para sacar el tamaño del decil
### se debe de truncar el resultado quitando los decimales.
tam_dec_20<-trunc(tot_hogares_20/10)

### muestra la suma del factor en variable hog.
Conc$tam_dec_20=tam_dec_20

############### CREACION DE DECILES DE INGRESO #######################

### se renombra la tabla concentrado a BD_20.
BD_20 <- Conc

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
ing_cormed_t_20<-tapply(BD_20$factor*BD_20$ing_cor,BD_20$Nhog,sum)/x
ing_cormed_d_20<-tapply(BD_20$factor*BD_20$ing_cor,BD_20$DECIL,sum)/y


########################## C U A D R O S #################################

### guardamos los resultados en un data frame
prom_rub <- data.frame (c(ing_cormed_t_20,ing_cormed_d_20))

### agregamos el nombre a las filas
row.names(prom_rub)<-Numdec


############### Cálculo del GINI #############

### GINI Nacional (sobre los 10 deciles) por hogar usando el promedio del ingreso corriente (ingcor)
deciles_hog_ingcor_20 <- data.frame(hogaresxdecil=c(x,x,x,x,x,x,x,x,x,x),
                                    ingreso=c(ing_cormed_d_20[1],ing_cormed_d_20[2],ing_cormed_d_20[3],
                                              ing_cormed_d_20[4],ing_cormed_d_20[5],ing_cormed_d_20[6],
                                              ing_cormed_d_20[7],ing_cormed_d_20[8],ing_cormed_d_20[9],
                                              ing_cormed_d_20[10]))

### se efectua la función Gini y se guarda en nuestro vector a.
gini_20<-gini(deciles_hog_ingcor_20$ingreso,weights=deciles_hog_ingcor_20$hogares)

### se renombran las variables (columnas)
names(prom_rub)=c("INGRESO_CORRIENTE")
names(gini_20)="GINI"

##### Mostramos el resultado en pantalla #####
round(prom_rub)
round(gini_20,3)


prom_rub_plot_20<-as.data.frame(prom_rub[2:11, 1])
prom_rub_plot_20$decil<-c("I", "II", "III","IV", "V", "VI", "VII", "VIII", "IX","X")
names(prom_rub_plot_20)=c("ing_prom_trim", "decil")
ggplot(prom_rub_plot_20, aes(x=reorder(decil, sort(as.roman(decil))), y=ing_prom_trim/3))+geom_bar(stat = "identity", col="black", fill = "grey", alpha = 0.5)+theme_minimal()+labs(x="Decil", y="Ingreso Promedio Mensual")

prom_rub_plot_20<-prom_rub_plot_20%>%mutate(ing_prom_men=ing_prom_trim/3)

write_xlsx(BD_20,"D:/pavil/Documents/IMPLAN/Datos/ENIGH/2020/conc_trc_20.xlsx")
