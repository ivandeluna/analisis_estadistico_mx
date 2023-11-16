# este codigo calcula la distribucion de ingresos por decil para Torreon
# tambien calcularemos el coeficiente de GINI
#los datos son extraidos de la base concentrada de la ENIGH 2016 del INEGI
#la base resultante es exportada a un archivo xlsx

## cargamos los paquetes
library(ggplot2)
library(doBy)
library(reldist)
library(writexl)
library(tidyverse)

## cargamos los datos
datos<-read.csv("D:/pavil/Documents/IMPLAN/Datos/ENIGH/2016/concentradohogar_16.csv")

###filtramos para torreon 

datos<-datos%>%mutate(mun =ubica_geo/10000)%>%filter(mun == 5035)

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
tot_hogares_16 <- sum(factor, to.data.frame = TRUE)

### otra opcion sin attach tot_hogares_1 <- sum(Conc$factor, to.data.frame = TRUE)

### se divide la suma de factores entre diez para sacar el tamaño del decil
### se debe de truncar el resultado quitando los decimales.
tam_dec_16<-trunc(tot_hogares_16/10)

### muestra la suma del factor en variable hog.
Conc$tam_dec_16=tam_dec_16

############### CREACION DE DECILES DE INGRESO #######################

### se renombra la tabla concentrado a BD_16.
BD_16 <- Conc

### dentro de la tabla BD_16 se crea la variable MAXT y se le asigna los valores que tienen el ing_cor.
BD_16$MAXT<-BD_16$ing_cor

### se ordena de menor a mayor según la variable MAXT.
BD_16<-BD_16[with(BD_16, order(rank(MAXT))),]

### se aplica la función cumsum, suma acumulada a la variable factor.
BD_16$ACUMULA<-cumsum(BD_16$factor)

### entra a un ciclo donde iremos generando los deciles 1 a 10.
for(i in 1:9)
{
  a1<-BD_16[dim(BD_16[BD_16$ACUMULA<tam_dec_16*i,])[1]+1,]$factor
  BD_16<-rbind(BD_16[1:(dim(BD_16[BD_16$ACUMULA<tam_dec_16*i,])[1]+1),],
             BD_16[(dim(BD_16[BD_16$ACUMULA<tam_dec_16*i,])[1]+1):dim(BD_16[1])[1],])
  b1<-tam_dec_16*i-BD_16[dim(BD_16[BD_16$ACUMULA<tam_dec_16*i,])[1],]$ACUMULA
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
x<-tapply(BD_16$factor,BD_16$Nhog,sum)

## DECILES
y<-tapply(BD_16$factor,BD_16$DECIL,sum)

## se calcula el promedio (ingreso entre los hogares) tanto para el total como para cada uno de los deciles
ing_cormed_t_16<-tapply(BD_16$factor*BD_16$ing_cor,BD_16$Nhog,sum)/x
ing_cormed_d_16<-tapply(BD_16$factor*BD_16$ing_cor,BD_16$DECIL,sum)/y


########################## C U A D R O S #################################

### guardamos los resultados en un data frame
prom_rub <- data.frame (c(ing_cormed_t_16,ing_cormed_d_16))

### agregamos el nombre a las filas
row.names(prom_rub)<-Numdec


############### Cálculo del GINI #############

### GINI Nacional (sobre los 10 deciles) por hogar usando el promedio del ingreso corriente (ingcor)
deciles_hog_ingcor_16 <- data.frame(hogaresxdecil=c(x,x,x,x,x,x,x,x,x,x),
                                 ingreso=c(ing_cormed_d_16[1],ing_cormed_d_16[2],ing_cormed_d_16[3],
                                           ing_cormed_d_16[4],ing_cormed_d_16[5],ing_cormed_d_16[6],
                                           ing_cormed_d_16[7],ing_cormed_d_16[8],ing_cormed_d_16[9],
                                           ing_cormed_d_16[10]))

### se efectua la función Gini y se guarda en nuestro vector a.
gini_16<-gini(deciles_hog_ingcor_16$ingreso,weights=deciles_hog_ingcor_16$hogares)

### se renombran las variables (columnas)
names(prom_rub)=c("INGRESO_CORRIENTE")
names(gini_16)="GINI"

##### Mostramos el resultado en pantalla #####
round(prom_rub)
round(a,3)


prom_rub_plot_16<-as.data.frame(prom_rub[2:11, 1])
prom_rub_plot_16$decil<-c("I", "II", "III","IV", "V", "VI", "VII", "VIII", "IX","X")
names(prom_rub_plot_16)=c("ing_prom_trim", "decil")
ggplot(prom_rub_plot_16, aes(x=reorder(decil, sort(as.roman(decil))), y=ing_prom_trim/3))+geom_bar(stat = "identity", col="black", fill = "grey", alpha = 0.5)+theme_minimal()+labs(x="Decil", y="Ingreso Promedio Mensual")

prom_rub_plot_16<-prom_rub_plot_16%>%mutate(ing_prom_men=ing_prom_trim/3)

write_xlsx(BD_16,"D:/pavil/Documents/IMPLAN/Datos/ENIGH/2016/conc_trc_16.xlsx")
