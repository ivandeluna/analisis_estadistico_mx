###############################################################################
# Encuesta Nacional del Ingreso y Gasto en los Hogares 2018
###############################################################################

## Rutina del documento
# https://www.inegi.org.mx/contenidos/programas/enigh/nc/2018/doc/enigh18_descripcion_calculo_r.pdf
##

library(foreign) # leer datos .dbf, .sav, etc
library(survey) # cálculo del diseño muestral
library(doBy) # ordenar datos de la tabla según el ingreso
library(reldist) # función para el cálculo del GINI
options(survey.lonely.psu="adjust") # trata los casos de los estratos con una sola UPM

## 2.1 Ingreso corriente total promedio trimestral por hogar en deciles de hogares y su coeficiente de GINI

# Cuadro 1
# 2018

# limpia la pantalla de tablas o basura de un ejercicio anterior
rm(list = ls())

# abre la tabla concentrado hogar
conc <- read.csv("concentradohogar.csv", as.is = T)

# selecciona las variables de interés
conc <- conc [c("folioviv", "foliohog", "ing_cor", "ingtrab", "trabajo", "negocio",
                "otros_trab", "rentas", "utilidad", "arrenda","transfer", "jubilacion",
                "becas", "donativos", "remesas", "bene_gob", "transf_hog", "trans_inst",
                "estim_alqu", "otros_ing", "factor", "upm", "est_dis")]

# se crea una variable paraa gregar la entidad feredativa
conc$entidad <- substr(conc$folioviv,1,2)

# se define la columna con el nombre de las entidades federativas
numdec <- c("Total", "I", "II", "III", "IV", "V", "VI", "VII","VIII", "IX", "X")

# se crea una bandera para numerar los hogares
conc$nhog <- 1

### Deciles de ingreso ###

# deja activa la tabla conc
attach(conc)

# ordena conc de acuerdo a ing_cor, folioviv, foliohog
conc <- orderBy(~+ing_cor + folioviv + foliohog, data = conc)

# suma todos los factores y guarda el valor en el vector tot_hogares
tot_hogares <- sum(factor,to.data.frame=TRUE)

# se divide la suma de factores entre diez para sacar el tamaño del decil
# se debe de truncar el resultado quitando los decimales
tam_dec <- trunc(tot_hogares/10)

# muestra la suma del factor de variables hog
conc$tam_dec = tam_dec

### Creación de deciles de ingreso ###

# se renomabra la tabla concentrado a BD1
bd1 <- conc

# dentro de la tabla bd1 se crea la variable maxt y se le asigna los valores que tienen el ing_cor
bd1$maxt <- bd1$ing_cor

# se ordena de menor a mayor según la variable maxt
bd1 <- bd1[with(bd1, order(rank(maxt))),]

# se aplica la función cumsum, suma acumulada a la variable factor
bd1$acumula <- cumsum(bd1$factor)

# entra a un ciclo donde iremos generando los deciles 1 a 10
for (i in 1:9) {
  a1 <- bd1[dim(bd1[bd1$acumula < tam_dec*i,])[1] + 1,]$factor
  bd1 <- rbind(bd1[1:(dim(bd1[bd1$acumula<tam_dec*i,])[1]+1),],
    bd1[(dim(bd1[bd1$acumula<tam_dec*i,])[1]+1):dim(bd1[1])[1],])
  b1 <- tam_dec*i - bd1[dim(bd1[bd1$acumula<tam_dec*i,])[1],]$acumula
  bd1[(dim(bd1[bd1$acumula<tam_dec*i,])[1]+1),]$factor <- b1
  bd1[(dim(bd1[bd1$acumula<tam_dec*i,])[1]+2),]$factor <- (a1-b1)
}
bd1$acumula2 <- cumsum(bd1$factor)
bd1$decil <- 0
bd1[(bd1$acumula2<=tam_dec),]$decil <- 1
for (i in 1:9) {
  bd1[((bd1$acumula2>tam_dec*i)&(bd1$acumula2<=tam_dec*(i+1))),]$decil<-(i+1)
}
bd1[bd1$decil%in%"0",]$decil<-10
###############################################
# Total Hogares
x <- tapply(bd1$factor, bd1$nhog, sum)

# Deciles
y <- tapply(bd1$factor, bd1$decil, sum)

# se calcula el promedio (ingreso entre los hogares) tanto para el total como apra cada uno de los deciles
ing_cormed_t <- tapply(bd1$factor*bd1$ing_cor, bd1$nhog, sum)/x
ing_cormed_d <- tapply(bd1$factor*bd1$ing_cor, bd1$decil, sum)/y
#############################################
## Guardamos los resultados en un dataframe
prom_rub <- data.frame(c(ing_cormed_t, ing_cormed_d))

# agregamos el nombre de las filas
row.names(prom_rub) <- numdec

######################## Cálculo del GINI ########################

# Gini nacional (sobre los 10 deciles) por hogar usando el promedio del inrgeso corriente (ingcor)
deciles_hog_incor <- data.frame(hogarexdecil=c(x,x,x,x,x,x,x,x,x,x),
                                ingreso = c(ing_cormed_d[1],ing_cormed_d[2],
                                            ing_cormed_d[3],ing_cormed_d[4],
                                            ing_cormed_d[5],ing_cormed_d[6],
                                            ing_cormed_d[7],ing_cormed_d[8],
                                            ing_cormed_d[9],ing_cormed_d[10]))
# se efectua la funció Gini y se guarda en nuestro vector a
a <- gini(deciles_hog_incor$ingreso, weights = deciles_hog_incor$hogarexdecil)

# se renombran las variables
names(prom_rub) = c("Ingreso corriente")
names(a) = "GINI"

# mostramos el resultado en pantalla
round(prom_rub)
round(a,3)


##### 2.2 Promedio de las principales fuentes de ingreso por entidad federativa

# limpia la pantalla de tablas o basura de un ejercicio anterior
rm(list = ls())

# abre la tabla concentradohogar
conc <- read.csv("concentradohogar.csv", as.is = T)

# se selecciona solo las variables de interes para nuestro cálculo
conc <- conc[c("folioviv", "foliohog", "ing_cor", "ingtrab", "trabajo",
               "negocio", "otros_trab", "rentas", "utilidad", "arrenda",
               "transfer", "jubilacion", "becas", "donativos", "remesas",
               "bene_gob", "transf_hog", "trans_inst", "estim_alqu",
               "otros_ing", "factor", "upm", "est_dis")]

# se crea una variable para agregar la entidad federativa
conc$entidad <- substr(conc$folioviv,1,2)

# se define la columna con el nombre de las entidades federativas
entidades <- c("Estados Unidos Mexicanos", "Aguastaclientes", "Baja California",
               "Baja California Sur", "Campeche", "Coahuila de Zaragoza", "Colima",
               "Chiapas", "Chihuahua", "México", "Durango", "Guanajuato", "Guerrero",
               "Hidalgo", "Jalisco", "Estado de México", "Michoacán de Ocampo", "Morelos",
               "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo",
               "San Luis Potosí", "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala",
               "Veracruz de Ignacion de la Llave", "Yucatán", "Zacatecas")

#### Hogares ####
# se crea una bandera para numerar los hogares
conc$nhog <- 1

# se carga el diseño muestral
mydesign <- svydesign(id = ~upm, strata=~est_dis, data=conc, weights = ~factor)

## Ingreso corriente ##
ming_corTot <- svyratio(~ing_cor, denominator=~nhog, mydesign) # Total promedio
ming_corEnt <- svyby(~ing_cor, denominator= ~nhog, by=~entidad, mydesign, svyratio) # Nacional promedio

### Ingreso del trabajo
mingtrabTot <- svyratio(~ingtrab, denominator=~nhog, mydesign) # Total promedio
mingtrabEnt <- svyby(~ingtrab, denominator= ~nhog, by=~entidad, mydesign, svyratio) # Nacional promedio

### Ingreso del trabajo subordinado
mtrabajoTot <- svyratio(~trabajo, denominator=~nhog, mydesign) # Total promedio
mtrabajoEnt <- svyby(~trabajo, denominator= ~nhog, by=~entidad, mydesign, svyratio) # Nacional promedio

### Ingreso del trabajo independiente
mnegocioTot <- svyratio(~negocio, denominator=~nhog, mydesign) # Total promedio
mnegocioEnt <- svyby(~negocio, denominator= ~nhog, by=~entidad, mydesign, svyratio) # Nacional promedio

### Ingreso de otros trabajos
motros_trabTot <- svyratio(~otros_trab, denominator=~nhog, mydesign) # Total promedio
motros_trabEnt <- svyby(~otros_trab, denominator= ~nhog, by=~entidad, mydesign, svyratio) # Nacional promedio

### Renta de la propiedad
mrentasTot <- svyratio(~rentas, denominator=~nhog, mydesign) # Total promedio
mrentasEnt <- svyby(~rentas, denominator= ~nhog, by=~entidad, mydesign, svyratio) # Nacional promedio

### Ingreso de sociedades
mutilidadTot <- svyratio(~utilidad, denominator=~nhog, mydesign) # Total promedio
mutilidadEnt <- svyby(~utilidad, denominator= ~nhog, by=~entidad, mydesign, svyratio) # Nacional promedio

### Ingreso arrendamiento
marrendaTot <- svyratio(~arrenda, denominator=~nhog, mydesign) # Total promedio
marrentaEnt <- svyby(~arrenda, denominator= ~nhog, by=~entidad, mydesign, svyratio) # Nacional promedio

### Transferencias
mtransferTot <- svyratio(~transfer, denominator=~nhog, mydesign) # Total promedio
mtransferEnt <- svyby(~transfer, denominator= ~nhog, by=~entidad, mydesign, svyratio) # Nacional promedio

### Jubilación
mjubilacionTot <- svyratio(~jubilacion, denominator=~nhog, mydesign) # Total promedio
mjubilacionEnt <- svyby(~jubilacion, denominator= ~nhog, by=~entidad, mydesign, svyratio) # Nacional promedio

### Becas
mbecasTot <- svyratio(~becas, denominator=~nhog, mydesign) # Total promedio
mbecasEnt <- svyby(~becas, denominator= ~nhog, by=~entidad, mydesign, svyratio) # Nacional promedio

### Donativos
mdonativosTot <- svyratio(~donativos, denominator=~nhog, mydesign) # Total promedio
mdonativosEnt <- svyby(~donativos, denominator= ~nhog, by=~entidad, mydesign, svyratio) # Nacional promedio

### remesas
mremesasTot <- svyratio(~remesas, denominator=~nhog, mydesign) # Total promedio
mremesasEnt <- svyby(~remesas, denominator= ~nhog, by=~entidad, mydesign, svyratio) # Nacional promedio

### bene_gob
mbene_gobTot <- svyratio(~bene_gob, denominator=~nhog, mydesign) # Total promedio
mbene_gobEnt <- svyby(~bene_gob, denominator= ~nhog, by=~entidad, mydesign, svyratio) # Nacional promedio

### transf_hog
mtransf_hogTot <- svyratio(~transf_hog, denominator=~nhog, mydesign) # Total promedio
mtransf_hogEnt <- svyby(~transf_hog, denominator= ~nhog, by=~entidad, mydesign, svyratio) # Nacional promedio

### trans_inst
mtrans_instTot <- svyratio(~trans_inst, denominator=~nhog, mydesign) # Total promedio
mtrans_instEnt <- svyby(~trans_inst, denominator= ~nhog, by=~entidad, mydesign, svyratio) # Nacional promedio

### estim_alqu
mestim_alquTot <- svyratio(~estim_alqu, denominator=~nhog, mydesign) # Total promedio
mestim_alquEnt <- svyby(~estim_alqu, denominator= ~nhog, by=~entidad, mydesign, svyratio) # Nacional promedio

### otros_ing
motros_ingTot <- svyratio(~otros_ing, denominator=~nhog, mydesign) # Total promedio
motros_ingEnt <- svyby(~otros_ing, denominator= ~nhog, by=~entidad, mydesign, svyratio) # Nacional promedio


# Estimaciones
es_ming_cortot <- ming_corTot[[1]]
es_ming_corent <- ming_corEnt[[2]]
es_mingtrabtot <- mingtrabTot[[1]]
es_mingtrabent <- mingtrabEnt[[2]]
es_mtrabajotot <- mtrabajoTot[[1]]
es_mtrabajoent <- mtrabajoEnt[[2]]
es_mnegociotot <- mnegocioTot[[1]]
es_mnegocioent <- mnegocioEnt[[2]]
es_motros_trabtot <- motros_trabTot[[1]]
es_motros_trabent <- motros_trabEnt[[2]]
es_mrentastot <- mrentasTot[[1]]
es_mrentasent <- mrentasEnt[[2]]
es_mutilidadtot <- mutilidadTot[[1]]
es_mutilidadent <- mutilidadEnt[[2]]
es_marrendatot <- marrendaTot[[1]]
es_marrendaent <- marrentaEnt[[2]]
es_mtransfertot <- mtransferTot[[1]]
es_mtransferent <- mtransferEnt[[2]]
es_mjubilaciontot <- mjubilacionTot[[1]]
es_mjubilacionent <- mjubilacionEnt[[2]]
es_mbecastot <- mbecasTot[[1]]
es_mbecasent <- mbecasEnt[[2]]
es_mdonativostot <- mdonativosTot[[1]]
es_mdonativosent <- mdonativosEnt[[2]]
es_mremesastot <- mremesasTot[[1]]
es_mremesasent <- mremesasEnt[[2]]
es_mbene_gobtot <- mbene_gobTot[[1]]
es_mbene_gobent <- mbene_gobEnt[[2]]
es_mtransf_hogtot <- mtransf_hogTot[[1]]
es_mtransf_hotent <- mtransf_hogEnt[[2]]
es_mtransf_insttot <- mtrans_instTot[[1]]
es_mtransf_instent <- mtrans_instEnt[[2]]
es_mestim_alqutot <- mestim_alquTot[[1]]
es_mestim_alquent <- mestim_alquEnt[[2]]
es_motros_ingtot <- motros_ingTot[[1]]
es_motros_ingent <- motros_ingEnt[[2]]


### Error Estándar
se_ming_cortot <- SE(ming_corTot)
se_ming_corent <- SE(ming_corEnt)
se_mingtrabtot <- SE(mingtrabTot)
se_mingtrabent <- SE(mingtrabEnt)
se_mtrabajotot <- SE(mtrabajoTot)
se_mtrabajoent <- SE(mtrabajoEnt)
se_mnegociotot <- SE(mnegocioTot)
se_mnegocioent <- SE(mnegocioEnt)
se_motros_trabtot <- SE(motros_trabTot)
se_motros_trabent <- SE(motros_trabEnt)
se_mrentastot <- SE(mrentasTot)
se_mrentasent <- SE(mrentasEnt)
se_mutilidadtot <- SE(mutilidadTot)
se_mutilidadent <- SE(mutilidadEnt)
se_marrendatot <- SE(marrendaTot)
se_marrendaent <- SE(marrentaEnt)
se_mtransfertot <- SE(mtransferTot)
se_mtransferent <- SE(mtransferEnt)
se_mjubilaciontot <- SE(mjubilacionTot)
se_mjubilacionent <- SE(mjubilacionEnt)
se_mbecastot <- SE(mbecasTot)
se_mbecasent <- SE(mbecasEnt)
se_mdonativostot <- SE(mdonativosTot)
se_mdonativosent <- SE(mdonativosEnt)
se_mremesastot <- SE(mremesasTot)
se_mremesasent <- SE(mremesasEnt)
se_mbene_gobtot <- SE(mbene_gobTot)
se_mbene_gobent <- SE(mbene_gobEnt)
se_mtransf_hogtot <- SE(mtransf_hogTot)
se_mtransf_hotent <- SE(mtransf_hogEnt)
se_mtransf_insttot <- SE(mtrans_instTot)
se_mtransf_instent <- SE(mtrans_instEnt)
se_mestim_alqutot <- SE(mestim_alquTot)
se_mestim_alquent <- SE(mestim_alquEnt)
se_motros_ingtot <- SE(motros_ingTot)
se_motros_ingent <- SE(motros_ingEnt)

### Coeficiente de variación
cv_ming_cortot <- cv(ming_corTot)
cv_ming_corent <- cv(ming_corEnt)
cv_mingtrabtot <- cv(mingtrabTot)
cv_mingtrabent <- cv(mingtrabEnt)
cv_mtrabajotot <- cv(mtrabajoTot)
cv_mtrabajoent <- cv(mtrabajoEnt)
cv_mnegociotot <- cv(mnegocioTot)
cv_mnegocioent <- cv(mnegocioEnt)
cv_motros_trabtot <- cv(motros_trabTot)
cv_motros_trabent <- cv(motros_trabEnt)
cv_mrentastot <- cv(mrentasTot)
cv_mrentasent <- cv(mrentasEnt)
cv_mutilidadtot <- cv(mutilidadTot)
cv_mutilidadent <- cv(mutilidadEnt)
cv_marrendatot <- cv(marrendaTot)
cv_marrendaent <- cv(marrentaEnt)
cv_mtransfertot <- cv(mtransferTot)
cv_mtransferent <- cv(mtransferEnt)
cv_mjubilaciontot <- cv(mjubilacionTot)
cv_mjubilacionent <- cv(mjubilacionEnt)
cv_mbecastot <- cv(mbecasTot)
cv_mbecasent <- cv(mbecasEnt)
cv_mdonativostot <- cv(mdonativosTot)
cv_mdonativosent <- cv(mdonativosEnt)
cv_mremesastot <- cv(mremesasTot)
cv_mremesasent <- cv(mremesasEnt)
cv_mbene_gobtot <- cv(mbene_gobTot)
cv_mbene_gobent <- cv(mbene_gobEnt)
cv_mtransf_hogtot <- cv(mtransf_hogTot)
cv_mtransf_hotent <- cv(mtransf_hogEnt)
cv_mtransf_insttot <- cv(mtrans_instTot)
cv_mtransf_instent <- cv(mtrans_instEnt)
cv_mestim_alqutot <- cv(mestim_alquTot)
cv_mestim_alquent <- cv(mestim_alquEnt)
cv_motros_ingtot <- cv(motros_ingTot)
cv_motros_ingent <- cv(motros_ingEnt)

### Límite inferior
li_ming_cortot <- confint(ming_corTot, level=0.90)[,1]
li_ming_corent <- confint(ming_corEnt, level=0.90)[,1]
li_mingtrabtot <- confint(mingtrabTot, level=0.90)[,1]
li_mingtrabent <- confint(mingtrabEnt, level=0.90)[,1]
li_mtrabajotot <- confint(mtrabajoTot, level=0.90)[,1]
li_mtrabajoent <- confint(mtrabajoEnt, level=0.90)[,1]
li_mnegociotot <- confint(mnegocioTot, level=0.90)[,1]
li_mnegocioent <- confint(mnegocioEnt, level=0.90)[,1]
li_motros_trabtot <- confint(motros_trabTot, level=0.90)[,1]
li_motros_trabent <- confint(motros_trabEnt, level=0.90)[,1]
li_mrentastot <- confint(mrentasTot, level=0.90)[,1]
li_mrentasent <- confint(mrentasEnt, level=0.90)[,1]
li_mutilidadtot <- confint(mutilidadTot, level=0.90)[,1]
li_mutilidadent <- confint(mutilidadEnt, level=0.90)[,1]
li_marrendatot <- confint(marrendaTot, level=0.90)[,1]
li_marrendaent <- confint(marrentaEnt, level=0.90)[,1]
li_mtransfertot <- confint(mtransferTot, level=0.90)[,1]
li_mtransferent <- confint(mtransferEnt, level=0.90)[,1]
li_mjubilaciontot <- confint(mjubilacionTot, level=0.90)[,1]
li_mjubilacionent <- confint(mjubilacionEnt, level=0.90)[,1]
li_mbecastot <- confint(mbecasTot, level=0.90)[,1]
li_mbecasent <- confint(mbecasEnt, level=0.90)[,1]
li_mdonativostot <- confint(mdonativosTot, level=0.90)[,1]
li_mdonativosent <- confint(mdonativosEnt, level=0.90)[,1]
li_mremesastot <- confint(mremesasTot, level=0.90)[,1]
li_mremesasent <- confint(mremesasEnt, level=0.90)[,1]
li_mbene_gobtot <- confint(mbene_gobTot, level=0.90)[,1]
li_mbene_gobent <- confint(mbene_gobEnt, level=0.90)[,1]
li_mtransf_hogtot <- confint(mtransf_hogTot, level=0.90)[,1]
li_mtransf_hotent <- confint(mtransf_hogEnt, level=0.90)[,1]
li_mtransf_insttot <- confint(mtrans_instTot, level=0.90)[,1]
li_mtransf_instent <- confint(mtrans_instEnt, level=0.90)[,1]
li_mestim_alqutot <- confint(mestim_alquTot, level=0.90)[,1]
li_mestim_alquent <- confint(mestim_alquEnt, level=0.90)[,1]
li_motros_ingtot <- confint(motros_ingTot, level=0.90)[,1]
li_motros_ingent <- confint(motros_ingEnt, level=0.90)[,1]

### Límite superior
ls_ming_cortot <- confint(ming_corTot, level=0.90)[,2]
ls_ming_corent <- confint(ming_corEnt, level=0.90)[,2]
ls_mingtrabtot <- confint(mingtrabTot, level=0.90)[,2]
ls_mingtrabent <- confint(mingtrabEnt, level=0.90)[,2]
ls_mtrabajotot <- confint(mtrabajoTot, level=0.90)[,2]
ls_mtrabajoent <- confint(mtrabajoEnt, level=0.90)[,2]
ls_mnegociotot <- confint(mnegocioTot, level=0.90)[,2]
ls_mnegocioent <- confint(mnegocioEnt, level=0.90)[,2]
ls_motros_trabtot <- confint(motros_trabTot, level=0.90)[,2]
ls_motros_trabent <- confint(motros_trabEnt, level=0.90)[,2]
ls_mrentastot <- confint(mrentasTot, level=0.90)[,2]
ls_mrentasent <- confint(mrentasEnt, level=0.90)[,2]
ls_mutilidadtot <- confint(mutilidadTot, level=0.90)[,2]
ls_mutilidadent <- confint(mutilidadEnt, level=0.90)[,2]
ls_marrendatot <- confint(marrendaTot, level=0.90)[,2]
ls_marrendaent <- confint(marrentaEnt, level=0.90)[,2]
ls_mtransfertot <- confint(mtransferTot, level=0.90)[,2]
ls_mtransferent <- confint(mtransferEnt, level=0.90)[,2]
ls_mjubilaciontot <- confint(mjubilacionTot, level=0.90)[,2]
ls_mjubilacionent <- confint(mjubilacionEnt, level=0.90)[,2]
ls_mbecastot <- confint(mbecasTot, level=0.90)[,2]
ls_mbecasent <- confint(mbecasEnt, level=0.90)[,2]
ls_mdonativostot <- confint(mdonativosTot, level=0.90)[,2]
ls_mdonativosent <- confint(mdonativosEnt, level=0.90)[,2]
ls_mremesastot <- confint(mremesasTot, level=0.90)[,2]
ls_mremesasent <- confint(mremesasEnt, level=0.90)[,2]
ls_mbene_gobtot <- confint(mbene_gobTot, level=0.90)[,2]
ls_mbene_gobent <- confint(mbene_gobEnt, level=0.90)[,2]
ls_mtransf_hogtot <- confint(mtransf_hogTot, level=0.90)[,2]
ls_mtransf_hotent <- confint(mtransf_hogEnt, level=0.90)[,2]
ls_mtransf_insttot <- confint(mtrans_instTot, level=0.90)[,2]
ls_mtransf_instent <- confint(mtrans_instEnt, level=0.90)[,2]
ls_mestim_alqutot <- confint(mestim_alquTot, level=0.90)[,2]
ls_mestim_alquent <- confint(mestim_alquEnt, level=0.90)[,2]
ls_motros_ingtot <- confint(motros_ingTot, level=0.90)[,2]
ls_motros_ingent <- confint(motros_ingEnt, level=0.90)[,2]

