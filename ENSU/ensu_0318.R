library(foreign)
library(ggplot2)

viv <- read.dbf("ENSU_VIV_0318.dbf")
cb <- read.dbf("ENSU_CB_0318.dbf")
cs <- read.dbf("ENSU_CS_0318.dbf")

viv.lag <- subset(viv, viv$NOM_MUN == "Torreon")
cb.lag <- subset(cb, cb$NOM_MUN == "Torreon")
cs.lag <- subset(cs, cs$NOM_MUN == "Torreon")

summary(viv.lag)

# 1. Tipo de vivienda
# AP1_1 clase de vivienda partícular
ap1_1.lev <- c(1,2,3,4,5)
ap1_1.lab <- c("Casa Independiente",
               "Departamento",
               "Vivienda en vecindad",
               "Vivienda en cuarto de azotea",
               "Local no construido para habitación")
ap1_1 <- factor(viv.lag$AP1_1, levels = ap1_1.lev,
                labels = ap1_1.lab)

summary(ap1_1)

# AP1_2 Barrera física de la vivienda
ap1_2.lev <- c(1,2)
ap1_2.lab <- c("Si", "No")

ap1_2 <- factor(viv.lag$AP1_2, levels = ap1_2.lev,
                labels = ap1_2.lab)

summary(ap1_2)
# 2. Hogares y residentes en la vivienda
# AP2_1 Número de personas
summary(viv.lag$AP2_1)

# AP2_2 Comparten mismo gasto para comer
summary(viv.lag$AP2_2)
# CS


# CB

# BP1_1 En términos de delincuencia, ¿considera que vivir actualmente en su ciudad, es?
bp1_1.lev <- c(1,2,9)
bp1_1.lab <- c("Seguro",
               "Inseguro",
               "No sabe/ No responde")
bp1_1 <- factor(cb.lag$BP1_1, levels = bp1_1.lev,
                labels = bp1_1.lab)
summary(bp1_1)
# BP1_2 En términos de delincuencia, digame si en La Laguna se siente seguro o inseguro
bp1_2.lev <- c(1,2,3,9)
bp1_2.lab <- c("Seguro",
               "Inseguro",
               "No aplica",
               "No sabe / no responde")

# BP1_2_01 Su casa
bp1_2_01 <- factor(cb.lag$BP1_2_01, levels = bp1_2.lev,
                labels = bp1_2.lab)
summary(bp1_2_01)

# BP1_2_02 Su trabajo
bp1_2_02 <- factor(cb.lag$BP1_2_02, levels = bp1_2.lev,
                   labels = bp1_2.lab)
summary(bp1_2_02)

# BP1_2_03 Calles que habitualmente usa
bp1_2_03 <- factor(cb.lag$BP1_2_03, levels = bp1_2.lev,
                   labels = bp1_2.lab)
summary(bp1_2_03)

# BP1_2_04 La escuela
bp1_2_04 <- factor(cb.lag$BP1_2_04, levels = bp1_2.lev,
                   labels = bp1_2.lab)
summary(bp1_2_04)

# BP1_2_05 El mercado
bp1_2_05 <- factor(cb.lag$BP1_2_05, levels = bp1_2.lev,
                   labels = bp1_2.lab)
summary(bp1_2_05)

# BP1_2_06 El centro comercial
bp1_2_06 <- factor(cb.lag$BP1_2_06, levels = bp1_2.lev,
                   labels = bp1_2.lab)
summary(bp1_2_06)

# BP1_2_07 El banco
bp1_2_07 <- factor(cb.lag$BP1_2_07, levels = bp1_2.lev,
                   labels = bp1_2.lab)
summary(bp1_2_07)

# BP1_2_08 El cajera automático localizado en vía publica
bp1_2_08 <- factor(cb.lag$BP1_2_08, levels = bp1_2.lev,
                   labels = bp1_2.lab)
summary(bp1_2_08)

# BP1_2_09 El transporte público
bp1_2_09 <- factor(cb.lag$BP1_2_09, levels = bp1_2.lev,
                   labels = bp1_2.lab)
summary(bp1_2_09)

# BP1_2_10 El automóvil
bp1_2_10 <- factor(cb.lag$BP1_2_10, levels = bp1_2.lev,
                   labels = bp1_2.lab)
summary(bp1_2_10)

# BP1_2_11 La carretera
bp1_2_11 <- factor(cb.lag$BP1_2_11, levels = bp1_2.lev,
                   labels = bp1_2.lab)
summary(bp1_2_11)

# BP1_2_12 El parque recreativo o centro recreativo
bp1_2_12 <- factor(cb.lag$BP1_2_12, levels = bp1_2.lev,
                   labels = bp1_2.lab)
summary(bp1_2_12)


# BP1_3 Pensando en condiciones de delincuencia en La Laguna,
# ¿considera que en los próximos 12 meses?

bp1_3.lev <- c(1,2,3,4,9)
bp1_3.lab <- c("Mejorará",
               "Seguirá igual de bien",
               "Seguirá igual de mal",
               "Empeorará",
               "No sabe / No responde")
bp1_3 <- factor(cb.lag$BP1_3, levels = bp1_3.lev,
                labels = bp1_3.lab)

summary(bp1_3)


# BP1_4 En los últimos tres meses, ¿ha escuchado o visto en los
# alrededores de su vivienda situaciones como?
bp1_4.lev <- c(1,2,9)
bp1_4.lab <- c("Si", "No", "No sabe / No responde")

# BP1_4_1 Vandalismo
bp1_4_1 <- factor(cb.lag$BP1_4_1, levels = bp1_4.lev,
                  labels = bp1_4.lab)
summary(bp1_4_1)

# BP1_4_2 Consumo de alcohol en las calles
bp1_4_2 <- factor(cb.lag$BP1_4_2, levels = bp1_4.lev,
                  labels = bp1_4.lab)
summary(bp1_4_2)
# BP1_4_3 Robos o asaltos
bp1_4_3 <- factor(cb.lag$BP1_4_3, levels = bp1_4.lev,
                  labels = bp1_4.lab)
summary(bp1_4_3)
# BP1_4_4 Bandas violentas o pandillerismo
bp1_4_4 <- factor(cb.lag$BP1_4_4, levels = bp1_4.lev,
                  labels = bp1_4.lab)
summary(bp1_4_4)
# BP1_4_5 Venta o consumo de droga
bp1_4_5 <- factor(cb.lag$BP1_4_5, levels = bp1_4.lev,
                  labels = bp1_4.lab)
summary(bp1_4_5)
# BP1_4_6 Disparos frecuentes con armas
bp1_4_6 <- factor(cb.lag$BP1_4_6, levels = bp1_4.lev,
                  labels = bp1_4.lab)
summary(bp1_4_6)

# BP1_5 En este mismo periodo de tres meses, por temor a sufrir algún delito
# ¿usted cambió sus hábitos respecto a...
bp1_5.lev <- c(1,2,3,9)
bp1_5.lab <- c("Si","No", "No aplica", "No sabe / No responde")

# BP1_5_1 Llevar cosas de valor
bp1_5_1 <- factor(cb.lag$BP1_5_1, levels = bp1_5.lev,
                  labels = bp1_5.lab)
summary(bp1_5_1)
# BP1_5_2 Caminar por los alrededores de su vivienda, pasada las ocho de la noche?
bp1_5_2 <- factor(cb.lag$BP1_5_2, levels = bp1_5.lev,
                  labels = bp1_5.lab)
summary(bp1_5_2)
# BP1_5_3 Visitar a parientes o amigas
bp1_5_3 <- factor(cb.lag$BP1_5_3, levels = bp1_5.lev,
                  labels = bp1_5.lab)
summary(bp1_5_3)
# BP1_5_4 Permitir que salgan de su vivienda sus hijos menores
bp1_5_4 <- factor(cb.lag$BP1_5_4, levels = bp1_5.lev,
                  labels = bp1_5.lab)
summary(bp1_5_4)
# BP1_5_5 Otro
bp1_5_5 <- factor(cb.lag$BP1_5_5, levels = bp1_5.lev,
                  labels = bp1_5.lab)
summary(bp1_5_5)

# BP1_5_5E Especifique
summary(cb.lag$BP1_5_5E)

# BP1_6 En los últimos tres meses, ¿cuáles han sido las tres principales formas de
# enterarse sobre la situación que guarda la seguridad pública, el narcotráfico
# y//o la delincuencia?

bp1_6.lev <- c(0,1)
bp1_6.lab <- c("No se declaró como opción aformativa",
               "Si")

# BP1_6_1 Platicando con familiares, vecinos o conocidas en los alrededores de su vivienda
bp1_6_1 <- factor(cb.lag$BP1_6_1, levels = bp1_6.lev,
                  labels = bp1_6.lab)
summary(bp1_6_1)

# BP1_6_2 Platicando con compañeros de su trabajo o escuela
bp1_6_2 <- factor(cb.lag$BP1_6_2, levels = bp1_6.lev,
                  labels = bp1_6.lab)
summary(bp1_6_2)
# BP1_6_3 Por Facebook
bp1_6_3 <- factor(cb.lag$BP1_6_3, levels = bp1_6.lev,
                  labels = bp1_6.lab)
summary(bp1_6_3)
# BP1_6_4 Por Twitter
bp1_6_4 <- factor(cb.lag$BP1_6_4, levels = bp1_6.lev,
                  labels = bp1_6.lab)
summary(bp1_6_4)
# BP1_6_5 Por Whatsapp
bp1_6_5 <- factor(cb.lag$BP1_6_5, levels = bp1_6.lev,
                  labels = bp1_6.lab)
summary(bp1_6_5)
# BP1_6_6 Otra aplicación eletrónica
bp1_6_6 <- factor(cb.lag$BP1_6_6, levels = bp1_6.lev,
                  labels = bp1_6.lab)
summary(bp1_6_6)
# BP1_6_7 Por mensajes SMS o alertas de noticias de su teléfono celular
bp1_6_7 <- factor(cb.lag$BP1_6_7, levels = bp1_6.lev,
                  labels = bp1_6.lab)
summary(bp1_6_7)
# BP1_6_8 Viendo noticieros en televisión
bp1_6_8 <- factor(cb.lag$BP1_6_8, levels = bp1_6.lev,
                  labels = bp1_6.lab)
summary(bp1_6_8)
# BP1_6_9 Escuchando noticieros en radio
bp1_6_9 <- factor(cb.lag$BP1_6_9, levels = bp1_6.lev,
                  labels = bp1_6.lab)
summary(bp1_6_9)
# BP1_6_10 Leyendo periódicos o revistas en papel
bp1_6_10 <- factor(cb.lag$BP1_6_10, levels = bp1_6.lev,
                  labels = bp1_6.lab)
summary(bp1_6_10)
# BP1_6_11 En internet (páginas, periódicos, revistas, youtube)
bp1_6_11 <- factor(cb.lag$BP1_6_11, levels = bp1_6.lev,
                  labels = bp1_6.lab)
summary(bp1_6_11)
# BP1_6_12 No veo, escucho ni leo noticias
bp1_6_12 <- factor(cb.lag$BP1_6_12, levels = bp1_6.lev,
                  labels = bp1_6.lab)
summary(bp1_6_12)
# BP1_6_13 Otra
bp1_6_13 <- factor(cb.lag$BP1_6_13, levels = bp1_6.lev,
                  labels = bp1_6.lab)
summary(bp1_6_13)
# BP1_6_13E Especifique
summary(cb.lag$BP1_6_13E)
# BP1_6_99 No sabe / No responde
bp1_6_99 <- factor(cb.lag$BP1_6_99, levels = bp1_6.lev,
                  labels = bp1_6.lab)
summary(bp1_6_99)

# BP1_7 Usted identifica a la
bp1_7.lev <- c(1,2,3, 9)
bp1_7.lab <- c("Si", "No", "No aplica", "No sabe / No responde")

# BP1_7_1 Policía preventiva municipal
bp1_7_1 <- factor(cb.lag$BP1_7_1, levels = bp1_7.lev,
                  labels = bp1_7.lab)
summary(bp1_7_1)

# BP1_7_2 Policía estatal
bp1_7_2 <- factor(cb.lag$BP1_7_2, levels = bp1_7.lev,
                  labels = bp1_7.lab)
summary(bp1_7_2)
# BP1_7_3 Policía federal
bp1_7_3 <- factor(cb.lag$BP1_7_3, levels = bp1_7.lev,
                  labels = bp1_7.lab)
summary(bp1_7_3)
# BP1_7_4 Gendarmería Nacional
bp1_7_4 <- factor(cb.lag$BP1_7_4, levels = bp1_7.lev,
                  labels = bp1_7.lab)
summary(bp1_7_4)
# BP1_7_5 Ejército
bp1_7_5 <- factor(cb.lag$BP1_7_5, levels = bp1_7.lev,
                  labels = bp1_7.lab)
summary(bp1_7_5)
# BP1_7_6 Marina
bp1_7_6 <- factor(cb.lag$BP1_7_6, levels = bp1_7.lev,
                  labels = bp1_7.lab)
summary(bp1_7_6)

# BP1_8 ¿Qué tan efectivo considera el desempeño de la autoridad?
bp1_8.lev <- c(1,2,3,4,9,"b")
bp1_8.lab <- c("Muy efectivo",
               "Algo efectivo",
               "Poco efectivo",
               "Nada efectivo",
               "No sabe / no responde",
               "NA")

# BP1_8_1 Policía preventiva municipal
bp1_8_1 <- factor(cb.lag$BP1_8_1, levels = bp1_8.lev,
                  labels = bp1_8.lab)
summary(bp1_8_1)

# BP1_8_2 Policía estatal
bp1_8_2 <- factor(cb.lag$BP1_8_2, levels = bp1_8.lev,
                  labels = bp1_8.lab)
summary(bp1_8_2)

# BP1_8_3 Policía Federal
bp1_8_3 <- factor(cb.lag$BP1_8_3, levels = bp1_8.lev,
                  labels = bp1_8.lab)
summary(bp1_8_3)

# BP1_8_4 Gendarmería Nacional
bp1_8_4 <- factor(cb.lag$BP1_8_4, levels = bp1_8.lev,
                  labels = bp1_8.lab)
summary(bp1_8_4)

# BP1_8_5 Ejército
bp1_8_5 <- factor(cb.lag$BP1_8_5, levels = bp1_8.lev,
                  labels = bp1_8.lab)
summary(bp1_8_5)

# BP1_8_6 Marina
bp1_8_6 <- factor(cb.lag$BP1_8_6, levels = bp1_8.lev,
                  labels = bp1_8.lab)
summary(bp1_8_6)

# BP1_9 ¿Cuánta confianza le inspira la autoridad?
bp1_9.lev <- c(1,2,3,4,9,"b")
bp1_9.lab <- c("Mucha confianza",
               "Algo de confianza",
               "Algo de desconfianza",
               "Mucha desconfianza",
               "No sabe / No responde",
               "NA")

# BP1_9_1 Policía Preventiva Municipal
bp1_9_1 <- factor(cb.lag$BP1_9_1, levels = bp1_9.lev,
                  labels = bp1_9.lab)
summary(bp1_9_1)


# BP1_9_2 Policía Estatal
bp1_9_2 <- factor(cb.lag$BP1_9_2, levels = bp1_9.lev,
                  labels = bp1_9.lab)
summary(bp1_9_2)
# BP1_9_3 Policía Federal
bp1_9_3 <- factor(cb.lag$BP1_9_3, levels = bp1_9.lev,
                  labels = bp1_9.lab)
summary(bp1_9_3)
# BP1_9_4 Gendarmería Nacional
bp1_9_4 <- factor(cb.lag$BP1_9_4, levels = bp1_9.lev,
                  labels = bp1_9.lab)
summary(bp1_9_4)
# BP1_9_5 Ejército
bp1_9_5 <- factor(cb.lag$BP1_9_5, levels = bp1_9.lev,
                  labels = bp1_9.lab)
summary(bp1_9_5)
# BP1_9_6 Marina
bp1_9_6 <- factor(cb.lag$BP1_9_6, levels = bp1_9.lev,
                  labels = bp1_9.lab)
summary(bp1_9_6)

# CB Parte 2 Conflictos y conductas antisociales

# BP2_1 ¿Usted ha tenido, de manera directa, alguno de los siguientes conflictos o enfrentamientos
# con familiares, vecinos, compañeros de trabajo o escuela, establecimientos comerciales
# o autoridades de gobierno por situaciones que considera lo afecten o molesten?

bp2_1lev <- c(1,2,9)
bp2_1.lab <- c("Si", "No", "No sabe / No aplica")

summary(cb.lag$BP2_1)

# BP2_2 En los últimos tres meses, ¿usted ha tenido, de manera directa, conflictos o enfrentamientos
# con familiares, vecinos, compañeros de trabajo o escuela, establecimientos comerciales
# o autoridades de gobierno por...
bp2_2.lev <- c(0,1,"b")
bp2_2.lab <- c("No se declaró como opción afirmativa",
               "Si",
               "NA")

# BP2_2_01 Ruido por golpes con martillo, uso del taladro, 
#música alta o fiestas
bp2_2_1 <- factor(cb.lag$BP2_2_01, levels = bp2_2.lev,
                  labels = bp2_2.lab)
summary(bp2_2_1)

# BP2_2_02 Peleas u ofensas en el transporte público o con 
#otros automovilistas
bp2_2_2 <- factor(cb.lag$BP2_2_02, levels = bp2_2.lev,
                  labels = bp2_2.lab)
summary(bp2_2_2)


# BP2_2_03 Basura tirada o quemada por vecinos en su jardín, cochera o áreas comunes
bp2_2_3 <- factor(cb.lag$BP2_2_03, levels = bp2_2.lev,
                  labels = bp2_2.lab)
summary(bp2_2_3)

# BP2_2_04 Falta de pago o morosidad en cuotas vecinales o de mantenimiento
bp2_2_4 <- factor(cb.lag$BP2_2_04, levels = bp2_2.lev,
                  labels = bp2_2.lab)
summary(bp2_2_4)

# BP2_2_05 Obstrucción de su cochera, invasión de su cajón de estacionamiento o falta de espacio para estacionarse
bp2_2_5 <- factor(cb.lag$BP2_2_05, levels = bp2_2.lev,
                  labels = bp2_2.lab)
summary(bp2_2_5)

# BP2_2_06 falta de control de los hijos de los vecinos
bp2_2_6 <- factor(cb.lag$BP2_2_06, levels = bp2_2.lev,
                  labels = bp2_2.lab)
summary(bp2_2_6)

# BP2_2_07 disputas familiares, por herencias, divorcios, pensiones alimenticias o custodia de los hijos
bp2_2_7 <- factor(cb.lag$BP2_2_07, levels = bp2_2.lev,
                  labels = bp2_2.lab)
summary(bp2_2_7)

# BP2_2_08 chismes o malos entendidos
bp2_2_8 <- factor(cb.lag$BP2_2_08, levels = bp2_2.lev,
                  labels = bp2_2.lab)
summary(bp2_2_8)

# BP2_2_09 ladridos, ataques o desechos de mascotas
bp2_2_9 <- factor(cb.lag$BP2_2_09, levels = bp2_2.lev,
                  labels = bp2_2.lab)
summary(bp2_2_9)

# BP2_2_10 molestias y hostigamiento por borrachos, drogadictos o pandillas
bp2_2_10 <- factor(cb.lag$BP2_2_010, levels = bp2_2.lev,
                  labels = bp2_2.lab)
summary(bp2_2_10)
# BP2_2_11 ruido excesivo u olores desagradables por parte de algún establecimiento comercial
bp2_2_11 <- factor(cb.lag$BP2_2_11, levels = bp2_2.lev,
                  labels = bp2_2.lab)
summary(bp2_2_11)
# BP2_2_12 conflicto en la compra/consumo de producto/servicio en un estacionamiento comercial
bp2_2_12 <- factor(cb.lag$BP2_2_12, levels = bp2_2.lev,
                  labels = bp2_2.lab)
summary(bp2_2_12)
# BP2_2_13 ambulantaje
bp2_2_13 <- factor(cb.lag$BP2_2_13, levels = bp2_2.lev,
                  labels = bp2_2.lab)
summary(bp2_2_13)
# BP2_2_14 abuso de policias o de agentes de tránsito
bp2_2_14 <- factor(cb.lag$BP2_2_14, levels = bp2_2.lev,
                  labels = bp2_2.lab)
summary(bp2_2_14)
# BP2_2_15 falta de atención en sus trámites o servicios o prepotencia de algún servidor público
bp2_2_15 <- factor(cb.lag$BP2_2_15, levels = bp2_2.lev,
                  labels = bp2_2.lab)
summary(bp2_2_15)
# BP2_2_16 grafiti o pintas a su casa por alguien que usted identifique
bp2_2_16 <- factor(cb.lag$BP2_2_16, levels = bp2_2.lev,
                  labels = bp2_2.lab)
summary(bp2_2_16)
# BP2_2_17 otro
bp2_2_17 <- factor(cb.lag$BP2_2_17, levels = bp2_2.lev,
                  labels = bp2_2.lab)
summary(bp2_2_17)
# BP2_2_17E
summary(cb.lag$BP2_2_17E)
# BP2_2_18
bp2_2_18 <- factor(cb.lag$BP2_2_18, levels = bp2_2.lev,
                  labels = bp2_2.lab)
summary(bp2_2_18)

# BP2_3 Derivado de los incidentes o problemas anteriores,
# ¿con quién ha tenido conflictos directos?
bp2_3.lev <- c(0,1,"b")
bp2_3.lab <- c("No se declaró como opción afirmativa",
               "Si",
               "NA")

# BP2_3_1 Familiares
bp2_3_1 <- factor(cb.lag$BP2_3_1, levels = bp2_3.lev,
                  labels = bp2_3.lab)
summary(bp2_3_1)

# BP2_3_2 Vecinos
bp2_3_2 <- factor(cb.lag$BP2_3_2, levels = bp2_3.lev,
                  labels = bp2_3.lab)
summary(bp2_3_2)
# BP2_3_3 Compañeros de trabajo o escuela
bp2_3_3 <- factor(cb.lag$BP2_3_3, levels = bp2_3.lev,
                  labels = bp2_3.lab)
summary(bp2_3_3)
# BP2_3_4 Desconocidos en la calle
bp2_3_4 <- factor(cb.lag$BP2_3_4, levels = bp2_3.lev,
                  labels = bp2_3.lab)
summary(bp2_3_4)
# BP2_3_5 Autoridades
bp2_3_5 <- factor(cb.lag$BP2_3_5, levels = bp2_3.lev,
                  labels = bp2_3.lab)
summary(bp2_3_5)
# BP2_3_6 Establecimientos
bp2_3_6 <- factor(cb.lag$BP2_3_6, levels = bp2_3.lev,
                  labels = bp2_3.lab)
summary(bp2_3_6)
# BP2_3_7 otro
bp2_3_7 <- factor(cb.lag$BP2_3_7, levels = bp2_3.lev,
                  labels = bp2_3.lab)
summary(bp2_3_7)
# BP2_3_7E Especifique
summary(cb.lag$BP2_3_7E)

# BP2_4 Derivado de lo anterior, ¿qué tipo de consecuencias tubo?
bp2_4.lev <- c(0,1,"b")
bp2_4.lab <- c("No se declaró como opción afirmativa",
               "Si",
               "NA")
# BP2_4_1 Gritos
bp2_4_1 <- factor(cb.lag$BP2_4_1, levels = bp2_4.lev,
                  labels = bp2_4.lab)
summary(bp2_4_1)








