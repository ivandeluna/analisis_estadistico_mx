#------------------------------------------------------------------------------
#
#    Descomposición del Ingreso (DiNardo, Fortin, Lemieux)
#
#------------------------------------------------------------------------------

library(McSpatial)

# Se usan las mismas variables que en enoe_oaxaca

# Para este caso usaremos la fórmula

# ln(ing_x_hrs) ~ log(edad) + log(anios_esc) + log(hrsocup)
#dfl.data <- t218.data[c("sexMujer","eda","anios_esc","ing_x_hrs","hrsocup")]

dfl.t218 <- subset(t218.trc, clase2 == 1)

dfl.data <- dfl.t218[c("sex", "eda","ing_x_hrs","anios_esc","hrsocup")]
dfl.data$eda <- as.numeric(dfl.data$eda)

dfl.data$sex <- setNames(0:1, c("Hombre", "Mujer"))

dfl <- dfldens(dfl.data$ing_x_hrs, sex ~ eda + anios_esc + hrsocup, window = 0.2,
               yname = "Logaritmo de los ingresos por hora",
               data = dfl.data)
dfl


data(matchdata)
matchdata$year05 <- matchdata$year == 2005

fit <- dfldens(matchdata$lnprice, year05~lnland+lnbldg, window=.2,
               yname = "Log of Sale Price", data=matchdata)

matchdata$age <- matchdata$year - matchdata$yrbuilt
fit <- dfldens(matchdata$lnprice, year05 ~age, window = .2,
               yname = "Log of Sale Price", data=matchdata)



dfl(dfl.data,"ing_x_hrs", "sex", c("eda","anios_esc", "hrsocup"), breps = 100)
