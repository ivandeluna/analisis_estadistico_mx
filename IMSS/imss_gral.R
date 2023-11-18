library(foreign)
library(ggplot2)
library(reshape2)
library(plyr)
library(Cairo)

ene18 <- read.csv("31-ene-18.csv", header = TRUE)
rm(ene18)


coah <- subset(ene18, cve_delegacion == 5)

trc <- subset(coah, cve_subdelegacion == 9)



names(trc)

unique(trc$cve_subdelegacion)

rango_salarial <- as.factor(trc$rango_salarial)

plot(rango_salarial)

masa_sal_ta <- as.factor(trc$masa_sal_ta)
plot(masa_sal_ta)

ta_sal <- as.factor(trc$ta_sal)
plot(ta_sal)

sbc <- as.numeric(masa_sal_ta)/as.numeric(ta_sal)

plot(sbc)
summary(sbc)

sexo.level  <- c(1,2)
sexo.labels <- c("Hombre", "Mujer")
sexo <- factor(trc$sexo, levels = sexo.level, labels = sexo.labels )
plot(sexo)

library(ggplot2)

p1 <- ggplot(trc, aes(masa_sal_ta,
                      fill = as.factor(sexo)))+
  geom_bar(position = "dodge")
p1


p2 <- ggplot(trc, aes(as.factor(rango_edad), masa_sal_ta,
                      fill = sexo)) +
  geom_boxplot()
p2


p3 <- ggplot(trc, aes(x = as.factor(rango_salarial), 
                            y = as.factor(sector_economico_1)))+
  geom_tile(aes(fill = asegurados), colour = "white")+
  scale_fill_gradient(low = "white", high = "steelblue")
p3

p4 <- ggplot(trc, aes(x = as.factor(rango_salarial), 
                      y = as.factor(sector_economico_1),
                      fill = asegurados))+
  geom_tile()
p4



m2 <- melt(trc, id.vars = c("rango_salarial","sector_economico_1",
                            "asegurados"))
colnames(m2) <- c("rango_salarial", "sector_economico_1","asegurados")

head(m2)
m2$rango_salarial <- factor(m2$rango_salarial)
m2$sector_economico_1 <- factor(m2$sector_economico_1)
m2$asegurados <- as.numeric(m2$asegurados)

naSum <- function(x)
  {
    if(all(is.na(x))) val <- sum(x,na.rm=F)
    if(!all(is.na(x))) val <- sum(x,na.rm=T)
    return(val)
}

m3 <- ddply(m2, c("rango_salarial","sector_economico_1"),
            num_asegurados = round(naSum(asegurados)),
            summarise)
head(m3)

p5 <- ggplot(m3, aes(x = rango_salarial,
                     y = sector_economico_1,
                     fill = num_asegurados))+
  geom_tile()
p5



# Género

genero.lev <- c(1,2)
genero.lab <- c("Hombre", "Mujer")
edad.lev <- c("E1","E2","E3","E4","E5","E6","E7","E8","E9",
              "E10","E13","E11","E12","E14")
edad.lab <- c("< 15 años", "15 a 20", "20 a 25",
              "25 a 30", "30 a 35","35 a 40","40 a 45",
              "45 a 50","50 a 55","55 a 60","60 a 65",
              "65 a 70","70 a 75", "> 75 años")


m.sex.sal <- trc[c("rango_salarial", "rango_edad", "sexo")]
m.sex.sal <- 


g.sexo.ing <- ggplot(trc, aes(x = as.factor(rango_salarial),
                              y = stat_count(as.factor(rango_salarial)),
                              fill = as.factor(sexo)))+
  geom_bar(stat = "identity",
           position = "dodge")
g.sexo.ing
