rm(list=ls())

library(readxl)
library(ggplot2)
library(scales)
library(dplyr)
library(stringr)
setwd("~/Documents/OnCampusJob/analisis-hosp-covid-19")
file <- "201102COVID19MEXICO.csv"
countries <- c("Mexico", "US", "UK", "Francia", "España", "Alemania", "Perú", "Taiwan", "Corea del Sur", "ITESM")

d <- read.csv(file, stringsAsFactors = FALSE)
d$POSITIVOS <- ifelse(d$CLASIFICACION_FINAL<=3, 1, 0)
pos <- d$POSITIVOS[d$POSITIVOS==1]
total <- length(pos)
#hosp <- d[d$TIPO_PACIENTE==2 & d$FECHA_DEF!="9999-99-99" & d$CLASIFICACION_FINAL<4,]
hosp <- d[d$TIPO_PACIENTE==2  & d$CLASIFICACION_FINAL<4,]
total_hosp <- length(hosp[,1])

mexico_population <- 129391657
peru_population <- 33126781
us_population <- 331663944
tec_population <- 20630

million <- 1000000

mexico <- total_hosp/mexico_population*million
france <- 252
UK <- 135
Italy <- 218

peru <- 5419/peru_population*million
US <- 48470/us_population*million
ITESM <- 42/tec_population*million
southKorea <- 1877/southKorea_population*million
taiwan <- 39/taiwan_population*million

data <- data.frame(
  Entidad = c("Mexico", "ITESM", "US", "UK", "Francia",  "Italia", "Perú", "Taiwan", "Corea del Sur"),
  Hospitalizacion = c(mexico, ITESM, US, UK, france, Italy, peru, taiwan, southKorea)
)

colnames(data) <- c("Entidad", "Hospitalizacion")

ggplot(data=data, aes(x=Entidad, y=Hospitalizacion, fill=Hospitalizacion)) +
  geom_bar(stat="identity", width=0.7, color="white") +
  geom_text(aes(label = round(Hospitalizacion,0)), vjust = -0.2, size=5) +
  ggtitle("Tasa de hospitalizacion de países por millón") +
  xlab("Entidad/País") + ylab("Tasa de hospitalización ppm") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none") +
  scale_fill_viridis_c()


data2 <- data.frame(
  Entidad = c("México", "ITESM", "US", "UK", "Singapur"),
  Hospitalizacion = c(1.631, 1.745, 1.456, 2.528, 0.466)
)
colnames(data2) <- c("Entidad", "Hospitalizacion")

ggplot(data=data2, aes(x=Entidad, y=Hospitalizacion, fill=Hospitalizacion)) +
  geom_bar(stat="identity", width=0.7, color="white") +
  geom_text(aes(label = round(Hospitalizacion,3)), vjust = -0.2, size=5) +
  ggtitle("Número de hospitalizados Covid19 por mil habitantes (acumulados)") +
  xlab("Entidad/País") + ylab("Número de hospitalizados / población (colaboradores en el caso del ITESM)*1000") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 18)) +
  theme(text = element_text(size=15)) + theme(legend.position="none") +
  scale_fill_viridis_c()
