library(rgdal)
library(colorspace)
library(sp)
library(grDevices)
library(tmap)
library(sf)
library(spData)
library(tidyr)
library(spdplyr)
library(RColorBrewer)
library(wesanderson)
library(spdep)
library(maptools)
library(leaflet)
library(colorRamps)
library(plotly)
library(MCMCpack)
library(mcmcplots)
library(foreign)
library(nlme)
library(readr)
library(spdep)
library(spBayes)
library(CARBayes)
library(dplyr)
library(readxl)

setwd('C:/Users/Meli/Documents/Proyecto_Espacial/10092019CEMEP')

pal<-colorRampPalette(c("#ffffd9","#41b6c4","#081d58"))
col1<-pal(20)

DATOS_DESERCION <- read_excel("~/Proyecto_Espacial/DATOS_DESERCION _COLEGIO_2016.xlsx", col_types = c("text", "text", "text","text", "text", "text", "text", "text","text", "text", "text", "numeric","text", "text", "text", "text", "text","text", "numeric", "numeric", "numeric"))
shapeCan<-readRDS("C:/Users/Meli/Documents/Proyecto_Espacial/CRI_adm2.rds")
shapeDR<-readOGR(dsn="C:/Users/Meli/Documents/Proyecto_Espacial/Mapas MEP",layer="DRE_2016_CRTM05", encoding = "UTF-8", use_iconv = TRUE)
shapeCPri<-readOGR(dsn="C:/Users/Meli/Documents/Proyecto_Espacial/10092019CEMEP",layer="10092019CEPrivados", encoding = "UTF-8", use_iconv = TRUE)
shapeCPub<-readOGR(dsn="C:/Users/Meli/Documents/Proyecto_Espacial/10092019CEMEP",layer="10092019CEPublicos", encoding = "UTF-8", use_iconv = TRUE)

plot(shapeDR,xlim = c(200000, 750000),ylim = c(880000, 1250000))
plot(shapeCPri,xlim = c(200000, 750000),ylim = c(880000, 1250000))
plot(shapeCPub,xlim = c(200000, 750000),ylim = c(880000, 1250000))

## Tasas por cantón

deser.canton<-DATOS_DESERCION  %>%
  group_by(ID_2) %>%
  summarize(deser = mean(TASADER))

shapeCan<-shapeCan[order(shapeCan$ID_2),]

shapeCan$TASADER<-deser.canton$deser

spplot(shapeCan,"TASADER",xlim = c(-86.3, -82.5),ylim = c(8, 11.5),col.regions = col1)

deser.regional<-DATOS_DESERCION  %>%
  group_by(ID) %>%
  summarize(deser = mean(TASADER))

shapeDR<-shapeDR[order(shapeDR$ID),]

shapeDR$TASADER<-deser.regional$deser

spplot(shapeDR,"TASADER",xlim = c(200000, 750000),ylim = c(880000, 1250000),col.regions = col1)

## Crietrios vecinos

list.queen<-poly2nb(shapeCan, queen=TRUE)
Wq<-nb2listw(list.queen, style="S", zero.policy=TRUE)
Wq
plot(shapeCan,xlim = c(-86.3, -82.5),ylim = c(8, 11.5))
plot(Wq,coordinates(shapeCan),col="blue",add=T,xlim = c(-86.3, -82.5),ylim = c(8, 11.5))


#Criterio de la torre
list.torre<-poly2nb(shapeCan, queen=FALSE)
Wt<-nb2listw(list.torre, style="S", zero.policy=TRUE)
Wt
plot(shapeCan,xlim = c(-86.3, -82.5),ylim = c(8, 11.5))
plot(Wt,coordinates(shapeCan),col="green",add=T,xlim = c(-86.3, -82.5),ylim = c(8, 11.5))


# Matriz de peso basada en distancias

coords<-coordinates(shapeCan)
W_dist<-dnearneigh(coords,0,1,longlat = FALSE)
Wd1<-nb2listw(W_dist, style="S", zero.policy=TRUE)
Wd1
plot(shapeCan,xlim = c(-86.3, -82.5),ylim = c(8, 11.5))
plot(Wd1,coordinates(shapeCan),col="orange",add=T)

dist.mat <- as.matrix(dist(coords, method = "euclidean"))
dist.mat.inv <- 1 / dist.mat # 1 / d_{ij}
diag(dist.mat.inv) <- 0 
dist.mat.inve <- mat2listw(dist.mat.inv, style = "W", row.names = shapeCan$NAME_2)
plot(shapeCan,xlim = c(-86.3, -82.5),ylim = c(8, 11.5))
plot(dist.mat.inve, coordinates(shapeCan),col="brown",add=T)

list.queen1<-poly2nb(shapeDR, queen=TRUE)
Wq1<-nb2listw(list.queen1, style="S", zero.policy=TRUE)
Wq1
plot(shapeDR,xlim = c(200000, 750000),ylim = c(880000, 1250000))
plot(Wq1,coordinates(shapeDR),col="blue",add=T,xlim = c(-86.3, -82.5),ylim = c(8, 11.5))

list.torre1<-poly2nb(shapeDR, queen=FALSE)
Wt1<-nb2listw(list.torre1, style="S", zero.policy=TRUE)
Wt1
plot(shapeDR,xlim = c(200000, 750000),ylim = c(880000, 1250000))
plot(Wt1,coordinates(shapeDR),col="green",add=T,xlim = c(-86.3, -82.5),ylim = c(8, 11.5))

coords1<-coordinates(shapeDR)
W_dist1<-dnearneigh(coords1,0,80000,longlat = FALSE)
Wd11<-nb2listw(W_dist1, style="S", zero.policy=TRUE)
Wd11
plot(shapeDR,xlim = c(200000, 750000),ylim = c(880000, 1250000))
plot(Wd11,coordinates(shapeDR),col="orange",add=T,xlim = c(-86.3, -82.5),ylim = c(8, 11.5))


dist.mat1 <- as.matrix(dist(coords1, method = "euclidean"))
dist.mat.inv1 <- 1 / dist.mat1 # 1 / d_{ij}
diag(dist.mat.inv1) <- 0 
dist.mat.inve1 <- mat2listw(dist.mat.inv1, style = "W", row.names = shapeDR$NOMBRE_DRE)
plot(shapeDR,xlim = c(200000, 750000),ylim = c(880000, 1250000))
plot(dist.mat.inve1, coordinates(shapeDR),col="brown",add=T,xlim = c(200000, 750000),ylim = c(880000, 1250000))


p.urb<-DATOS_DESERCION %>%
  group_by(ID_2,ZONA) %>%
  summarise(n = n()) %>%
  mutate(PZONAUR = round(n*100 / sum(n),3)) %>%
  ungroup() %>%
  complete(ID_2,ZONA, fill = list(n = 0, freq = 0))
p.urb[is.na(p.urb)] <- 0
p.urb<-p.urb %>% dplyr ::filter(ZONA=="Urbana")
shapeCan$PZONAUR<-p.urb$PZONAUR

p.noc<-DATOS_DESERCION %>%
  group_by(ID_2,HORA) %>%
  summarise(n = n()) %>%
  mutate(PHORANOC = round(n*100 / sum(n),3)) %>%
  ungroup() %>%
  complete(ID_2,HORA, fill = list(n = 0, freq = 0))
p.noc[is.na(p.noc)] <- 0
p.noc<-p.noc %>% dplyr ::filter(HORA=="Nocturno")
shapeCan$PHORANOC <-p.noc$PHORANOC

p.ram<-DATOS_DESERCION %>%
  group_by(ID_2,RAMA) %>%
  summarise(n = n()) %>%
  mutate(PRAMAAC = round(n*100 / sum(n),3)) %>%
  ungroup() %>%
  complete(ID_2,RAMA, fill = list(n = 0, freq = 0))
p.ram[is.na(p.ram)] <- 0
p.ram<-p.ram %>% dplyr ::filter(RAMA=="Academica")
shapeCan$PRAMAAC<-p.ram$PRAMAAC

p.pub<-DATOS_DESERCION %>%
  group_by(ID_2,SECTOR) %>%
  summarise(n = n()) %>%
  mutate(PPUB = round(n*100 / sum(n),3)) %>%
  ungroup() %>%
  complete(ID_2,SECTOR, fill = list(n = 0, freq = 0))
p.pub[is.na(p.pub)] <- 0
p.pub<-p.pub %>% dplyr ::filter(SECTOR=="Publico")
shapeCan$PPUB<-p.pub$PPUB

p.urb1<-DATOS_DESERCION %>%
  group_by(ID,ZONA) %>%
  summarise(n = n()) %>%
  mutate(PZONAUR = round(n*100 / sum(n),3)) %>%
  ungroup() %>%
  complete(ID,ZONA, fill = list(n = 0, freq = 0))
p.urb1[is.na(p.urb1)] <- 0
p.urb1<-p.urb1 %>% dplyr ::filter(ZONA=="Urbana")
shapeDR$PZONAUR<-p.urb1$PZONAUR

p.noc1<-DATOS_DESERCION %>%
  group_by(ID,HORA) %>%
  summarise(n = n()) %>%
  mutate(PHORANOC = round(n*100 / sum(n),3)) %>%
  ungroup() %>%
  complete(ID,HORA, fill = list(n = 0, freq = 0))
p.noc1[is.na(p.noc1)] <- 0
p.noc1<-p.noc1 %>% dplyr ::filter(HORA=="Nocturno")
shapeDR$PHORANOC <-p.noc1$PHORANOC

p.ram1<-DATOS_DESERCION %>%
  group_by(ID,RAMA) %>%
  summarise(n = n()) %>%
  mutate(PRAMAAC = round(n*100 / sum(n),3)) %>%
  ungroup() %>%
  complete(ID,RAMA, fill = list(n = 0, freq = 0))
p.ram1[is.na(p.ram1)] <- 0
p.ram1<-p.ram1 %>% dplyr ::filter(RAMA=="Academica")
shapeDR$PRAMAAC<-p.ram1$PRAMAAC

p.pub1<-DATOS_DESERCION %>%
  group_by(ID,SECTOR) %>%
  summarise(n = n()) %>%
  mutate(PPUB = round(n*100 / sum(n),3)) %>%
  ungroup() %>%
  complete(ID,SECTOR, fill = list(n = 0, freq = 0))
p.pub1[is.na(p.pub1)] <- 0
p.pub1<-p.pub1 %>% dplyr ::filter(SECTOR=="Publico")
shapeDR$PPUB<-p.pub1$PPUB

par(mfrow=c(1,4))
spplot(shapeDR,"PZONAUR",xlim = c(200000, 750000),ylim = c(880000, 1250000),col.regions = col1)
spplot(shapeDR,"PRAMAAC",xlim = c(200000, 750000),ylim = c(880000, 1250000),col.regions = col1)
spplot(shapeDR,"PHORANOC",xlim = c(200000, 750000),ylim = c(880000, 1250000),col.regions = col1)
spplot(shapeDR,"PPUB",xlim = c(200000, 750000),ylim = c(880000, 1250000),col.regions = col1)



spplot(shapeCan,"PZONAUR",xlim = c(-86.3, -82.5),ylim = c(8, 11.5),col.regions = col1)
spplot(shapeCan,"PRAMAAC",xlim = c(-86.3, -82.5),ylim = c(8, 11.5),col.regions = col1)
spplot(shapeCan,"PHORANOC",xlim = c(-86.3, -82.5),ylim = c(8, 11.5),col.regions = col1)
spplot(shapeCan,"PPUB",xlim = c(-86.3, -82.5),ylim = c(8, 11.5),col.regions = col1)

## I Moran

moran.test(shapeCan$TASADER,dist.mat.inve, alternative="two.sided",zero.policy=TRUE)
moran.test(shapeDR$TASADER,dist.mat.inve1, alternative="two.sided",zero.policy=TRUE)

####### Modelos

mod1<-lm(TASADER~PZONAUR+PRAMAAC+PHORANOC+PPUB, data = shapeCan)
summary(mod1)
moran.lm<-lm.morantest(mod1, dist.mat.inve, alternative="two.sided",zero.policy=TRUE)
print(moran.lm)
moran.plot(residuals(mod1), dist.mat.inve, quiet = TRUE)


mod2<-lm(TASADER~PZONAUR+PRAMAAC+PHORANOC+PPUB, data = shapeDR)
summary(mod2)
moran.lm2<-lm.morantest(mod2,dist.mat.inve1, alternative="two.sided",zero.policy=TRUE)
print(moran.lm2)
moran.plot(residuals(mod2), dist.mat.inve1, quiet = TRUE)

shapeCan$resid.model<-residuals(mod1)
moran.resid<-moran.mc(x=shapeCan$resid.model, listw= dist.mat.inve, nsim=10000)
moran.resid

shapeDR$resid.model2<-residuals(mod2)
moran.resid2<-moran.mc(x=shapeDR$resid.model2, listw= dist.mat.inve1, nsim=10000)
moran.resid2

impacts(sar.wq, listw=dist.mat.inve)

wtsar<-spautolm(TASADER~PZONAUR+PRAMAAC+PHORANOC+PPUB,data=shapeCan@data, dist.mat.inve,zero.policy=TRUE, family="SAR")
summary(wtsar)

wtcar<-spautolm(TASADER~PZONAUR+PRAMAAC+PHORANOC+PPUB,data=shapeCan@data, dist.mat.inve,zero.policy=TRUE,family="CAR", tol.solve=1.0e-30)
summary(wtcar)

## Residuos
shapeCan@data$mod.res<-resid(mod1)
shapeCan@data$sarwt.res<-resid(wtsar) 
shapeCan@data$car.res<-resid(wtcar)

spplot(shapeCan,"mod.res", at=seq(min(shapeCan@data$mod.res,na.rm=TRUE)-1,max(shapeCan@data$mod.res,na.rm=TRUE)+1,length=14),col.regions=col1,xlim = c(-86.3, -82.5),ylim = c(8, 11.5))
spplot(shapeCan,"sarwt.res", at=seq(min(shapeCan@data$sarwt.res,na.rm=TRUE)-1,max(shapeCan@data$sarwt.res,na.rm=TRUE)+1,length=14),col.regions=col1,xlim = c(-86.3, -82.5),ylim = c(8, 11.5))
spplot(shapeCan,"car.res", at=seq(min(shapeCan@data$car.res,na.rm=TRUE)-1,max(shapeCan@data$car.res,na.rm=TRUE)+1,length=14),col.regions=col1,xlim = c(-86.3, -82.5),ylim = c(8, 11.5))


## Ajustados

shapeCan@data$mod.fit<-mod1$fitted.values
shapeCan@data$sarwt.fit<-wtsar$fit$fitted.values
shapeCan@data$car.fit<-wtcar$fit$fitted.value

spplot(shapeCan,"mod.fit", at=seq(min(shapeCan@data$mod.fit,na.rm=TRUE)-1,max(shapeCan@data$mod.fit,na.rm=TRUE)+1,length=14),col.regions=col1,xlim = c(-86.3, -82.5),ylim = c(8, 11.5))
spplot(shapeCan,"sarwt.fit", at=seq(min(shapeCan@data$sarwt.fit,na.rm=TRUE)-1,max(shapeCan@data$sarwt.fit,na.rm=TRUE)+1,length=14),col.regions=col1,xlim = c(-86.3, -82.5),ylim = c(8, 11.5))
spplot(shapeCan,"car.fit", at=seq(min(shapeCan@data$car.fit,na.rm=TRUE)-1,max(shapeCan@data$car.fit,na.rm=TRUE)+1,length=14),col.regions=col1,xlim = c(-86.3, -82.5),ylim = c(8, 11.5))

rbind(c(AIC(mod1),AIC(wtcar),AIC(wtsar)),c(BIC(mod1),BIC(wtcar),BIC(wtsar)))