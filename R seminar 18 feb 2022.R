
# Kjør linjen du er på med å trykke Ctrl-Enter
2+5

# install.packages("lm.beta")
library(lm.beta)

# Lage variabler of tilordne verdi
A<-5
A

B<-(-5)
B

# Funksjoner returnerer verdier, du kan legge disse i nye variabler.
C<-abs(B)

# Slik lager du vektorer (ordnede lister av tall)
X<-c(1,2,6,4,2)

X[c(3)] # Få tak i det tredje elementet
X[c(2,4)] # Få tak i det andre og fjerde elementet

# Ekstrafunksjoner kommer gjennom pakker. Slik installerer du en pakke:
install.packages("tidyverse")

# Hver gang du starter R/Rstudio må du laste inn de pakkene du trenger (som allerede er installert)
library(tidyverse)

# Her leses en tekst fil inn.
dt<-read_csv("https://www.sv.uio.no/psi/personer/vit/nikolaic/psy2014/rwa_dataset.dat")
dt <- dt %>%  mutate (male = factor(male)) # Ikke så viktig å forstå, men jeg gjør variabelen "male " om til en faktor.

#
dt %>% select(male)

# Ta ut en enkeltkolonne (her RWA)
dt$RWA

mean(dt$RWA) # Snitt
var(dt$RWA) # Varians
sd(dt$RWA) # Standardavvik

cor(dt$RWA,dt$BFI_O) # Korrelasjon

hist(dt$RWA) # "Base"-R histogram
plot(dt$BFI_O,dt$RWA) # "Base"-R spredningsplot


ggplot(data=dt, aes(x=RWA))+geom_histogram() # GGplot histogram
ggplot(data=dt, aes(x=RWA))+geom_histogram(color="white") # Litt penere

ggplot(data=dt, aes(x=RWA))+geom_density() #Bytt geom til density

ggplot(data=dt, aes(x=BFI_O, y=RWA))+geom_point() # GGplot spredningsplot

# "Jitter" rister litt på punktene og hindrer overplotting
ggplot(data=dt, aes(x=BFI_O, y=RWA))+geom_point(position="jitter")

# Bytt farger
ggplot(data=dt, aes(x=BFI_O, y=RWA))+
  geom_point(position="jitter", color="steelblue")

# Sett fargene etter nivåer på en faktor
ggplot(data=dt, aes(x=BFI_O, y=RWA, color=male))+
  geom_point(position="jitter")

# Sett fargen etter nivået på en kontinuerlig variabel (alder)
ggplot(data=dt, aes(x=BFI_O, y=RWA, color=age))+
  geom_point(position="jitter")

# Legg regresjonslinjen til spredningsplottet med geom_smooth
ggplot(data=dt, aes(x=BFI_O, y=RWA))+
  geom_point(position="jitter")+
  geom_smooth(method="lm")


# Bivaruar regresjon
lm(RWA~BFI_O, data=dt)
m1<-lm(RWA~BFI_O, data=dt)

# Skriv ut resultatene fra regresjonsanalysen
summary(m1)

# Ta ut residualene og lag et histogram
hist(residuals(m1))

# Broom pakken har et par nyttige hjelperfunksjoner, vi skal bruke augment()
library(broom)

augment(m1, dt) # Legg til en del resultater av kjøringen til datasettet
aug<-augment(m1)

# Histogram av residualene
ggplot(aug, aes(x=.resid))+geom_histogram(color="white")

# spredningsplot av residualene
ggplot(aug, aes(x=BFI_O, y=.resid))+geom_point()


