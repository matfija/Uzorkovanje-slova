# ucitavanje biblioteke i podataka
library(e1071)
slova <- read.csv('slova.csv')

# podela skupa na dva dela
x <- subset(slova,
            select = -slovo)
y <- as.factor(slova$slovo)

# izdvajanje obelezja od znacaja
obelezje <- x$x_ivice
N <- length(obelezje) # 20000
sr <- mean(obelezje) # 3.0461
vr <- var(obelezje) # 5.4407

# ranije odredjen obim uzorka
n <- 3808

# nejednake verovatnoce odabira
psi <- (obelezje - sr)^2
psi <- psi / sum(psi)

# fiksiranje generatora pseudoslucajnosti
set.seed(0)

# uzorkovanje prema izracunatom
indeksi <- sample(N, n,
                  prob = psi,
                  replace = T)
uzorak <- obelezje[indeksi]
psi_uz <- psi[indeksi]

# osnovne statistike uzorka
xn <- mean(uzorak) # 5.5087
sn2 <- var(uzorak) # 17.1583

# Hansen-Hurwitzova ocena srednje vrednosti
xn_hh <- 1/N * mean(uzorak/psi_uz) # 2.3304

# ocena disperzije ovakve HH ocene
D_hh_ocena <- 1/(n-1) *
              mean((uzorak/(N*psi_uz) -
                   xn_hh)^2) # 0.0033

# pravljenje SVM modela
model <- svm(x[indeksi,],
             y[indeksi],
             fitted = F)
summary(model)

# provera kvaliteta modela
pred <- predict(model, x)
prec <- mean(y == pred) # 0.68785

# iskljucivanje ponovljenih entiteta
indeksi <- unique(indeksi)
uzorak <- obelezje[indeksi]
psi_uz <- psi[indeksi]

# osnovne statistike uzorka
n <- length(indeksi) # 2781
xn <- mean(uzorak) # 4.5750
sn2 <- var(uzorak) # 14.5171

# verovatnoce ukljucenja prvog reda
pi <- 1 - (1 - psi)^n
pi_uz <- pi[indeksi]

# Horvitz-Thompsonova ocena srednje vrednosti
xn_ht <- 1/N * sum(uzorak/pi_uz) # 3.0334

# ocena disperzije ovakve HT ocene
D_ht_ocena <- sum((1/pi_uz^2 - 1/pi_uz) * uzorak^2)
for (k in 1:n) {
  for (l in 1:n) {
    if (k != l) {
      # verovatnoca ukljucenja drugog reda
      pi_kl <- pi_uz[k] + pi_uz[l] - 1 +
              (1 - psi_uz[k] - psi_uz[l])^n
      D_ht_ocena <- D_ht_ocena +
                   (1/(pi_uz[k]*pi_uz[l]) - 1/pi_kl) *
                   uzorak[k] * uzorak[l]
    }
  }
}
D_ht_ocena <- 1/N^2 * D_ht_ocena # 0.0058

# Hajekova ocena srednje vrednosti
xn_hajek <- sum(uzorak/pi_uz) / sum(1/pi_uz) # 3.0000

# pravljenje SVM modela
model <- svm(x[indeksi,],
             y[indeksi],
             fitted = F)
summary(model)

# provera kvaliteta modela
pred <- predict(model, x)
prec <- mean(y == pred) # 0.68045
