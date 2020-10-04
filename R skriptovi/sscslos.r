# ucitavanje biblioteke i podataka
library(e1071)
slova <- read.csv('slova.csv')

# podela skupa na dva dela
x <- subset(slova,
            select = -slovo)
y <- as.factor(slova$slovo)

# izdvajanje obelezja od znacaja
obelezje <- x$x_ivice
M <- length(obelezje) # 20000
sr <- mean(obelezje) # 3.0461
vr <- var(obelezje) # 5.4407

# klasterovanje prema tipu slova
klast <- lapply(LETTERS,
                function (k) which(k == y))
obelezje_klast <- lapply(klast,
                         function (k) obelezje[k])
M_klast <- sapply(klast, length)
N <- length(M_klast) # 26

# IZVLACENJE BEZ PONAVLJANJA;
# velicina uzorka kako bi broj
# jedinki sto slicniji kao psu
n <- 3199 # zeljen br. jedinki
n <- round(n/(M/N)) # 4 grupe

# fiksiranje generatora pseudoslucajnosti
set.seed(0)

# uzorkovanje prema izracunatom
ind_klast <- sample(N, n)
indeksi <- lapply(ind_klast,
                  function (i) klast[[i]])
uzorak <- lapply(ind_klast,
                 function (i) obelezje_klast[[i]])

# histogrami uzorkovanih vrednosti
layout(matrix(1:n, n/2, n/2))
lapply(uzorak,
       function (k) hist(k, main = '',
                         xlab = '', ylab = '',
                         col = 'cadetblue'))

# uzoracke vrednosti po skupinama
Mi_klast <- M_klast[ind_klast]
tau_klast <- sapply(uzorak, sum)
sn2_tau <- var(tau_klast) # 1254254

# klasicna ocena srednje vrednosti
xn_psu <- N/M * mean(tau_klast) # 3.523
D_xn_psu <- (N/M)^2 * sn2_tau/n * (1 - n/N) # 0.4484

# kolicnicka ocena srednje vrednosti
xn_kol <- sum(tau_klast)/sum(Mi_klast) # 3.4446
sn2_kol <- 1/(n-1) * sum((tau_klast - xn_kol *
                            Mi_klast)^2) # 1251616.4646
D_xn_kol <- (N/M)^2 * sn2_kol/n * (1 - n/N) # 0.4475

# spojeni uzorak
indeksi <- unlist(indeksi)
m <- length(indeksi) # 3147

# pravljenje SVM modela
model <- svm(x[indeksi,],
             y[indeksi],
             fitted = F)
summary(model)

# provera kvaliteta modela
pred <- predict(model, x)
prec <- mean(y == pred) # 0.15585

# IZVLACENJE SA PONAVLJANJEM;
# velicina uzorka kako bi broj
# jedinki sto slicniji kao psu
n <- 3808 # zeljen br. jedinki
n <- round(n/(M/N)) # 5 grupa

# nejednake verovatnoce izvlacenja
# srazmerne velicini skupine
psi <- M_klast/M

# fiksiranje generatora pseudoslucajnosti
set.seed(0)

# uzorkovanje prema izracunatom
ind_klast <- sample(N, n,
                    prob = psi,
                    replace = T)
indeksi <- lapply(ind_klast,
                  function (i) klast[[i]])
uzorak <- lapply(ind_klast,
                 function (i) obelezje_klast[[i]])

# uzoracke vrednosti po skupinama
Mi_klast <- M_klast[ind_klast]
tau_klast <- sapply(uzorak, sum)

# Hansen-Hurwitzova ocena srednje vrednosti
xn_hh <- mean(tau_klast/Mi_klast) # 2.6710

# ocena disperzije ovakve HH ocene
D_xn_hh <- 1/(n-1) *
  mean((tau_klast/Mi_klast -
          xn_hh)^2) # 0.0799

# spojeni uzorak
indeksi <- unlist(indeksi)
m <- length(indeksi) # 3806

# pravljenje SVM modela
model <- svm(x[indeksi,],
             y[indeksi],
             fitted = F)
summary(model)

# provera kvaliteta modela
pred <- predict(model, x)
prec <- mean(y == pred) # 0.15335

# iskljucivanje ponovljenih entiteta
ind_klast <- unique(ind_klast)
indeksi <- lapply(ind_klast,
                  function (i) klast[[i]])
uzorak <- lapply(ind_klast,
                 function (i) obelezje_klast[[i]])
Mi_klast <- M_klast[ind_klast]
tau_klast <- sapply(uzorak, sum)

# verovatnoce ukljucenja prvog reda
pi <- 1 - (1 - psi)^n
pi_klast <- pi[ind_klast]

# Horvitz-Thompsonova ocena srednje vrednosti
xn_ht <- 1/M * sum(tau_klast/pi_klast) # 2.4546

# ocena disperzije ovakve HT ocene
nn <- length(ind_klast)
psi_klast <- psi[ind_klast]
D_xn_ht <- sum((1/pi_klast^2 - 1/pi_klast) * tau_klast^2)
for (k in 1:nn) {
  for (l in 1:nn) {
    if (k != l) {
      # verovatnoca ukljucenja drugog reda
      pi_kl <- pi_klast[k] + pi_klast[l] - 1 +
        (1 - psi_klast[k] - psi_klast[l])^n
      D_xn_ht <- D_xn_ht +
        (1/(pi_klast[k]*pi_klast[l]) - 1/pi_kl) *
        tau_klast[k] * tau_klast[l]
    }
  }
}
D_xn_ht <- 1/M^2 * D_xn_ht # 0.3618

# spojeni uzorak
indeksi <- unlist(indeksi)
m <- length(indeksi) # 3070

# pravljenje SVM modela
model <- svm(x[indeksi,],
             y[indeksi],
             fitted = F)
summary(model)

# provera kvaliteta modela
pred <- predict(model, x)
prec <- mean(y == pred) # 0.15335
