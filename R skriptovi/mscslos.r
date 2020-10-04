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

# PRVA ETAPA DVOETAPNOG UZORKA:
# izvlacenje primarnih jedinica
ind_klast <- sample(N, n)
indeksi <- lapply(ind_klast,
                  function (i) klast[[i]])
uzorak <- lapply(ind_klast,
                 function (i) obelezje_klast[[i]])

# uzoracke velicine skupina
Mi_klast <- M_klast[ind_klast]

# DRUGA ETAPA DVOETAPNOG UZORKA:
# izvlacenje sekundarnih jedinica
odabrani <- lapply(1:n,
            function (i) sample(Mi_klast[i],
                                Mi_klast[i]/2))
indeksi <- lapply(1:n,
           function (i) indeksi[[i]][odabrani[[i]]])
uzorak <- lapply(1:n,
          function (i) uzorak[[i]][odabrani[[i]]])
n_klast <- round(Mi_klast/2)

# ocena prema psu bez ponavljanja
tau_klast <- Mi_klast * sapply(uzorak, mean)
sn2_klast <- sapply(uzorak, var)
sn2_tau <- var(tau_klast) # 1185951.4721

# dodatna disperzija zbog druge etape
sn2_dod <- N/M^2 * mean(Mi_klast^2 *
                        sn2_klast/n_klast *
                        (1 - n_klast/Mi_klast)) # 0.0001

# klasicna ocena srednje vrednosti
xn_psu <- N/M * mean(tau_klast) # 3.5131
D_xn_psu <- (N/M)^2 * sn2_tau/n * (1 - n/N) +
                                   sn2_dod # 0.4241

# kolicnicka ocena srednje vrednosti
xn_kol <- sum(tau_klast)/sum(Mi_klast) # 3.4349
sn2_kol <- 1/(n-1) * sum((tau_klast - xn_kol *
                          Mi_klast)^2) # 1180892.9221
D_xn_kol <- (N/M)^2 * sn2_kol/n * (1 - n/N) +
                                   sn2_dod # 0.4223

# spojeni uzorak
indeksi <- unlist(indeksi)
m <- length(indeksi) # 1572

# pravljenje SVM modela
model <- svm(x[indeksi,],
             y[indeksi],
             fitted = F)
summary(model)

# provera kvaliteta modela
pred <- predict(model, x)
prec <- mean(y == pred) # 0.1546

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

# PRVA ETAPA DVOETAPNOG UZORKA:
# izvlacenje primarnih jedinica
ind_klast <- sample(N, n,
                    prob = psi,
                    replace = T)
indeksi <- lapply(ind_klast,
                  function (i) klast[[i]])
uzorak <- lapply(ind_klast,
                 function (i) obelezje_klast[[i]])

# uzoracke velicine skupina
Mi_klast <- M_klast[ind_klast]

# DRUGA ETAPA DVOETAPNOG UZORKA:
# izvlacenje sekundarnih jedinica
odabrani <- lapply(1:n,
            function (i) sample(Mi_klast[i],
                                Mi_klast[i]/2))
indeksi <- lapply(1:n,
           function (i) indeksi[[i]][odabrani[[i]]])
uzorak <- lapply(1:n,
          function (i) uzorak[[i]][odabrani[[i]]])
n_klast <- round(Mi_klast/2)

# ocena prema psu bez ponavljanja
tau_klast <- Mi_klast * sapply(uzorak, mean)

# Hansen-Hurwitzova ocena srednje vrednosti
xn_hh <- mean(tau_klast/Mi_klast) # 2.6848

# spojeni uzorak
indeksi <- unlist(indeksi)
m <- length(indeksi) # 1902

# pravljenje SVM modela
model <- svm(x[indeksi,],
             y[indeksi],
             fitted = F)
summary(model)

# provera kvaliteta modela
pred <- predict(model, x)
prec <- mean(y == pred) # 0.1531

# PRVA ETAPA DVOETAPNOG UZORKA:
# iskljucivanje ponovljenih entiteta
ind_klast <- unique(ind_klast)
indeksi <- lapply(ind_klast,
                  function (i) klast[[i]])
uzorak <- lapply(ind_klast,
                 function (i) obelezje_klast[[i]])
Mi_klast <- M_klast[ind_klast]
nn <- length(ind_klast) # 4

# fiksiranje generatora pseudoslucajnosti
set.seed(0)

# DRUGA ETAPA DVOETAPNOG UZORKA:
# izvlacenje sekundarnih jedinica
odabrani <- lapply(1:nn,
            function (i) sample(Mi_klast[i],
                                Mi_klast[i]/2))
indeksi <- lapply(1:nn,
           function (i) indeksi[[i]][odabrani[[i]]])
uzorak <- lapply(1:nn,
          function (i) uzorak[[i]][odabrani[[i]]])
n_klast <- round(Mi_klast/2)

# ocena prema psu bez ponavljanja
tau_klast <- Mi_klast * sapply(uzorak, mean)

# verovatnoce ukljucenja prvog reda
pi <- 1 - (1 - psi)^n
pi_klast <- pi[ind_klast]

# Horvitz-Thompsonova ocena srednje vrednosti
xn_ht <- 1/M * sum(tau_klast/pi_klast) # 2.4717

# spojeni uzorak
indeksi <- unlist(indeksi)
m <- length(indeksi) # 1534

# pravljenje SVM modela
model <- svm(x[indeksi,],
             y[indeksi],
             fitted = F)
summary(model)

# provera kvaliteta modela
pred <- predict(model, x)
prec <- mean(y == pred) # 0.15285
