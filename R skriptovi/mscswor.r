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

# fiksiranje generatora pseudoslucajnosti
set.seed(0)

# odredjivanje velicina klastera
M_klast <- c()
while (sum(M_klast) < M) {
  nov <- runif(1, 300, 1300)
  nov <- round(nov)
  M_klast <- c(M_klast, nov)
}
N <- length(M_klast) # 23

# popravka da bi bilo ok
while (sum(M_klast) != M) {
  if (sum(M_klast) > M) {
    i <- sample(N, 1)
    M_klast[i] <- M_klast[i] - 1
  } else {
    i <- sample(N, 1)
    M_klast[i] <- M_klast[i] + 1
  }
}

# klasterovanje prema velicini;
# pomoc su kumulativni indeksi
kumul <- c(0, cumsum(M_klast))
klast <- lapply(2:length(kumul),
                function (i) (kumul[i-1]+1):kumul[i])
obelezje_klast <- lapply(klast,
                         function (k) obelezje[k])

# velicina uzorka kako bi broj
# jedinki sto slicniji kao psu
n <- 3199 # zeljen br. jedinki
n <- round(n/(M/N)) # 4 grupe

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
sn2_tau <- var(tau_klast) # 838969.9985

# dodatna disperzija zbog druge etape
sn2_dod <- N/M^2 * mean(Mi_klast^2 *
                   sn2_klast/n_klast *
              (1 - n_klast/Mi_klast)) # 0.0003

# klasicna ocena srednje vrednosti
xn_psu <- N/M * mean(tau_klast) # 2.9127
D_xn_psu <- (N/M)^2 * sn2_tau/n * (1 - n/N) +
                                  sn2_dod # 0.2294

# kolicnicka ocena srednje vrednosti
xn_kol <- sum(tau_klast)/sum(Mi_klast) # 2.9127
sn2_kol <- 1/(n-1) * sum((tau_klast - xn_kol *
                          Mi_klast)^2) # 989.5052
D_xn_kol <- (N/M)^2 * sn2_kol/n * (1 - n/N) +
                                  sn2_dod # 0.0005

# spojeni uzorak
indeksi <- unlist(indeksi)
m <- length(indeksi) # 1694

# pravljenje SVM modela
model <- svm(x[indeksi,],
             y[indeksi],
             fitted = F)
summary(model)

# provera kvaliteta modela
pred <- predict(model, x)
prec <- mean(y == pred) # 0.81655
