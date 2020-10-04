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
sn2_tau <- var(tau_klast) # 885813.6667

# klasicna ocena srednje vrednosti
xn_psu <- N/M * mean(tau_klast) # 2.9388
D_xn_psu <- (N/M)^2 * sn2_tau/n * (1 - n/N) # 0.2419

# kolicnicka ocena srednje vrednosti
xn_kol <- sum(tau_klast)/sum(Mi_klast) # 3.0162
sn2_kol <- 1/(n-1) * sum((tau_klast - xn_kol *
                          Mi_klast)^2) # 1227.1843
D_xn_kol <- (N/M)^2 * sn2_kol/n * (1 - n/N) # 0.0003

# spojeni uzorak
indeksi <- unlist(indeksi)
m <- length(indeksi) # 3389

# pravljenje SVM modela
model <- svm(x[indeksi,],
             y[indeksi],
             fitted = F)
summary(model)

# provera kvaliteta modela
pred <- predict(model, x)
prec <- mean(y == pred) # 0.87385
