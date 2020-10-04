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
n <- 3199

# odredjivanje koraka uzorkovanja
K <- ceiling(N/n) # 7

# fiksiranje generatora pseudoslucajnosti
set.seed(0)

# odabir prve sekundarne jedinice
r <- sample(K, 1) # 6

# uzimanje svih jedinki dok je moguce
indeksi <- seq(r, N, K)
ok <- length(indeksi) == n # FALSE

# popravka da bi bilo ok
if (!ok) {
  kraj <- length(indeksi) # 2857
  dopuna <- cumsum(c(indeksi[kraj] + K - N,
                     rep(K, n - kraj - 1)))
  indeksi <- c(indeksi, dopuna)
  ok <- length(indeksi) == n # TRUE
}

# uzorkovanje prema izracunatom
uzorak <- obelezje[indeksi]

# ocenjivanje srednje vrednosti
xn <- mean(uzorak) # 3.0150
sn2 <- var(uzorak) # 5.7021
D_xn <- sn2/n * (1 - n/N) # 0.0015
greska <- abs(sr - xn) # 0.0311

# interval poverenja
alpha <- 0.05
z <- qnorm(1 - alpha/2) # 1.9600
sirina <- z * sqrt(D_xn) # 0.0758
I_xn <- c(xn - sirina, # 2.94
          xn + sirina) # 3.09
upada <- sr >= I_xn[1] &&
         sr <= I_xn[2] # TRUE

# pravljenje SVM modela
model <- svm(x[indeksi,],
             y[indeksi],
             fitted = F)
summary(model)

# provera kvaliteta modela
pred <- predict(model, x)
prec <- mean(y == pred) # 0.87615
