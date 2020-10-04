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

# raslojavanje prema tipu slova;
# funkcionalni koncept: lapply od niza svih slova
# pravi listu indeksa podataka u kojima tip slova y
# odgovara tekucem slovu k, dakle gde je k == y
strat <- lapply(LETTERS,
                function (k) which(k == y))
obelezje_strat <- lapply(strat,
                         function (k) obelezje[k])
broj_strat <- length(strat) # 26
N_strat <- sapply(strat, length)
ok <- sum(N_strat) == N # TRUE

# dozvoljena apsolutna greska
d <- 0.07

# dozvoljena greska prve vrste
alpha <- 0.05

# odgovarajuci kvantil N(0,1)
z <- qnorm(1 - alpha/2) # 1.9600

# tacne statistike po slojevima
sr_strat <- sapply(obelezje_strat, mean)
vr_strat <- sapply(obelezje_strat, var)

# neophodan obim uzorka
ups <- 1/N * sum(N_strat * vr_strat) # 2.3161
n <- ups * (z/d)^2 # 1815
n <- 2*ceiling(n) # 3632

# proporcionalni raspored
n_strat <- round(n*N_strat/N)
ok <- sum(n_strat) == n # FALSE

# fiksiranje generatora pseudoslucajnosti
set.seed(0)

# popravka da bi bilo ok
while (!ok) {
  if (sum(n_strat) > n) {
    i <- sample(broj_strat, 1)
    n_strat[i] <- n_strat[i] - 1
  } else {
    i <- sample(broj_strat, 1)
    n_strat[i] <- n_strat[i] + 1
  }
  ok <- sum(n_strat) == n
}

# uzorkovanje prema izracunatom
indeksi <- lapply(1:broj_strat,
           function (i) sample(N_strat[i], n_strat[i]))
ok <- all(sapply(indeksi, length) == n_strat) # TRUE
uzorak <- lapply(1:broj_strat,
          function (i) obelezje_strat[[i]][indeksi[[i]]])
ok <- all(sapply(uzorak, length) == n_strat) # TRUE

# uzoracke vrednosti po stratumima
xn_strat <- sapply(uzorak, mean)
sn2_strat <- sapply(uzorak, var)
D_xn_strat <- sn2_strat/n_strat * (1 - n_strat/N_strat)
greska <- abs(sr_strat - xn_strat)

# ocenjivanje srednje vrednosti
xn <- 1/N * sum(N_strat * xn_strat) # 3.0429
D_xn <- 1/N^2 * sum(N_strat^2 * D_xn_strat) # 0.0005

# interval poverenja
sirina <- z * sqrt(D_xn) # 0.0448
I_xn <- c(xn - sirina, # 3.00
          xn + sirina) # 3.09
upada <- sr >= I_xn[1] &&
         sr <= I_xn[2] # TRUE

# spojeni uzorak
indeksi <- unlist(sapply(1:broj_strat,
                  function (i) strat[[i]][indeksi[[i]]]))
ok <- length(indeksi) == n # TRUE

# pravljenje SVM modela
model <- svm(x[indeksi,],
             y[indeksi],
             fitted = F)
summary(model)

# provera kvaliteta modela
pred <- predict(model, x)
prec <- mean(y == pred) # 0.8794
