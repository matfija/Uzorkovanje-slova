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

# raslojavanje prema tipu slova
strat <- lapply(LETTERS,
                function (k) which(k == y))
obelezje_strat <- lapply(strat,
                         function (k) obelezje[k])
broj_strat <- length(strat) # 26
N_strat <- sapply(strat, length)

# dozvoljena apsolutna greska
d <- 0.07

# dozvoljena greska prve vrste
alpha <- 0.05

# odgovarajuci kvantil N(0,1)
z <- qnorm(1 - alpha/2) # 1.9600

# tacne statistike po slojevima
sr_strat <- sapply(obelezje_strat, mean)
vr_strat <- sapply(obelezje_strat, var)
sd_strat <- sapply(vr_strat, sqrt)

# neophodan obim uzorka
ups <- 1/N * sum(N_strat * vr_strat) # 2.3161
n <- ups * (z/d)^2 # 1815
n <- 2*ceiling(n) # 3632

# fiksiranje generatora pseudoslucajnosti
set.seed(0)

# namerno nepravilan raspored
n_strat <- rexp(broj_strat)
n_strat <- n/sum(n_strat) * n_strat
n_strat <- round(n_strat)

# popravka da bi bilo ok
while (sum(n_strat) != n) {
  if (sum(n_strat) > n) {
    i <- sample(broj_strat, 1)
    n_strat[i] <- n_strat[i] - 1
  } else {
    i <- sample(broj_strat, 1)
    n_strat[i] <- n_strat[i] + 1
  }
}

# uzorkovanje prema izracunatom
indeksi <- lapply(1:broj_strat,
           function (i) sample(N_strat[i], n_strat[i]))
uzorak <- lapply(1:broj_strat,
          function (i) obelezje_strat[[i]][indeksi[[i]]])

# uzoracke vrednosti po stratumima
xn_strat <- sapply(uzorak, mean)
sn2_strat <- sapply(uzorak, var)
D_xn_strat <- sn2_strat/n_strat * (1 - n_strat/N_strat)
greska <- abs(sr_strat - xn_strat)

# ocenjivanje srednje vrednosti
xn <- 1/N * sum(N_strat * xn_strat) # 3.0202
D_xn <- 1/N^2 * sum(N_strat^2 * D_xn_strat) # 0.0013

# interval poverenja
sirina <- z * sqrt(D_xn) # 0.0717
I_xn <- c(xn - sirina, # 2.95
          xn + sirina) # 3.09
upada <- sr >= I_xn[1] &&
         sr <= I_xn[2] # TRUE

# spojeni uzorak
indeksi <- unlist(sapply(1:broj_strat,
                  function (i) strat[[i]][indeksi[[i]]]))

# pravljenje SVM modela
model <- svm(x[indeksi,],
             y[indeksi],
             fitted = F)
summary(model)

# provera kvaliteta modela
pred <- predict(model, x)
prec <- mean(y == pred) # 0.74095
