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

# dozvoljena apsolutna greska
d <- 0.07

# dozvoljena greska prve vrste
alpha <- 0.05

# odgovarajuci kvantil N(0,1)
z <- qnorm(1 - alpha/2) # 1.9600

# fiksiranje generatora pseudoslucajnosti
set.seed(0)

# pilot istrazivanje nad malim uzorkom;
# OVDE JE REPLACE TRUE, PONAVLJANJE
n <- 100
indeksi <- sample(N, n,
                  replace = T)
uzorak <- obelezje[indeksi]
sn2 <- var(uzorak) # 4.8562

# neophodan obim uzorka;
# KORAK MANJE NEGO SRSWOR
n0 <- sn2 * (z/d)^2 # 3807
n <- ceiling(n0) # 3808

# uzorkovanje prema izracunatom;
# OVDE JE REPLACE TRUE, PONAVLJANJE
indeksi <- sample(N, n,
                  replace = T)
uzorak <- obelezje[indeksi]

# ocenjivanje srednje vrednosti;
# BEZ OTKLONA U DISPERZIJI
xn <- mean(uzorak) # 3.0355
sn2 <- var(uzorak) # 5.4899
D_xn <- sn2/n # 0.0014
greska <- abs(sr - xn) # 0.0106

# interval poverenja
sirina <- z * sqrt(D_xn) # 0.0744
I_xn <- c(xn - sirina, # 2.96
          xn + sirina) # 3.11
upada <- sr >= I_xn[1] &&
         sr <= I_xn[2] # TRUE

# pravljenje SVM modela
model <- svm(x[indeksi,],
             y[indeksi],
             fitted = F)
summary(model)

# provera kvaliteta modela
pred <- predict(model, x)
prec <- mean(y == pred) # 0.8829

# iskljucivanje ponovljenih entiteta
indeksi <- unique(indeksi)
uzorak <- obelezje[indeksi]

# ocenjivanje srednje vrednosti
n <- length(uzorak) # 3445
xn <- mean(uzorak) # 3.0165
sn2 <- var(uzorak) # 5.4385
D_xn <- sum(sapply(1:(N-1),
            function (k) (k/N)^(n-1)/N)) *
            sn2 # 0.0014
greska <- abs(sr - xn) # 0.0296

# pravljenje SVM modela
model <- svm(x[indeksi,],
             y[indeksi],
             fitted = F)
summary(model)

# provera kvaliteta modela
pred <- predict(model, x)
prec <- mean(y == pred) # 0.8811
