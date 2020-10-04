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

# pilot istrazivanje nad malim uzorkom
n <- 100
indeksi <- sample(N, n)
uzorak <- obelezje[indeksi]
sn2 <- var(uzorak) # 4.8562

# neophodan obim uzorka
n0 <- sn2 * (z/d)^2 # 3807
n1 <- (1/n0 + 1/N)^{-1} # 3198
n <- ceiling(n1) # 3199

# uzorkovanje prema izracunatom
indeksi <- sample(N, n)
uzorak <- obelezje[indeksi]

# ocenjivanje srednje vrednosti
xn <- mean(uzorak) # 3.0078
sn2 <- var(uzorak) # 5.4111
D_xn <- sn2/n * (1 - n/N) # 0.0014
greska <- abs(sr - xn) # 0.0383

# interval poverenja
sirina <- z * sqrt(D_xn) # 0.0739
I_xn <- c(xn - sirina, # 2.93
          xn + sirina) # 3.08
upada <- sr >= I_xn[1] &&
         sr <= I_xn[2] # TRUE

# pravljenje SVM modela
model <- svm(x[indeksi,],
             y[indeksi],
             fitted = F)
summary(model)

# provera kvaliteta modela
pred <- predict(model, x)
prec <- mean(y == pred) # 0.8768
