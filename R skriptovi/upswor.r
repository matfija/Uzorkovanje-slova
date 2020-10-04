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

# ranije odredjen obim uzorka;
# RAZLIKA U ODNOSU NA UPSWR
n <- 3199

# nejednake verovatnoce odabira
psi <- (obelezje - sr)^2
psi <- psi / sum(psi)

# fiksiranje generatora pseudoslucajnosti
set.seed(0)

# uzorkovanje prema izracunatom
indeksi <- sample(N, n,
                  prob = psi)
uzorak <- obelezje[indeksi]
psi_uz <- psi[indeksi]

# osnovne statistike uzorka
xn <- mean(uzorak) # 4.5355
sn2 <- var(uzorak) # 14.1192

# pravljenje SVM modela
model <- svm(x[indeksi,],
             y[indeksi],
             fitted = F)
summary(model)

# provera kvaliteta modela
pred <- predict(model, x)
prec <- mean(y == pred) # 0.683
