# ucitavanje biblioteke i podataka
library(e1071)
slova <- read.csv('slova.csv')

# podela skupa na dva dela
x <- subset(slova,
            select = -slovo)
y <- as.factor(slova$slovo)

# odredjivanje korelacija
kor <- cor(x)

# izbacivanje niskokorelisanih
niskor <- c(8, 9, 12, 16)
x <- subset(x,
            select = -niskor)

# MODEL NAD CELOKUPNIM SKUPOM PODATAKA;
# fiksiranje generatora pseudoslucajnosti
set.seed(0)

# pravljenje SVM modela
model <- svm(x, y)
summary(model)

# provera kvaliteta modela
pred <- fitted(model)
prec <- mean(y == pred) # 0.8565

# MODEL NAD PSU BEZ PONAVLJANJA;
# fiksiranje generatora pseudoslucajnosti
set.seed(0)

# ranije odredjen obim uzorka
N <- 20000
n <- 3199

# uzorkovanje prema izracunatom
indeksi <- sample(N, n)

# pravljenje SVM modela
model <- svm(x[indeksi,],
             y[indeksi],
             fitted = F)
summary(model)

# provera kvaliteta modela
pred <- predict(model, x)
prec <- mean(y == pred) # 0.74635

# MODEL NAD PSU SA PONAVLJANJEM;
# fiksiranje generatora pseudoslucajnosti
set.seed(0)

# ranije odredjen obim uzorka
n <- 3808

# uzorkovanje prema izracunatom
indeksi <- sample(N, n,
                  replace = T)

# pravljenje SVM modela
model <- svm(x[indeksi,],
             y[indeksi],
             fitted = F)
summary(model)

# provera kvaliteta modela
pred <- predict(model, x)
prec <- mean(y == pred) # 0.7544

# iskljucivanje ponovljenih entiteta
indeksi <- unique(indeksi)
n <- length(indeksi) # 3434

# pravljenje SVM modela
model <- svm(x[indeksi,],
             y[indeksi],
             fitted = F)
summary(model)

# provera kvaliteta modela
pred <- predict(model, x)
prec <- mean(y == pred) # 0.7528
