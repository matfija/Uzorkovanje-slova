# ucitavanje biblioteke i podataka
library(e1071)
slova <- read.csv('slova.csv')

# vertikalna podela skupa na dva dela;
# simbolicki koncept: minus nije oduzimanje
x <- subset(slova,
            select = -slovo)
y <- as.factor(slova$slovo)

# fiksiranje generatora pseudoslucajnosti
set.seed(0)

# pravljenje SVM modela
model <- svm(x, y)
summary(model)

# provera kvaliteta modela
pred <- fitted(model)
prec <- mean(y == pred) # 0.9624
