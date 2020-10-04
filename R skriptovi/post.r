# ucitavanje podataka
slova <- read.csv('slova.csv')

# izdvajanje obelezja od znacaja
obelezje <- slova$x_ivice
N <- length(obelezje) # 20000
sr <- mean(obelezje) # 3.0461
vr <- var(obelezje) # 5.4407

# ranije odredjen obim uzorka
n <- 3199

# fiksiranje generatora pseudoslucajnosti
set.seed(0)

# uzorkovanje prema izracunatom
indeksi <- sample(N, n)
uzorak <- obelezje[indeksi]
tip <- slova$slovo[indeksi]

# raslojavanje prema tipu slova
post <- lapply(LETTERS,
               function (k) which(tip == k))
n_post <- sapply(post, length)
broj_post <- length(post) # 26
ok <- sum(n_post) == n # TRUE

# raspodela populacije po stratumima
N_post <- sapply(LETTERS,
                 function (k) sum(slova$slovo == k))
ok <- sum(N_post) == N # TRUE

# vrednost obelezja na slojevima
obelezje_post <- lapply(post,
                        function (k) obelezje[k])
xn_post <- sapply(obelezje_post, mean)
sn2_post <- sapply(obelezje_post, var)

# ocenjivanje srednje vrednosti
xn <- 1/N * sum(N_post * xn_post) # 2.9877
D_xn <- 1/(n*N) * (1 - n/N) * sum(N_post * sn2_post) +
        1/n^2 * (1 - (n-1)/(N-1)) *
        sum((1 - N_post/N) * sn2_post) # 0.0014

# interval poverenja
alpha <- 0.05
z <- qnorm(1 - alpha/2) # 1.9600
sirina <- z * sqrt(D_xn) # 0.0737
I_xn <- c(xn - sirina, # 2.91
          xn + sirina) # 3.06
upada <- sr >= I_xn[1] &&
         sr <= I_xn[2] # TRUE
