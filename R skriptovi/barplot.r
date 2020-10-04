# ucitavanje podataka
slova <- read.csv('slova.csv')
cilj <- slova$slovo

# brojanje svakog pojedinacnog slova;
# funkcionalni koncepti: sapply umesto petlje
# i anonimna funkcija bez return naredbe
freq <- sapply(LETTERS,
               function(x) sum(x == cilj))

# iscrtavanje trakastog dijagrama
barplot(freq,
        main = 'Raspodela slova',
        xlab = 'Slova',
        ylab = 'Frekvencije',
        cex.names = .7,
        col = 'cadetblue')
