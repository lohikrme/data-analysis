# EXAM OF STATISTICAL DATA ANALYSIS 1
# 7.3.2024


options(digits=10)


# Tehtävä 1: Muodosta regressioyhtälö
# Anna vakiotermi ja kulmakerroin
# 4 desimaalin tarkkuudella.
# Anna selitysaste 4 desimaalin tarkkuudella.
# Datana toimii seuraava:

ajettu_km <- c(76, 72, 89, 144, 158, 92, 156, 109, 138, 100, 51, 107, 129, 65, 121)
polttoaine_l <- c(8, 9, 11, 18, 19, 10, 20, 14, 17, 12, 6, 13, 16, 7, 15)

# Oletan, että ajettu_km on selitettävä muuttuja, ja polttoaine on selittävä.
# lm yhtälöön ensin selitettävä, sitten selittävä

malli = lm(ajettu_km ~ polttoaine_l)
summary(malli)

# piirrä graafi (selkeämpi):

# Piirrä datapisteet
plot(polttoaine_l, ajettu_km, 
     main="Lineaarinen regressio", 
     xlab="polttoaineen_maara", ylab="ajettu_matka")

# Piirrä regressiosuora
abline(malli, col="darkgreen")


vakiotermi = round(coef(malli)[1], 4)
kulmakerroin = round(coef(malli)[2], 4)
selitysaste = round(summary(malli)$r.squared, 4)

sprintf("%.4f", vakiotermi)
sprintf("%.4f", kulmakerroin)
sprintf("%.4f", selitysaste)



# Tehtävä 2: Monivalintatentin
# jokaisessa kysymyksessä on 
# 3 vastausvaihtoehtoa, joista yksi oikea.
# Jos on 13 kysymystä, millä tn
# vähintään 3 menee oikein, 
# jos vastaukset arvotaan? 
# Anna tulos kahden merkitsevän numeron 
# tarkkuudella desimaalilukuna.

# Ok, eli 1/3 per kysymys saada oikein.
# Kyseessä on binomijakauma
# Voisi laskea joko
# 1 - 0 oikein - 1 oikein - 2 oikein
# Tai sitten R:llä käyttäen pbinom

p_less_than_3 = pbinom(2, size=13, prob=1/3)
answer1 = 1 - p_less_than_3

p_zero_right = (2/3)^13
p_one_right = (1/3)*(2/3)^12*choose(13,1)
p_two_right = (1/3)^2*(2/3)^11*choose(13,2)

answer2 = 1 - p_zero_right - p_one_right - p_two_right


signif(answer1, 2)
signif(answer2, 2)


# Tehtävä 3A: Ihmiset kuuluvat johonkin 
# veriryhmistä A, B, AB, O. Suomalaisista
# 44% A, 17% B, 8% AB, 31% O. 
# Millä tn 2 ihmistä molemmat kuuluvat O:

signif(0.31 * 0.31, 2)

# Tehtävä 3B: Samaan veriryhmään:

both_a = 0.44^2
both_b = 0.17^2
both_ab = 0.08^2
both_o = 0.31^2

signif(both_a + both_b + both_ab + both_o, 2)


# Tehtävä 4A: Tänä vuonna pelaaja sai 725p.
# Aiemmilta 10 vuodelta on myös kerätty talteen pisteet.
# Laske merkitsevyystasolla 0.05, että onko
# pelaajan tulos muuttunut aiemmasta.
# Anna p-arvo 5 desimaalin tarkkuudella.

data = c(721, 699, 721, 710, 719, 741, 724, 716, 720, 700)

# Koska otoskoko on näin pieni, käytän T-testiä:
# H0: pelaajan pisteet eivät ole muuttuneet.
# H1: pelaajan pisteet ovat muuttuneet.

new_score = 725

t_testi = t.test(data, mu = new_score)
print(t_testi)

p_value = round(t_testi$p.value, 5)
p_value

# Tehtävä 4B: Onko pelaajan tulos huonontunut.
# Anna p-arvo 5 desimaalin tarkkuudella.
# Arvioi H0 ja H1 hypoteesit.

t_testi2 = t.test(data, mu = new_score, alternative = "greater")
print(t_testi2)

p_value2 = round(t_testi2$p.value, 5)
p_value2

summary(data)

t_testi3 = t.test(data, mu = new_score, alternative = "greater", conf.level = 0.95)
print(t_testi3)


# Tehtävä 5A: Tuotteen käyttöikää mitattiin
# 35 kappaleen otoksella. Mittaustuloksien 
# keskiarvoksi saatiin 1480 tuntia
# otoskeskihajonnaksi 126 tuntia.
# Vastaa 1 desimaalin tarkkuudella
# Mikä on odotusarvon 99% luottamusväli

# Koska emme tunne populaatiokeskihajontaa,
# hyödynnämme t-jakaumaa

mean1 = 1480
sd1 = 126
sample_size1 = 35

luottamusvali_funktio = function (keskiarvo, keskihajonta, luottamusprosentti, otoskoko) {
  merkitsevyystaso = 1 - (1 - luottamusprosentti/100) / 2
  print(merkitsevyystaso)
  T =  qt(merkitsevyystaso, df = otoskoko - 1)
  keskivirhe = keskihajonta / sqrt(otoskoko) # keskihajonta pitää sisällään "tutkittavan yksikön"
  luottamusvalin_puolikas = T * keskivirhe
  # mene luottamusvälin puolikkaan verran vasemmalle ja oikealle keskiarvosta
  pienin = round(keskiarvo - luottamusvalin_puolikas, 2)
  suurin = round(keskiarvo + luottamusvalin_puolikas, 2)
  formatted_pienin = sprintf("%.1f", pienin)
  formatted_suurin = sprintf("%.1f", suurin)
  return (paste("pienin:", formatted_pienin, "... suurin:", formatted_suurin))
}

luottamusvali_funktio(mean1, sd1, 99, sample_size1)


# Tehtävä 5A: Muuten sama, mutta 
# aiemmin annettu otoskeskihajonta onkin nyt
# populaatiokeskihajonta

# eli, koska otoskoko > 30 ja populaatiokeskihaj
# on tunnettu, siksi käytämme Z-hajontaa

luottamusvali_funktio2 = function (keskiarvo, keskihajonta, luottamusprosentti, otoskoko) {
  merkitsevyystaso = 1 - (1 - luottamusprosentti/100) / 2
  print(merkitsevyystaso)
  Z =  qnorm(merkitsevyystaso)
  keskivirhe = keskihajonta / sqrt(otoskoko) # keskihajonta pitää sisällään "tutkittavan yksikön"
  luottamusvalin_puolikas = Z * keskivirhe
  # mene luottamusvälin puolikkaan verran vasemmalle ja oikealle keskiarvosta
  pienin = round(keskiarvo - luottamusvalin_puolikas, 2)
  suurin = round(keskiarvo + luottamusvalin_puolikas, 2)
  formatted_pienin = sprintf("%.1f", pienin)
  formatted_suurin = sprintf("%.1f", suurin)
  return (paste("pienin:", formatted_pienin, "... suurin:", formatted_suurin))
}

luottamusvali_funktio2(mean1, sd1, 99, sample_size1)





# Tehtävä 6: Laske seuraavalle lukujonolle arvoja:
lukujono = c(6, 24, 3, 21, 9, 21, 14)

# a) keskiarvo 2 desimaalin tarkkuudella
sprintf("%.2f", round(mean(lukujono), 2))

# b) keskihajonta 2 desimaalin tarkkuudella
sprintf("%.2f", round(sd(lukujono), 2))

# c) mediaani tarkka arvo:
median(lukujono)

# d) moodi tarkka arvo:
moodit <- as.numeric(names(which(table(lukujono) == max(table(lukujono)))))
print(moodit)
