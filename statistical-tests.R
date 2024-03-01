# statistical tests 
# 26. helmikuu 2024
# myy = populaatiokeskiarvo, x_viiva = otoskeskiarvo

# lower.tail TRUE laskee vasemman hännän todennäköisyyden
# lower.tail FALSE laskee oikean hännän todennäköisyyden

# eli kun lasketaan, onko jokin suurempi kuin jokin toinen
# käyttetään FALSE. ja toisin päin.

# Kun on 2 eri dataa ja niiden yhdistetty T-testi, tällöin
# on tärkeää huomata, kummin päin tehdään miinuslasku
# eli jos vaikka halutaan testa, onko A isompi kuin B
# miinustetaan A:sta tällöin B. Jos taas, onko B A:ta isompi,
# miinustetaan B:sta A. Muuten käy niin, että 
# lower.tail FALSE/TRUE antaa vastauksen peilikuvan

options(digits=10)

getwd()

# tehtävä 1 A: Otoskoko 35, otoskeskiarvo 168.1cm.
# Perusjoukon keskihajonta sigma = 10cm. 
# Testaa poikkeaako myy arvosta 172 tilastollisesti?
# Mikä on testiin liittyvä p-arvo 8 desimaalin tarkkuudella?

# Mikä on johtopäätös, vastausvaihtoehdot ovat:
# 0 = ei poikkea tilastollisesti
# 1 = poikkeaa melkein merkitsevästi (p < 0.05)
# 2 = poikkeaa merkitsevästi (p < 0.01)
# 3 = poikkeaa erittäin merkitsevästi (p < 0.001)

# Tunnen populaatiokeskihajonnan ja otoskoko on iso,
# ja koska suuntaa ei ole määritelty, käytän 
# 2-suuntaista Z-testiä, jossa p arvo kerrotaan 2:lla:

z_testi_kaksisuuntainen = function(otoskoko, otoskeskiarvo, populaatiokeskiarvo, keskihajonta) {
  jaettava = otoskeskiarvo - populaatiokeskiarvo
  jakaja = keskihajonta / sqrt(otoskoko)
  z = jaettava / jakaja
  p_value = 2 * pnorm(abs(z), lower.tail = FALSE)
  formatted_p_value = sprintf("%.8f", p_value)
  return (formatted_p_value)
}

z_testi_kaksisuuntainen(35, 168.1, 172, 10)

# Tehtävä 1 B: Muuten sama, mutta otoskoko 100:

z_testi_kaksisuuntainen(100, 168.1, 172, 10)

# seur 2 pitäisi antaa samal tulos:
z_testi_kaksisuuntainen(35, 168.1, 172, 10)
z_testi_kaksisuuntainen(35, 173.9, 170, 10)


# Tehtävä 2: Suklaakonvehtirasian painoksi 
# on ilmoitettu 300g, mutta otoskoko 20, Otoskeskiarvo 295g,
# otoskeskihajonta 7.8g. Testaa 2-suuntaisella testillä
# 5% merkitsevyystasolla, onko ilmoitettu paino oikea?
# Anna p-arvo 9 desimaalin tarkkuudella ja vastaa
# Astuuko voimaan H1 hypoteesi 5% riskkitasolla?

# Koska kyseessä on otos, käytän 2-suunt T-testiä:

t_testi_kaksisuuntainen = function(otoskoko, otoskeskiarvo, populaatiokeskiarvo, otoskeskihajonta) {
  jaettava = otoskeskiarvo - populaatiokeskiarvo
  jakaja = otoskeskihajonta / sqrt(otoskoko)
  t = jaettava / jakaja
  p_value = 2 * pt(abs(t), df = otoskoko - 1, lower.tail = FALSE)
  formatted_p_value = sprintf("%.9f", p_value)
  return (formatted_p_value)
}

# seur 2 pitäisi antaa samal tulos:
t_testi_kaksisuuntainen(20, 295, 300, 7.8)
t_testi_kaksisuuntainen(20, 305, 300, 7.8)


# Tehtävä 3: Tyttöjen ja poikien empaattisuutta
# vertailtiin pienessä tutkimuksessa
# Selvitä 2-suuntaisella testillä 5% riskitasolla,
# onko tyttöjen ja poikien keskiarvoissa eroja?

# Data on seuraava: 
girls = c(52, 56, 56, 58, 60, 62, 68, 74)
boys = c(60, 58, 56, 54, 52, 50, 48, 46)

# eli 2 eri dataa, jonka seurauksena lasketaan
# molemmille otoskeskiarvo, otoskeskihajonta

girls_mean = mean(girls)
girls_sd = sd(girls)
girls_sample_size = length(girls)
boys_mean = mean(boys)
boys_sd = sd(boys)
boys_sample_size = length(boys)

girls_mean
girls_sd
girls_sample_size
boys_mean
boys_sd
boys_sample_size

# a will be girls, be will be boys
# huomaa vapausasteet, molemmat otoskoot - 2
two_sample_t_test_kaksisuuntainen = function (a_mean, a_sd, a_sample_size, b_mean, b_sd, b_sample_size) {
  if (a_sample_size != b_sample_size) {
    return ("fail")
  }
  jaettava = a_mean - b_mean
  jakaja = sqrt((a_sd^2 / a_sample_size) + (b_sd^2 / b_sample_size))
  t = jaettava / jakaja
  p_value = 2 * pt(abs(t), df = a_sample_size + b_sample_size - 2, lower.tail = FALSE)
  formatted_p_value = sprintf("%.5f", p_value)
  return (formatted_p_value)
}

two_sample_t_test_kaksisuuntainen(girls_mean, girls_sd, girls_sample_size, boys_mean, boys_sd, boys_sample_size)



# tehtävä 4: Erään tuoteominaisuuden ilmoitettiin
# olevan keskimäärin 15 yksikköä. Tuotteesta otettiin
# 20 tuotteen otos. Testaa 1% riskillä, ovatko
# ominaisuudet keskimäärin ilmoitettua pienempiä?
# Eli nyt tehdään yksisuuntainen t-testi vasemmalle
# Otosdata on seuraava:
dataset = c(13.7, 13.7, 14.4, 14.1, 17.0, 
            15.3, 14.9, 13.3, 13.0, 14.1, 
            14.5, 14.0, 13.5, 16.6, 14.9, 
            13.2, 15.2, 13.4, 15.0, 16.3)

sd = sd(dataset)
otoskoko = length(dataset)
otoskeskiarvo = mean(dataset)
populaatiokeskiarvo = 15

t_testi_vasemmalle = function(otoskoko, otoskeskiarvo, populaatiokeskiarvo, otoskeskihajonta) {
  jaettava = otoskeskiarvo - populaatiokeskiarvo
  jakaja = otoskeskihajonta / sqrt(otoskoko)
  t = jaettava / jakaja
  p_value = pt(t, df = otoskoko - 1, lower.tail = TRUE)
  formatted_p_value = sprintf("%.5f", p_value)
  return (formatted_p_value)
}

t_testi_vasemmalle(otoskoko, otoskeskiarvo, populaatiokeskiarvo, sd)



# tehtävä 5: Kaksi konetta tuottaa esineitä, ja
# esineille annettiin laatua kuvaava lukuarvo.
# testaa 5% riskillä, voidaanko jälkimmäisen koneen
# tuottamia tuotteita pitää korkealaatuisempina?
# H0: Oletetaan ei eroa
# H1: Kone2:n tuotteet keskimäärin korkealaatuisempia.
# Tehtävän data on seuraava:
kone1 = c(3.2,	3.6,	3.9,	2.8,	4.1,	3.5,	3.0,	2.7,	3.7,	3.4,	4.1,	3.7,	3.1,	4.0,	3.4)
kone2 = c(2.2, 2.9,	4.7,	3.9,	3.9,	3.6,	4.2,	3.7,	4.1,	3.7,	4.4,	4.0,	3.4,	3.5,	3.6)

#a on kone1, b on kone2
# Kahden otoksen oikeasuuntainen t-testi
# Koska testaan, onko B parempi kuin A, 
# miinustan B_mean A_meanista, koska tällöin
# jos B_mean > A_mean, niin erotus on positiivinen
# Ja sen seurauksena mitä suurempi B_mean on verrattuna A_mean,
# sitä suurempi t-arvo saadaan
two_sample_t_test_oikealle = function(a_mean, a_sd, a_sample_size, b_mean, b_sd, b_sample_size) {
  if (a_sample_size != b_sample_size) {
    return ("fail")
  }
  jaettava = b_mean - a_mean
  jakaja = sqrt((a_sd^2 / a_sample_size) + (b_sd^2 / b_sample_size))
  t = jaettava / jakaja
  p_value = pt(t, df = a_sample_size + b_sample_size - 2, lower.tail = FALSE)
  formatted_p_value = sprintf("%.4f", p_value)
  return (formatted_p_value)
}

two_sample_t_test_oikealle(mean(kone1), sd(kone1), length(kone1), mean(kone2), sd(kone2), length(kone2))



# Tehtävä 6: Populaation hajonta on 0.7. Haluttiin testa,
# onko myy kasvanut arvosta 30.0. Tehtiin 30 kappaleen satunnaisotos,
# josta saatiin keskiarvo x_viiva = 30.3. Testaa 1% riskitasolla,
# kallistutko H0 myy = 30.0 vai H1 myy > 30.0 kannalle.
# Palauta myös p-arvo 5 desimaalin tarkkuudella.

# Koska perusjoukon keskihajonta tunnetaan ja otos on >= 30,
# käytän yksisuuntaista Z-testiä.

Z_test_right = function(otoskeskiarvo, populaatiokeskiarvo, keskihajonta, otoskoko) {
  jaettava = otoskeskiarvo - populaatiokeskiarvo
  jakaja = keskihajonta / sqrt(otoskoko)
  z = jaettava / jakaja
  p_value = pnorm(z, lower.tail=FALSE)
  formatted_p_value = sprintf("%.5f", p_value)
  return (formatted_p_value)
}

Z_test_right(30.3, 30.0, 0.7, 30)



# Tehtävä 7: Kauppias väittää porkkanapussien
# keskipainon olevan 500g. Pussien paino
# kuitenkin vaihtelee satunnaisesti.
# otettiin 16 pussin satunnaisotos, jonka
# otoskeskiarvoksi saatiin 490g ja
# otosvarianssiksi 145g.
# Määritä 0 hypoteesi ja testaa sitä
# 2-suuntaisella testauksella riskitasoilla
# 0.05 sekä 0.001. Laske P-arvo ja päättele
# ovatko nollahypoteesit sen perusteella voimassa.
# palauta testisuureen t-arvo 4 desimaalin tarkkuudella.

# teen nyt sellaisen 2-suuntaisen t-testerin, joka
# palauttaa sekä t:n arvon että p:n arvon

otoskeskiarvo = 490
keskiarvo = 500
sd = sqrt(145)
otoskoko = 16

two_sided_t_test_return_t = function(otoskeskiarvo, populaatiokeskiarvo, otoskeskihajonta, otoskoko) {
  jaettava = otoskeskiarvo - populaatiokeskiarvo
  jakaja = otoskeskihajonta / sqrt(otoskoko)
  t = jaettava / jakaja
  p_value = pt(abs(t), df = otoskoko -1, lower.tail = FALSE)
  formatted_p_value = sprintf("%.4f", p_value)
  formatted_t_value = sprintf("%.4f", t)
  return(paste("p-value: ", formatted_p_value, "t-value:", formatted_t_value))
}

two_sided_t_test_return_t(otoskeskiarvo, keskiarvo, sd, otoskoko)

# "p-value:  0.0023 t-value: -3.3218"
# tämän perusteella 0.05 p-testi meni läpi
# mutta 0.001 p-testi ei mennyt läpi