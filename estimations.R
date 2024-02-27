# estimations
# 13.2.2024

# Terminologiasta lyhyesti: 
# Keskivirhe(SE) on keskihajonta(sd) / sqrt(otoskoko)
# Luottamusväli on ääriarvosta ääriarvoon
# Kun taas margin_of_error(E) on puolet luottamusvälistä
# eli jos luottamusväli = 1.5mm, niin silloin E=0.75mm
# Funktioissa kutsun E:tä usein "luottamusvälin puolikkaaksi"
# englanniksi luottamusvälin_puolikas on siis margin_of_error.

options(digits = 10)

getwd()
setwd("C:\\Users\\Parrot\\data-analyysi1\\visualisointi\\Rstudio-graphs\\")
getwd()


# Tehtävä 1A: Laske keskiarvo ja hajonta, 
# anna vastaus 2 desimaalin tarkkuudella:

dataset1 = c(20.0, 18.6, 18.9, 19.3, 19.5, 19.2, 20.1, 20.2, 19.4, 18.8, 18.2, 
             19.4, 19.6, 20.1, 19.3, 18.7, 20.3, 19.5, 19.6, 19.7, 20.5, 18.6, 
             18.9, 19.3, 19.5, 19.2, 20.1, 20.2, 19.4, 18.8, 18.2, 19.4, 19.6, 
             20.1, 19.3, 18.7, 20.3, 19.5, 19.6, 19.7)

mean1 = round(mean(dataset1), 2)
mean1

sd1 = round(sd(dataset1), 2)
sd1

sample_size1 = length(dataset1)
sample_size1

# Tehtävä 1B: laske 99% luottamusrajat samalle datasetille
# koska otoskoko on yli 30, mutta populaation keskihajonta on tuntematon
# käytetään studentin t-jakaumaa
# HUOM: R:ssä qt-funktio on oletusarvona 1-suuntainen
# eli jos kirjoittaa qt(0.975, df=9) tulee 2.26
# joka vastaa 0.025 yksisuuntaisessa taulukossa
# kun df=9 ja siten otoskoko = 9+1
# jos tehdään 2-suuntainen testi, pitäisi jakaa häntä 2:lla

luottamusvali_funktio = function (keskiarvo, keskihajonta, luottamusprosentti, otoskoko) {
  merkitsevyystaso = 1 - (1 - luottamusprosentti/100) / 2
  T =  qt(merkitsevyystaso, df = otoskoko - 1)
  keskivirhe = keskihajonta / sqrt(otoskoko) # keskihajonta pitää sisällään "tutkittavan yksikön"
  luottamusvalin_puolikas = T * keskivirhe
  # mene luottamusvälin puolikkaan verran vasemmalle ja oikealle keskiarvosta
  pienin = round(keskiarvo - luottamusvalin_puolikas, 2)
  suurin = round(keskiarvo + luottamusvalin_puolikas, 2)
  formatted_pienin = sprintf("%.2f", pienin)
  formatted_suurin = sprintf("%.2f", suurin)
  return (paste("pienin:", formatted_pienin, "... suurin:", formatted_suurin))
}

luottamusvali_funktio(mean1, sd1, 99, sample_size1)




# Tehtävä 2: kyselytutkimuksessa kysytään 
# 2300 ihmiseltä mielipide
# 628 kannatti poliitikon ajamaa asiaa
# poliitikko sanoo, että 30 prosenttia kansasta kannattaa
# määritä otoksesta 95 ja 99% luottamusvälit, ja sen perusteella
# määrittele, pitääkö poliitikon väite paikkansa

# käytän Z-taulukkoa, koska puhutaan isoista otoksista
# ja koska kyse on 2 suuntaisesta, 
# niin 95% tarkoittaa 2.5% häntiä
#ja 99% tarkoittaa 0.5% häntiä
# eli R-komentoina qnorm(0.975) ja qnorm(0.995)

# keskihajonta lasketaan binomijakauman mukaisesti 
# yhden tilastoyksikön eli yhden ihmisen sd on sqrt(p*q)
# mutta nyt on otos, mitä isompi otos, sitä pienempi sd
# sd kapenee jakamalla sqrt(n)
# eli lopullinen lasku on:
# sqrt(p*q) / sqrt(n)


sample_size = 2300
support = 628/2300


luottamusvali_prosentit = function (kannatus, luottamusprosentti, otoskoko) {
  sd = sqrt(kannatus*(1-kannatus))
  merkitsevyystaso = 1-(1-luottamusprosentti/100)/2
  keskivirhe = sd / sqrt(otoskoko)
  Z = qnorm(merkitsevyystaso)
  luottamusvalin_puolikas = keskivirhe * Z
  # laske luottamusrajat ja palauta se
  pienin = round(kannatus - luottamusvalin_puolikas, 3)
  suurin = round(kannatus + luottamusvalin_puolikas, 3)
  formatted_pienin = sprintf("%.3f", pienin)
  formatted_suurin = sprintf("%.3f", suurin)
  return (paste("pienin:", formatted_pienin, "... suurin:", formatted_suurin))
}

luottamusvali_prosentit(support, 95, sample_size)
luottamusvali_prosentit(support, 99, sample_size)



# Tehtävä 3: tehdas valmistaa ruuveja. ruuvien paino
# vaihtelee satunnaisesti normaalijakautuneesti
# otoskeskiarvo 18g. oletetaan (epärealistisesti)
# että normaalijakauman varianssi 0.16 g^2.
# määritä 95% luottamusvälit painon odotusarvolle
# jos otoskoko on a) 23 b) 2300

# koska 95% luottamusväli, tiedän, että keskihajontojen määrä
# vastaa sitä. Koska normaalijakauman varianssi ja 
# siten myös keskihajonta ovat tunnettuja
# Pienemmässä otos on alle 30, eli käytän T-testiä
# isommassa otos on yli 30 ja käytän Z-testiä

# keskivirhe eli keskihajontojen pituus saadaan
# varianssista. pitää vain ottaa huomioon otoskoko

# luottamusprosentti on 95%, eli 2.5% per häntä
# Muuta varianssi sd:ksi
luottamusprosentti = 95
sd = sqrt(0.16)
average = 18
otoskoko1 = 23
otoskoko2 = 2300

# pienemmän otoksen lasken T-jakaumalla koska alle 30 otos:

conf_T = function (sd, keskiarvo, luottamusprosentti, otoskoko) {
  luottamusdesimaali = luottamusprosentti / 100
  kaksisuuntainen_luottamusvali = luottamusdesimaali + (1-luottamusdesimaali)/2
  print(kaksisuuntainen_luottamusvali)
  standard_error = sd/sqrt(otoskoko)
  T = qt(kaksisuuntainen_luottamusvali, otoskoko - 1)
  pienin = signif(keskiarvo - T * standard_error, 4)
  suurin = signif(keskiarvo + T * standard_error, 4)
  return (paste("pienin:", pienin, "... suurin:", suurin))
}


# isomman otoksen lasken z-jakaumalla:

conf_Z = function (sd, keskiarvo, luottamusprosentti, otoskoko) {
  luottamusdesimaali = luottamusprosentti / 100
  kaksisuuntainen_luottamusvali = luottamusdesimaali + (1-luottamusdesimaali)/2
  standard_error = sd/sqrt(otoskoko)
  Z = qnorm(kaksisuuntainen_luottamusvali)
  pienin = signif(keskiarvo - Z * standard_error, 4)
  suurin = signif(keskiarvo + Z * standard_error, 4)
  return (paste("pienin:", pienin, "... suurin:", suurin))
}

# tulokset:
conf_T(sd, average, 95, otoskoko1)
conf_Z(sd, average, 95, otoskoko2)



# Tehtävä 4A: normaalisti jakautuneesta suureesta 
# otettiin 30kpl otos, jonka
# otoskeskiarvo 12.4 ja otoshajonta 1.4
# Laske 99% luottamusväli perusjoukon keskiarvolle.

# Koska ei tunneta pop keskihajontaa, käytän T-testiä


sample_size = 30
sd = 1.4
standard_error = sd/sqrt(sample_size)
mean = 12.4
T = qt(0.995, df = sample_size - 1)
lower = mean - T * standard_error
higher = mean + T * standard_error
formatted_lower = sprintf("%.2f", lower)
formatted_higher = sprintf("%.2f", higher)
print(paste("lower", formatted_lower, "higher", formatted_higher))


# Tehtävä 4b: Laske, kuinka iso otoskoko pitäisi olla, jotta
# luottamusväli 99.9% olisi keskiarvo +- 0.1mm
# tehtävässä sanotaan, että perusjoukon hajonta on 1.4mm
# mikä mahdollistaa laskea, otoskeskihajonnalla ei voisi laskea

# kaavana käytän: E = Z * pikku_sigma/sqrt(n).
# E tarkoittaa margin_of_error pituutta
# eli luottamusvälin puolikkaan pituutta
# jos luottamusväli olisi 10, niin E olisi 5.
# tehtävässä haluamme, että E = 0.1mm
# saamme tämän helposti laskettua kaavalla:
# n = ((Z * pikku_sigma)/E)^2


Z = qnorm(0.9995)
pikku_sigma = 1.4
n = ceiling(((Z * pikku_sigma)/0.1)^2)
n


# Tehtävä 5: Galluppiin osallistui 3000 haastateltavaa
# 2115 vastasi kyllä
# laske kyllä-vastanneiden prosenttiosuuden 
# 95% luottamusrajat eli virhemarginaali

# yhden ihmisen p on 2115/3000 ja q on 1 - 2115/3000
# josta siis tulee keskihajonta sqrt(p*q)
# koska otoksen koko on 3000, otoskeskihajonnaksi
# tulee sqrt(p*q) / sqrt(3000)
# sen jälkeen käytetään Z-taulukkoa 95%


luottamusvali_funktio = function (kannatus, luottamusprosentti, otoskoko) {
  p = kannatus/otoskoko
  q = 1 - p
  standard_error = sqrt(p*q) / sqrt(otoskoko)
  tail = (1-luottamusprosentti/100)/2
  Z = qnorm(1-tail)
  luottamusvalin_puolikas = Z * standard_error
  mean = p
  lower = round((mean - luottamusvalin_puolikas) * 100, 1)
  higher = round((mean + luottamusvalin_puolikas) * 100, 1)
  return (c(lower, higher))
  
}

luottamusvali_funktio(2115, 95, 3000)



# Tehtävä 6: estimoitaessa normaalisti jakautuneen 
# N(μ ; 2.2) satunnaisuureen odotusarvoa μ, otetaan n kpl otos.
# Kuinka suuri otos on valittava, että μ:n
# 99%:n luottamusvälin pituus ei ole suurempi kuin 1.5

# Todettakoon ensiksi, että N(μ ; 2.2) tarkoittaa siis
# normaalijakautunutta dataa, jossa odotusarvo on tuntematon
# ja jossa keskihajonta pikku_sigma on 2.2

# Tehtävässä kysytään luottamusvälin pituutta.
# Se saadaan helposti laskettua kaavasta
# E = Z * pikku_sigma/sqrt(n)
# jossa E tarkoittaa luottamusvälin_puolikkaan pituutta.
# Koska tehtävänannossa sanotaan 
# luottamusvälin olevan on 1.5mm, 
# puolikas on tietty 0.75mm.
# Eli E = 1.5/2

# Tämän jälkeen sijoitetaan E kaavaan:
# n = ((Z * pikku_sigma)/E)^2 
# joka on johdettu samasta kaavasta ja kertoo tarvittavan otoskoon


Z = qnorm(0.995) 
sd = 2.2
E = 1.5 / 2 
n = ceiling((Z * sd / E)^2) 
print(paste("Sample size:", n))




# Tehtävä 7: Heikki mittasi lepopulssiaan ja sai seuraavan otoksen:
# 67, 62, 58, 74, 65, 66, 63
# määritä 95% luottamusväli Heikin 
# keskimääräiselle lepopulssille
# anna vastaus yhden desimaalin tarkkuudella

dataset = c(67, 62, 58, 74, 65, 66, 63)
mean = mean(dataset)
sample_size = length(dataset)
sd = sd(dataset)
standard_error = sd / sqrt(sample_size)
T = qt(0.975, df = sample_size - 1)

lower = round(mean - T * standard_error, 1)
higher = round(mean + T * standard_error, 1)

print(paste("Lower:", lower, "Higher:", higher))



# Tehtävä 8: Internetgallupissa 1500 suomalaiselta kysyttiin, 
# onko heillä ilmalämpöpumppu. 52.9% sanoi omistavansa.
# Määritä 95% luottamusväli ilmalämpöpumpun omistavien
# suhteelliselle osuudelle. 
# Anna vastaus prosentteina 1 desimaalin tarkkuudella

sample_size = 1500
owners = 0.529
q = 1 - owners
Z = qnorm(0.975)
sd = sqrt(owners * q)
standard_error = sd / sqrt(sample_size)
mean = owners

lower = round((mean - Z * standard_error)*100, 1)
higher = round((mean + Z * standard_error)*100, 1)

print(paste("Lower:", lower, "Higher:", higher))



# short test about R's 'sd()' function
testdata = c(20.0, 18.6, 18.9, 19.3, 19.5, 19.2, 20.1, 20.2, 19.4, 18.8, 18.2, 
             19.4, 19.6, 20.1, 19.3, 18.7, 20.3, 19.5, 19.6, 19.7, 20.5, 18.6, 
             18.9, 19.3, 19.5, 19.2, 20.1, 20.2, 19.4, 18.8, 18.2, 19.4, 19.6, 
             20.1, 19.3, 18.7, 20.3, 19.5, 19.6, 19.7)

#----------------------------------------------------
# Lopuksi: testailin vähän eroja miten sd:tä voi laskea:

# divide with n-1
mean = mean(testdata)
sample_size = length(testdata)
variance = sum((testdata - mean)^2 / (sample_size - 1))
standard_deviation = sqrt(variance)
standard_deviation

# divide with n
variance2 = sum((testdata - mean)^2 / (sample_size))
standard_deviation2 = sqrt(variance2)
standard_deviation2

# use inbuilt sd function
sd(testdata)


