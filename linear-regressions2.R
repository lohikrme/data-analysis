# linear-regressions2
# 2. maaliskuu 2024

options(digits=10)
getwd()


# Tehtävä 1: Laske alla olevasta aineistosta
# regressiosuoran yhtälö. 
# Piirrä kuva, jossa on
# havaintopisteet ja regressiosuora.
# Mitkä ovat muodostuneen yhtälön
# vakiotermi ja kulmakerroin (regressiokerroin) 
# Anna nämä 5 desimaalin tarkkuudella.
# Lisäksi, mikä onm selitysaste R^2.
# Anna tämä 4 desimaalin tarkkuudella.
# Data on seuraava:
selittava = c(1.0, 1.5, 1.9, 2.4, 2.9, 3.4, 3.8, 4.3, 
              4.8, 5.3, 5.7, 6.2, 6.7, 7.2, 7.6, 8.1, 
              8.6, 9.1, 9.5, 10.0)
selitettava = c(1.4, 1.9, 2.2, 2.0, 2.0, 2.3, 2.4, 2.3,
                2.8, 2.6, 2.7, 3.0, 3.3, 3.2, 3.6, 3.7, 
                3.6, 4.6, 3.9, 4.1)

malli = lm(selitettava ~ selittava)
summary(malli)

vakiotermi = paste("vakiotermi:", sprintf("%.5f", coef(malli)[1]))
regressiokerroin = paste("regressiokerroin:", sprintf("%.5f", coef(malli)[2]))

vakiotermi
regressiokerroin

selitysaste = paste("selitysaste:", sprintf("%.4f", summary(malli)$r.squared))
selitysaste

# piirrä graafi (tarkempi ruudukko):

library(ggplot2)

data = data.frame(selittava, selitettava)

ggplot(data, aes(x=selittava, y=selitettava)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, color="darkgreen") +
  labs(x="X-arvot", y="Y-arvot") +
  ggtitle("Lineaarinen regressio")


# piirrä graafi (selkeämpi):

# Piirrä datapisteet
plot(selittava, selitettava, 
     main="Lineaarinen regressio", 
     xlab="X-arvot", ylab="Y-arvot")

# Piirrä regressiosuora
abline(malli, col="darkgreen")




# Tehtävä 2aaa: Luo matemaattinen malli,
# eli regressioyhtälö datasta Auton_hinta.xlsx

library(readxl)

auton_hinta_data = read_excel("data/Auton_hinta.xlsx")

# Tehtävä 2A: Käytä selittävänä muuttujana
# auton ikä. Anna vakiotermi ja kulmakerroin
# 3 desimaalin tarkkuudella. Anna selitysaste
# 4 desimaalin tarkkuudella.

malli1 = lm(Hinta_euroa ~ Ika_v, auton_hinta_data)
summary(malli1)


plot(auton_hinta_data$Ika_v, auton_hinta_data$Hinta_euroa, 
     main="Auton hinta ja ikä", 
     xlab="Ikä", ylab="Hinta",
     pch=9, col="blue")
abline(malli1, col="darkgreen", lwd=2)

vakiotermi1 = paste("vakiotermi:", sprintf("%.3f", coef(malli1)[1]))
regressiokerroin1 = paste("regressiokerroin:", sprintf("%.3f", coef(malli1)[2]))
selitysaste1 = paste("selitysaste:", sprintf("%.4f", summary(malli1)$r.squared))

vakiotermi1
regressiokerroin1
selitysaste1



# Tehtävä 2B: Käytä selittävänä muuttujana 
# ajettua matkaa.

auton_hinta_data = read_excel("data/Auton_hinta.xlsx")

malli = lm(Hinta_euroa ~ Ajettu_km, auton_hinta_data)
summary(malli)

plot(auton_hinta_data$Ajettu_km, auton_hinta_data$Hinta_euroa, 
     main="Auton hinta ja ajetut km", 
     xlab="Kilometrit", 
     ylab="Hinta",
     pch=9, col="blue")
abline(malli, col="darkgreen", lwd=2)

vakiotermi = paste("vakiotermi:", sprintf("%.3f", coef(malli)[1]))
regressiokerroin = paste("regressiokerroin:", sprintf("%.3f", coef(malli)[2]))
selitysaste = paste("selitysaste:", sprintf("%.4f", summary(malli)$r.squared))

vakiotermi
regressiokerroin
selitysaste


# Tehtävä 2C: Käytävä selittävänä muuttujana
# sekä ikää että ajettua matkaa molempia

auton_hinta_data = read_excel("data/Auton_hinta.xlsx")

malli = lm(Hinta_euroa ~ Ajettu_km + Ika_v, auton_hinta_data)
summary(malli)

vakiotermi = paste("vakiotermi:", sprintf("%.3f", coef(malli)[1]))
regressiokerroin_km = paste("regressiokerroin (km):", sprintf("%.3f", coef(malli)[2]))
regressiokerroin_years = paste("regressiokerroin (years):", sprintf("%.3f", coef(malli)[3]))
selitysaste = paste("selitysaste:", sprintf("%.4f", summary(malli)$r.squared))

vakiotermi
regressiokerroin_km
regressiokerroin_years
selitysaste



# Tehtävä 2D: Anna auton hinta edelliseen malliin perustuen,
# jos autolla on ajettu 80000km ja ikää on 3 vuotta.
# Anna vastaus euroina 2 desimaalin tarkkuudella.

# Ensin otan string arvojen sijaan numeeriset arvot edellisestä mallista
# Ja sitten sijoitan ne lineaariseen yhtälöön, jossa y = hinta

vakiotermi_numeerinen = coef(malli)[1]
regressiokerroin_km_numeerinen = coef(malli)[2]
regressiokerroin_years_numeerinen = coef(malli)[3]

hinta = paste("hinta:", sprintf("%.2f", 
                                vakiotermi_numeerinen 
                                + 80000 * regressiokerroin_km_numeerinen 
                                + 3 * regressiokerroin_years_numeerinen))
hinta





# Tehtävä 3: Lataa tiedosto ika_verenpaine.xlsx
# ja määritä matemaattinen malli, jossa
# ikä selittää verenpainetta.
# Anna vakiotermi ja kulmakerroin
# 2 desimaalin tarkkuudella.
# Anna selitysaste 4 desimaalin tarkkuudella.
# Piirrä graafi. 

verenpaine_data = read_excel("data/ika_verenpaine.xlsx")
summary(verenpaine_data)

# Vaikuttaisi olevan systolinen verenpaine

malli = lm(verenpaine ~ ika, verenpaine_data)
summary(malli)

plot(verenpaine_data$ika, verenpaine_data$verenpaine,
     main="Iän vaikutus verenpaineeseen",
     xlab="Ikä",
     ylab="Verenpaine",
     pch=8, col="blue")
abline(malli, col="darkgreen", lwd=2)

vakiotermi = coef(malli)[1]
regressiokerroin = coef(malli)[2]
selitysaste = summary(malli)$r.square

print(paste("Vakiotermi:", sprintf("%.2f", vakiotermi)))
print(paste("Regressiokerroin:", sprintf("%.2f", regressiokerroin)))
print(paste("Selitysaste:", sprintf("%.4f", selitysaste)))



# Tehtävä 4: Määritä henkilön palkkaa
# kuvaava regressioyhtälö niin, että 
# mallin merkitsevyystasoksi riittää 5%.
# Datan saa tiedostosta palkka.xlsx tiedosto.

palkkadata = read_excel("data/palkka.xlsx")
summary(palkkadata)

# eli, datassa on vuosipalkka, sekä 3 selittävää muuttujaa
# jotka ovat opiskeluvuodet, työkokemus_vuodet, keskimäärin_työtunteja_viikossa


# Tehtävä 4A: Muodosta malli, jossa on mukana
# kaikki palkkaa selittävät muuttujat
# Anna vakiotermi ja regressiokertoimet
# 3 desimaalin tarkkuudella.

# Siispä otan malliin mukaan nyt aluksi kaikki 3 selittävää muuttujaa
malli = lm(vuosipalkka ~ opiskeluvuodet + työkokemus_vuodet + keskimäärin_työtunteja_viikossa, palkkadata)
summary(malli)

# seuraavaksi otan talteen vakiotermi ja regressiokertoimet
vakiotermi = coef(malli)[1]
k_opiskeluvuodet = coef(malli)[2]
k_työkokemus = coef(malli)[3]
k_työtunnit = coef(malli)[4]

vakiotermi
k_opiskeluvuodet
k_työkokemus
k_työtunnit

print(paste("vakiotermi:", sprintf("%.3f", vakiotermi)))
print(paste("k_opiskeluvuodet:", sprintf("%.3f", k_opiskeluvuodet)))
print(paste("k_työkokemus:", sprintf("%.3f", k_työkokemus)))
print(paste("k_työtunnit:", sprintf("%.3f", k_työtunnit)))


# Tehtävä 4B: Tällä kertaa pudota pois vähiten merkitsevä muuttuja.
# Eli valitse mukaan seuraavaan regressioyhtälö-malliin
# vain kaksi merkitsevintä selittävää muuttujaa.
# Kirjoita vakiotermi ja regressiokertoimet x1 ja x2
# 3 desimaalin tarkkuudella.

# jotta saan lisäkäsitystä datasta, teen ensin 
# graafin, josta käy ilmi, onko eri 
# muuttujien välillä multikollineaarisuutta,
# ja miten korrelaatiot ovat jakautuneet:
library(ggplot2)
library(GGally)

ggpairs(palkkadata)
cor(palkkadata)

# Laajan tarkastelun perusteella vaikuttaa siltä,
# että opiskeluvuodet ja viikkotyötunnit korreloivat vahvimmin
# kirjaan kuitenkin vielä ylös kaikki eri korrelaatiot

opiskeluvuodet_korrelaatio = cor(palkkadata$opiskeluvuodet, palkkadata$vuosipalkka)
työkokemusvuodet_korrelaatio = cor(palkkadata$työkokemus_vuodet, palkkadata$vuosipalkka)
viikkotyötunnit_korrelaatio = cor(palkkadata$keskimäärin_työtunteja_viikossa, palkkadata$vuosipalkka)

opiskeluvuodet_korrelaatio
työkokemusvuodet_korrelaatio
viikkotyötunnit_korrelaatio

# Eli kuten näkyy, 
# opiskeluvuodet 0.75,
# viikkotyötunnit 0.68,
# työkokemusvuodet 0.42
# jolloin 2 korreloivinta on
# opiskeluvuodet ja viikkotyötunnit

# Seuraavaksi muodostan uuden mallin, 
# jossa on vain opiskeluvuodet ja viikkotyötunnit:

malli2 = lm(vuosipalkka ~ opiskeluvuodet + keskimäärin_työtunteja_viikossa, palkkadata)
summary(malli2)

# En saavuttanut vaadittua p-arvoa 0.05. Jonka seurauksena koitan muita malleja:

malli3 = lm(vuosipalkka ~ opiskeluvuodet + työkokemus_vuodet, palkkadata)
summary(malli3)

# Tämä malli p-arvo on 0.005, eli 10 kertaa parempi kuin vaadittu.

malli4 = lm(vuosipalkka ~ työkokemus_vuodet + keskimäärin_työtunteja_viikossa, palkkadata)
summary(malli4)

# Tämä malli antoi p-arvon 0.1. Eli vielä huonompi kuin malli2.


# Siispä, tarkasteltuani eri malleja, päädyin tulokseen, että
# malli3, jossa opiskeluvuodet ja työkokemusvuodet selittävät palkkaa parhaiten

uusi_vakiotermi = coef(malli3)[1]
uusi_k_opiskeluvuodet = coef(malli3)[2]
uusi_k_työkokemus = coef(malli3)[3]

uusi_vakiotermi
uusi_k_opiskeluvuodet
uusi_k_työkokemus

print(paste("vakiotermi:", sprintf("%.3f", uusi_vakiotermi)))
print(paste("uusi_k_opiskeluvuodet:", sprintf("%.3f", uusi_k_opiskeluvuodet)))
print(paste("uusi_k_työkokemus:", sprintf("%.3f", uusi_k_työkokemus)))

# Tehtävä 4C: Arvioi vielä lopuksi,
# paljonko henkilö saa palkkaa,
# jos taustalla on 4 vuoden opinnot
# (AMK insinööri), ja lisäksi
# työkokemusta on 2 vuotta, ja hän
# työskentelee keskim. 40 tuntia viikossa.
# anna vastaus 2 desimaalin tarkkuudella.

# Eli nyt käytän ekaa mallia arvioon.
# otan aiemmat printit nähdäkseni että muuttujat kunnossa

print(paste("vakiotermi:", sprintf("%.3f", vakiotermi)))
print(paste("k_opiskeluvuodet:", sprintf("%.3f", k_opiskeluvuodet)))
print(paste("k_työkokemus:", sprintf("%.3f", k_työkokemus)))
print(paste("k_työtunnit:", sprintf("%.3f", k_työtunnit)))

insinöörin_palkka = vakiotermi + 4*k_opiskeluvuodet + 2*k_työkokemus + 40*k_työtunnit

insinöörin_palkka

print(paste("insinöörin palkka:", sprintf("%.2f", insinöörin_palkka)))

# nopeasti katson alkup. datasta mitä arvoja siellä oli vertailun vuoksi
summary(palkkadata)

# summaryn perusteella mediaanisti 
# 4 vuotta opintoja, 3 vuotta työkokemusta, 50h viikkotyötunteja
# ja summaryssä tämä johtaa 77000e
# eli saatu tulos 71054.74 on aika realistinen
# kun otetaan huomioon vähän pienempi työkokemus ja vähemmän viikkotyötunteja