# linear-regressions1
# 29.2.2024

options(digits=10)


# Monen selittävän muuttujan lineaariregressio:
# On valittava sellaisia selittäviä muuttujia, 
# jotka eivät korreloi keskenään (multikollineaarisuus).
# Tämä selviää tarkastelemalla muuttujien välisiä
# korrelaatiokertoimia (VIF-arvo ei saa olla yli 5).
# Muuttujien tulee olal välimatka tai suhdeasteikollisia,
# tai ns. 'dummy'-muuttuja, eli esim. 0 = päällä, 1 = kiinni.
# Kulmakertoimen sijaan puhutaan regressiokertoimista,
# koska viiva piirretään n-ulotteiseen avaruuteen.

# F-testillä testataan, onko malli tilastollisesti merkitsevä.
# F on vähän samantyyppinen jakauma kuin T:kin on.
# H0: Kaikkien selittävien muuttujien regressiokerroin on 0.
# H1: Ainakin yksi regressiokertoimista on nollasta poikkeava.
# Jos F-testin p-arvo on esim. alle 0.05, niin H1 astuu voimaan.

# Yksittäisen selittävän muuttujan sopivuus voidaan testata T-testillä.

# Mallin sopivuutta tutkitaan myös jäännöstermien avulla.
# Jäännöstermi on mittaustulosten ja ennusteiden erotus.
# Se kuvaa sitä osaa y-muuttujan vaihtelusta, jota
# regressiomalli ei selitä. 
# Jäännöskuvio piirretään niin, että y-akselilla on jäännöstermi
# ja vaaka-akselilla selittävä muuttuja. 
# Ideaalisti jäännöskuvion pisteet ovat jakautuneet
# Säännöllisesti ja ilman säännönmukaisuutta.
# Jäännöskuviosta voidaan tunnistaa poikkeavat arvot.

# Ennustevyöhyke on alue, joka laajentaa aluetta 
# regressiosuoran ympärille siten, että
# esim. 95% tod näk tutkittavan muuttujan arvot
# pysyvät tietyllä "vyöhykkeellä". Tällaista näkee
# esim. sääennustuksia katsoessa, kun 
# ennuste-viivan ympärillä on "hajonta-alue"

# Regressiosuoran laskemiseen käytetään useimmiten valmiita komentoja.
# Mutta sen voi laskea pienimmän neliösumman menetelmällä:

# Yhden selittävän muuttujan:
X = matrix(c(4, 6, 7, 11, 14, 21, 32, rep(1, 7)), ncol=2)
Y = matrix(c(15, 14, 12, 11, 10, 8, 6), ncol=1)
solve( t(X) %*% X ) %*% t(X) %*% Y

# tulokseksi tulee y = -0.307x + 15.027
# -0.307 tulee X-matriisin ekasta vektorista, 
# ja 15.027 tulee X-matriisisin c(1,1,1,1,1,1,1) vektorista

# Kahden selittävän muuttujan:
selittava1 = c(4, 6, 7, 11, 14, 21, 32)
selittava2 = c(55, 62, 41, 63, 75, 70)
selitettava = c(15, 14, 12, 11, 10, 8, 6)
X = matrix(c(selittava1, selittava2, rep(1, 7)), ncol=3)
Y = matrix(selitettava, ncol=1)
solve( t(X) %*% X ) %*% t(X) %*% Y

# tulokseksi tulee y = -0.612x1 + 0.189x2 + 6.477



library(ggplot2)

# Tehtävä 0A: Laske kulmakerroin, joka kertoo,
# kuinka hyvin matematiikan arvosanat selittävät
# fysiikan arvosanoja... Piirrä lineaarinen regressio.
# laske myös pearsonin korrelaatiokerroin sekä selitysaste.
math_grades = c(5, 1, 2, 4, 2)
physics_grades = c(3, 2, 3, 4, 2)

# kulmakerroin saadaan kaavasta
# (HUOM. kaavassa x = selittävä ja y = ennustettava): 
# SIGMA { (x - mean(x)) * (y - mean(y)) } / 
# SIGMA { (x - mean(x))^2 }

# luo ensin dataframe
df = data.frame(math_grades, physics_grades)
df

# laske slope matemaattisesti
slope = sum((df$math_grades - mean(df$math_grades)) * (df$physics_grades - mean(df$physics_grades))) / 
  sum((df$math_grades - mean(df$math_grades))^2)
slope


# Piirretään hajontakaavio
ggplot(df, aes(x=math_grades, y=physics_grades)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE, color="red") +
  labs(x="Matematiikan arvosanat", y="Fysiikan arvosanat") +
  ggtitle("Matematiikan ja fysiikan arvosanojen suhde")


# graafi ja slope arvo 0.35 näyttävät täsmäävän

# Seuraavaksi lasken korrelaatiokertoimen (r) 
# sekä selitysasteen (joka on yksinkertaisesti r^2)
# r saadaan kaavasta:
# r = sum( (x - mean(x)) * (y - mean(y)) ) 
# / sqrt(sum( (x - mean(x))^2 ) * sum( (y - mean(y))^2 ) )
r = sum( (df$math_grades - mean(df$math_grades)) *  (df$physics_grades - mean(df$physics_grades)) ) /
  sqrt(sum( (df$math_grades - mean(df$math_grades))^2 ) * sum( (df$physics_grades - mean(df$physics_grades))^2 ) )
r

korrelaatiokerroin = r
selitysaste = r^2

korrelaatiokerroin
selitysaste

# korrelaatiokertoimeksi tuli 0.691, selitysasteeksi 0.478



# Tehtävä 0B: Krokotiilien keskimääräistä pituutta tutkittiin, 
# ja haluttiin selvittää, vaikuttaako asuinmaan lämpötila pituuteen.
# Laske laske kulmakerroin, piirrä lineaariregressio
# sekä laske pearsonin korrelaatiokerroin ja selitysaste.
# Dataksi saatiin seuraavat:
crocodile_lengths = c(3.5, 3.7, 4.0, 4.2, 4.5)
temperatures = c(20, 23, 25, 28, 30)

df2 = data.frame(crocodile_lengths, temperatures)
df2

# kulmakerroin saadaan kaavasta
# (HUOM. kaavassa x = selittävä ja y = ennustettava): 
# SIGMA { (x - mean(x)) * (y - mean(y)) } / 
# SIGMA { (x - mean(x))^2 }

# huomaa laittaa x:n paikalle selittävä = lämpötila
# laske slope matemaattisesti
slope = sum( (df2$temperatures - mean(df2$temperatures) ) * (df2$crocodile_lengths - mean(df2$crocodile_lengths)) ) /
  sum( (df2$temperatures - mean(df2$temperatures))^2 ) 
slope

# kulmakertoimeksi tuli noin 0.1

# piirrä graafi
ggplot(df2, aes(x=df2$temperatures, y=df2$crocodile_lengths)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE, color="darkgreen") +
  labs(x="Lämpötilat maassa", y="Krokotiilien keskipituudet") +
  ggtitle("Elinympäristön lämpötilan ja krokotiilin pituuden suhde")

# grafiikka näytätä oikealta!


# r saadaan kaavasta:
# r = sum( (x - mean(x)) * (y - mean(y)) ) 
# / sqrt(sum( (x - mean(x))^2 ) * sum( (y - mean(y))^2 ) )

korrelaatiokerroin2 = sum( (df2$temperatures - mean(df2$temperatures)) * (df2$crocodile_lengths - mean(df2$crocodile_lengths)) ) /
  sqrt(sum( (df2$temperatures - mean(df2$temperatures))^2) * sum( (df2$crocodile_lengths - mean(df2$crocodile_lengths))^2 ) )
selitysaste2 = korrelaatiokerroin2 ^ 2

korrelaatiokerroin2
selitysaste2

# tuli korrelaatiokertoimeksi 0.99, ja selitysasteeksi 0.98



# Tehtävä 0C: Muodostetaan seuraavan aineiston avulla
# regressiomalli kesämökin myyntihinnalle tuhansina euroina (y),
# jossa selittävinä muuttujina ovat 
# rantaviivan pituus ja mökin pinta-ala.
# Data koostuu siis 3 vektorista
# jotka löytyvät tiedostosta data/Mokki.xlsx

library(readxl)
cottageData = read_excel("data/Mokki.xlsx")

# valitaan halutut muuttujat, suodatetaan homeen määrä pois
input = cottageData[ , c("myyntihinta_tuhattaeuroa", "rantaviiva_m", "mokin_pinta_ala_m2")]

# muodostetaan malli "linear model" eli lm:
# lm eka parametri on yhtälö, siihen laitetaan muotoa
# ennustettava ~ ennustava1 + ennustava2
malli = lm(myyntihinta_tuhattaeuroa ~ rantaviiva_m + mokin_pinta_ala_m2, data = input)
summary(malli)

# summary tuottaa luettavan taulukon tuloksista
# tärkeintä tämän tehtävän kannalta oli
# Coefficients taulukko, jossa on 
# estimate (x1, x2, vakiotermi arvot)
# lisäksi muita arvota kuten keskivirhe, t-arvo, jne.
# multiple R squared = selitysaste. ja p-arvo on koko testin p.

# tulokseksi saatiin y = -75.210 + 1.9149x1 + 2.5545x2
# Eli 1 metri lisää rantaviivaa lisää hintaa noin 1915e
# Ja 1 neliömetri lisää pinta-alaa lisää hintaa noin 2555e

# yhtälön voi myös tulostaa ohjelmallisesti:
vakiotermi = round(coef(malli)[1], 3)
rantaviiva_selittaja1 = round(coef(malli)[2], 3)
pinta_ala_selittaja2 = round(coef(malli)[3], 3)

yhtalo = paste0("y = ", vakiotermi, " + ", rantaviiva_selittava1, "X1 + ", pinta_ala_selittava2, "X2")
yhtalo




# Tehtävä 0D: Etsi multikollineaarisuutta uscrime datasta.
# Data on vuosina 1959-1960 kerättu USA:ssa.
# Sarakkeita on 13 erilaista.

library(readxl)
uscrime = read_excel("data/uscrime.xlsx")

# piste tarkoittaa että valitaan kaikki sarakkeet
malli = lm(R ~., data = uscrime)
summary(malli)

# Koska monet sarakkeet ovat summaryssä 0 tähteä
# ja niillä on T-arvo lähellä 0:aa, voidaan tehdä arvaus,
# Että muuttujien välillä olisi multikollineaarisuutta

# Tehdään tätä arviota varten taulukko, jossa 
# näkyy korrelaatiokertoimia
# muista laittaa kirjastot päälle, ja mikäli saat virheilmoituksia
# mennä Tools -> asenna paketti
library(ggplot2)
library(GGally)

# Nyt piirretään graafi, jossa näkyy kaikkien eri muuttujien
# väliset korrelaatiot. Lisäksi katsotaan sen jälkeen
# Listaus korrelaatioista, joista käy periaatteessa ilmi
# Sama asia mutta vähemmän graafisesti

ggpairs(uscrime)
cor(uscrime)

#ggpairs piirtämästä graafista näkyy, että 
# Ex0 ja Ex1 välillä on äärimmäisen vahva korrelaatio 0.994
# ja siinä kohtaa palloista myös piirtyy melko suora viiva
# Siksi toinen näistä muuttujista pitää ottaa pois datasta
# Ja sitten ajaa uudelleen lineaarinen regressio

malli2 = lm(R ~. -Ex1, data=uscrime)
summary(malli2)

# Nyt Ex0 tuli kolmen tähden selittäjä, Ed sekä X kahden tähden,
# Ja Age sekä U2 yhden tähden
# Eli nyt tehdään vielä uusi malli, johon valitaan mukaan
# vain ne sarakkeet/muuttujat joilla on tilastollista merkitsevyyttä

final_malli = lm(R ~ Age + Ed + Ex0 + U2 + X, data = uscrime)
summary(final_malli)

# Tästä muodostuisi yhtälö:
vakiotermi = round(coef(final_malli)[1], 3)
Age_selittaja1 = round(coef(final_malli)[2], 3)
Ed_selittaja2 = round(coef(final_malli)[3], 3)
Ex0_selittaja3 = round(coef(final_malli)[4], 3)
U2_selittaja4 = round(coef(final_malli)[5], 3)
X_selittaja5 = round(coef(final_malli)[6], 3)

yhtalo = paste0(vakiotermi, " + ", Age_selittaja1, "X1 + ", Ed_selittaja2, "X2 + ", Ex0_selittaja3, "X3 + ", U2_selittaja4, "X4 + ", X_selittaja5, "X5")
yhtalo



















