# more statistical tests 
# 27. helmikuu 2024

# Johdatusta riippuvaan t-testiin,
# Chi squared yhteensopivuustestiin sekä
# Chi squared riippumattomuustestiin

# Chi squared käytetään luokitteluasteikon muuttujiin
# Jos halutaan tutkia jatkuvaa riippumattomuutta
# tehdään regressioanalyysi

# yhteensopivuustestissä vapausaste = luokkien määrä -1
# riippuvuustestissä vapausaste = (s-1)(r-1)
# kun s = sarakkeiden lkm, r = rivien lkm

options(digits=10)

#-------------------------------------
# Tehtävä 0A: Lääkkeen tehoa testataan potilailla.
# Eli ensiksi mitataan jotakin arvoa (korkeampi parempi)
# ennen lääkettä. Sen jälkeen tuntemattoman ajan jälkeen,
# kun lääke on ehtinyt vaikuttaa. 
# Tarkoitus on testata 1% riskitasolla,
# onko uusi lääke auttanut potilaita, vai ei. 
# Datana on seuraava:

ennen = c(1.87, 1.81, 2.24, 1.73, 2.05,
          1.85, 1.59, 2.16, 2.15, 1.74,
          2.01, 2.00, 1.90, 1.78, 1.81,
          2.19, 1.99, 2.30, 1.93, 1.67)

jalkeen = c(1.65, 2.00, 2.39, 1.62, 1.70,
            1.66, 2.75, 2.41, 2.47, 2.05,
            2.14, 2.26, 2.51, 1.89, 1.96,
            2.30, 2.00, 2.85, 2.41, 1.93)

# Eli, ensiksi muotoilen hypoteesit:
# H0: jalkeen = ennen
# H1: jalkeen > ennen

# Seuraavaksi käytän valmista funktiota:
# funktio katsoo hypoteesin vasemmalta oikealle, eri järjestyksessä pitää olla tarkka.
# Nyt kun on jalkeen, ennen, greater, se tarkoittaa, onko eka data suuurempi kuin toka data.
t.test(jalkeen, ennen, alternative="greater", conf.level = 0.99, var.equal = TRUE, paired = TRUE)

# saatu tulos: "t = 2.8017283, df = 19, p-value = 0.005690368"


#-------------------------------------
# Tehtävä 0B: Erästä pelaajaa epäillään vilpistä.
# Yhtä hänen nopistaan heitettiin 600 kertaa. 
# Laske käyttäen chi square yhteensopivuustestiä, 
# ovatko havaitut frekvenssit peräisin normaalista nopasta,
# kun riskitaso on 0.1%. Entä jos riskitaso on 10%?
# Data on seuraava (1:stä 6:een jaettu frekvensseihin)
# saadut tulokset nopanheitosta)
nopanheitot = c(68, 76, 73, 105, 127, 151)
tn_pitaisi_olla = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)

# muotoilen ensin hypoteesit:
# H0: Jakauma on tasainen
# H1: Jakauma ei ole tasainen

vastaus = chisq.test(nopanheitot, p = tn_pitaisi_olla)
format(vastaus$p.value, scientific = FALSE)

# saatu tulos: "0.00000000005455878998"
# on pienempi kuin 0.1, eli 
# 10% riskitasolla noppa on painotettu.
# myös 0.1% riskitasolla noppa on painotettu,
# koska saatu tulos on pienempi kuin 0.001!



#-------------------------------------
# Tehtävä 0C: Testaa 0.1% riskillä, onko lääkkeellä 
# vaikutusta paranemiseen vai ei. 
# Koe on seuraava:
# Ryhmälle A oikea lääke, 
# Ryhmälle B lumelääke.
# Kysymys kuuluu, 
# riippuiko paraneminen oikean lääkkeen syömisesti vai ei?
# Datana toimii seuraava (luokiteltu paranemisajan mukaan): 
# Ryhmä, 1-2vrk, 3-4vrk, 5-6vrk, 7-10vrk, yli 10vrk
A = c(52, 46, 25, 15, 12)
B = c(80, 95, 123, 65, 37)

# Ensin määrittelen hypoteesit:
# H0: paraneminen ei riipu mitenkään lääkkeestä.
# H1: paraneminen riippuu (molempiin suuntiin) lääkkeen syömisestä

# Riskitaso 0.1 ! (0.001)

# Seuraavaksi yhdistetään vektorit matriisiksi:
matriisi = matrix(c(A, B), nrow=2, byrow=TRUE)
matriisi

vastaus2 = chisq.test(matriisi)
format(vastaus2$p.value, scientific = FALSE)

# p-arvo 0.0001280688924 on pienempi kuin riskitaso 0.001
# jonka seurauksena
# H0 hylätään, ja H1 astuu voimaan
# Lääkkeellä on selvä vaikutus paranemiseen



# Tehtävä 1: Testaa 5% riskillä, noudattavatko
# linja-autojen kulkuajat tasaista jakaumaa.
# Tätä varten laskettiin tunnin aikana
# pysäkin ohittavat linja-autot. Mikä on testiin 
# liittyvä p-arvo 5 desimaalin tarkkuudella?
# Mikä on johtopäätös, kun
# H0 = ei poikkea tilastollisesti 5% riskillä.
# H1 = poikkeaa tilastollisesti 5% riskillä.
# Dataksi saatiin seuraava (1,2,3,4) neljännestunti:
data = c(6, 15, 9, 18)

# koska data on luokitteluasteikollista, käytän
# chi squared yhteensopivuustestiä:
tn_pitaisi_olla = c(0.25, 0.25, 0.25, 0.25)

vastaus = chisq.test(data, p = tn_pitaisi_olla)
p_arvo = vastaus$p.value
formatted_p_arvo = sprintf("%.5f", p_arvo)
formatted_p_arvo

# koska saatu p-arvo on 0.05756 eli yli 0.05,
# H0 jää voimaan


# Tehtävä 2: Alkoholin vaikutusta testattiin suorituskykyyn.
# Testaa 5% riskitasolla, onko alkoholin nautiskelu
# madaltanut testeistä saatavia pisteitä. Pistemäärät
# oletetaan normaalisti jakautuneeksi.
# Datana toimii seuraava:
ennen = c(18.0,	17.0,	16.5,	15.5,	14.7,	14.0,	13.5,	12.5,	12.5,	11.5,	10.0,	9.0,	8.5,	8.2,	3.5)
jalkeen = c(19.5,	18.5,	12.5,	16.4,	13.8,	13.5,	12.5,	9.0,	14.5,	8.0,	7.5,	6.0,	8.0,	7.8,	2.5)

summary(ennen)
summary(jalkeen)

# Hypoteesit:
# H0: jalkeen = ennen
# H1: jalkeen < ennen

# Koska data on normaalisti jakautunutta jatkuvaa dataa,
# käytän riippuvaa t-testiä. 
# Annan parametriksi myös var.equal, eli oletan molempien datojen varianssien olevan sama,
# Laitan jalkeen, ennen, less, koska haluan katsoa, onko jalkeen pienempi kuin ennen.
vastaus = t.test(jalkeen, ennen, alternative="less", conf.level = 0.95, var.equal = TRUE, paired = TRUE)

p_value = vastaus$p.value
formatted_p_value = sprintf("%.5f", p_value)
formatted_p_value

# Koska p-arvo 0.03486 on pienempi kuin 0.05, 
# H1-hypoteesi astuu voimaan



# Tehtävä 3: Tutkittiin rautatablettien vaikutusta
# henkilöiden hemoglobiiniiniarvoihin. 
# Rautatabletteja syötiin 3kk, ja hemoglobiini
# mitattiin ennen ja jälkeen. 
# Ilmoita p-arvo 6 desimaalin tarkkuudella.
# Lisäksi testaa 1% riskillä, muuttiko 
# rautatabletit hemoglobiinia johonkin suuntaan.
# Dataksi saatiin seuraava: 
ennen = c(136,	129,	140,	137,	128,	145,	156,	141,	133,	126,	122,	147,	138,	130,	154)
jalkeen = c(142,	128,	145,	138,	134,	152,	165,	145,	141,	134,	128,	149,	136,	135,	154)

# Käytän taas kerran riippuvaa (PAIRED=TRUE) t-testiä, koska data on jatkuvaa,
# ja koska hemoglobiini on ilmiönä normaalijakautunut.
vastaus = t.test(jalkeen, ennen, alternative="two.sided", conf.level = 0.99, var.equal=TRUE, paired=TRUE)
p_value = vastaus$p.value
formatted_p_value = sprintf("%.6f", p_value)
formatted_p_value

# Hypoteesit oli 1 % (0.01) riskitasolla:
# H0: tablettien syöminen ei vaikuta hemoglobiiniin
# H1: tablettien syöminen vaikuttaa hemoglobiiniin

# Koska saatu p-arvo 0.000306 on pienempi kuin 0.01, 
# H1 hypoteesi astuu voimaan riskitasolla 1%



# Tehtävä 4: 80 koehenkilölle annetaan testiin 4 eri shampoota.
# Jos jokainen shampoo olisi yhtä hyvä, 
# Vastaukset jakautuisivat Shampoo (A, B, C, D) = (20, 20, 20, 20)
# Testaa 5% riskillä, voidaanko shampoo-esimerkissä 
# käyttää tasajakaumaa matemaattisena mallina
# Ilmoita testistä saatu p-arvo 4 desimaalin tarkkuudella. 
# Sekä päätä kumpi hypoteeseista on 5% riskillä tosi:
# H0: shampoolaatujen välillä ei ole eroa
# H1: shampoolaatujen välillä on eroa
# Dataksi saatiin seuraava (suosikki shampoo) (A, B, C, D):
data = c(15, 28, 17, 20)

# koska data on luokitteluasteikollista, 
# käytän chi squared yhteensopivuustestiä
tn_pitaisi_olla = c(0.25, 0.25, 0.25, 0.25)

vastaus = chisq.test(data, p = tn_pitaisi_olla)
p_arvo = vastaus$p.value
formatted_p_arvo = sprintf("%.4f", p_arvo)
formatted_p_arvo

# koska saatu p-arvo 0.1793 on suurempi kuin 0.05,
# jää voimaan H0 hypoteesi


# Tehtävä 5: Tutkittiin rokotteen tehoa.
# Rokotettiin 70 vapaaehtoista.
# Vertailuryhmäksi valittiin 70 rokottamatonta.
# Ilmoita testistä saatu p-arvo 5 desimaalin tarkkuudella.
# Testaa 1% riskillä, ehkäisikö rokote tartuntoja.
# Dataksi saatiin c(sairastui, ei-sairastui) seuraavat tulokset:
rokotetuista = c(20, 50)
rokottamattomista = c(40, 30)

# Data on luokitteluasteikollista, eli käytän
# chi squared riippumattomuustestiä:

# Riskitaso 1 % (0.01)
# H0: Rokote ei vaikuttanut tartuntoihin
# H1: Rokotteella on ehkäisevää vaikutusta tartuntoihin'
matriisi = matrix(c(rokotetuista, rokottamattomista), nrow=2, byrow = TRUE)

vastaus = chisq.test(matriisi)
p_arvo = vastaus$p.value
formatted_p_arvo = sprintf("%.5f", p_arvo)
formatted_p_arvo

# saatu p-arvo 0.00118 on pienempi kuin 0.01
# Eli H1 hypoteesi astuu voimaan
