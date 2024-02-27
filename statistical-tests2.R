# more statistical tests 
# 27.2.2024

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