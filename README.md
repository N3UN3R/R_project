# neuer Ansatz Version 2 ist aktuellste


# task
Sie und Ihr Team bekommen von Ihrem Vorgesetzten die Aufgabe, die Anzahl an Ersatzeinzelteile für das kommende Kalenderjahr zu kalkulieren. 

Analysieren Sie die Daten aus den letzten drei Jahren (2014 - einschließlich 2016).

Sie können davon ausgehen, dass Fahrzeuge mit Fehlern in der Stadt repariert wurden, in der sie auch zugelassen worden sind.

In den Städten Köln, Dortmund, Bielefeld, Bonn, Oberhausen, Bochum, Münster und Aachen werden Werkstätten von Ihnen betrieben


## analysis of needed data

nachfolgend sind für jeden Ordner und innerhalb des Ordners die Spaltenbezeichnungen der Datensätze aufgelistet

# Analysing which dataset contain needed information

"Bestandteile_Fahrzeuge_OEM1_Typ11" 

ID_Karosserie	ID_Schaltung	ID_Sitze	ID_Motor	ID_Fahrzeug

"Fahrzeuge oem csv"

"X1","ID_Fahrzeug","Produktionsdatum","Herstellernummer","Werksnummer","Fehlerhaft","Fehlerhaft_Datum","Fehlerhaft_Fahrleistung"

Fahrzeuge oem_typ_21,"X1","ID_Fahrzeug","Herstellernummer","Werksnummer","Fehlerhaft","Fehlerhaft_Datum","Fehlerhaft_Fahrleistung","Produktionsdatum_Origin_01011970","origin"

# Einzelteil folder

contains .txt files and csv files

columns of Einzelteil_T04 

X1,ID_T04, Herstellernummer, Werksnummer, Fehlerhaft, Fehlerhaft_Datum,Fehlerhaft_Fahrleistung,Produktionsdatum_Origin_01011970,origin 

Einzelteil_T16.csv

"X1" | | "ID_T16.x" | | "Produktionsdatum.x" | | "Herstellernummer.x" | | "Werksnummer.x" | | "Fehlerhaft.x" | | "Fehlerhaft_Datum.x" | | "Fehlerhaft_Fahrleistung.x" | | "ID_T16.y" | | "Produktionsdatum.y" | | "Herstellernummer.y" | | "Werksnummer.y" | | "Fehlerhaft.y" | | "Fehlerhaft_Datum.y" | | "Fehlerhaft_Fahrleistung.y" | | "ID_T16" | | "Produktionsdatum" | | "Herstellernummer" | | "Werksnummer" | | "Fehlerhaft" | | "Fehlerhaft_Datum" | | "Fehlerhaft_Fahrleistung"


# Fahrzeug

Bestandteile_Fahrzeuge_OEM1_Typ11.csv

ID_Karosserie	ID_Schaltung	ID_Sitze	ID_Motor	ID_Fahrzeug

Fahrzeuge_OEM1_Typ11.csv
,"X1","ID_Fahrzeug","Produktionsdatum","Herstellernummer","Werksnummer","Fehlerhaft","Fehlerhaft_Datum","Fehlerhaft_Fahrleistung"

Fahrzeuge_OEM2_Typ21.csv
,"X1","ID_Fahrzeug","Herstellernummer","Werksnummer","Fehlerhaft","Fehlerhaft_Datum","Fehlerhaft_Fahrleistung","Produktionsdatum_Origin_01011970","origin"

# Geodaten

Tier2_Werke_2017-07-11_v1.2_TrR.csv
PLZ 	ORT	Werk	Breitengrad	L„ngengrad

Geodaten_Gemeinden_v1.2_2017-08-22_TrR.csv
X	Postleitzahl	Gemeinde	Laengengrad	Breitengrad


# Logistikverzug
Komponente_K7.csv
IDNummer	Produktionsdatum	Herstellernummer	Werksnummer	Fehlerhaft

Logistikverzug_K7.csv
IDNummer	Wareneingang	Herstellernummer	Werksnummer	Fehlerhaft


# Komponenten folder
Bestandteile_Komponente_K1BE1.cvs

ID_T1	ID_T2	ID_T3	ID_T4	ID_K1BE1

Komponente_K1BE1.cvs

,"X1","ID_Motor","Herstellernummer","Werksnummer","Fehlerhaft","Fehlerhaft_Datum","Fehlerhaft_Fahrleistung","Produktionsdatum_Origin_01011970","origin"

Komponente_K4.cvs

X1	ID_Karosserie.x	Produktionsdatum.x	Herstellernummer.x	Werksnummer.x	Fehlerhaft.x	Fehlerhaft_Datum.x	Fehlerhaft_Fahrleistung.x	ID_Karosserie.y	Produktionsdatum.y	Herstellernummer.y	Werksnummer.y	Fehlerhaft.y	Fehlerhaft_Datum.y	Fehlerhaft_Fahrleistung.y



# Zulassungen alle Fahrzeuge
Zulassungen_alle_Fahrzeuge.csv
	xx IDNummer	Gemeinden	Zulassung

### Einzelteile
 
##1
```{r, message=FALSE, warning = FALSE, cache = TRUE}
txt <- readLines("Einzelteil/Einzelteil/Einzelteil_T01.txt")
txt <- gsub(" | | ",",",txt, fixed = TRUE)
txt <- gsub(" ","\n",txt, fixed = TRUE)
part_T01 <- fread(text = txt, sep = ",", data.table = FALSE) %>% 
  filter((Fehlerhaft.x == 1 | Fehlerhaft.y == 1 | Fehlerhaft == 1 ) &
         ((year(as.Date(Fehlerhaft_Datum)) >= 2014 & year(as.Date(Fehlerhaft_Datum)) <= 2016) |
          (year(as.Date(Fehlerhaft_Datum.x)) >= 2014 & year(as.Date(Fehlerhaft_Datum.x)) <= 2016) |
          (year(as.Date(Fehlerhaft_Datum.y)) >= 2014 & year(as.Date(Fehlerhaft_Datum.y)) <= 2016))) 

glimpse(part_T01)
```
##2
```{r, message=FALSE, warning = FALSE, cache = TRUE}
txt <- readLines("Einzelteil/Einzelteil/Einzelteil_T02.txt")
txt <- gsub("  ",",",txt, fixed = TRUE)
txt <- gsub("\t","\n",txt, fixed = TRUE)
part_T02 <- fread(text = txt, sep = ",", data.table = FALSE) %>% 
  filter((Fehlerhaft.x == 1 | Fehlerhaft.y == 1) &
         ((year(as.Date(Fehlerhaft_Datum.x)) >= 2014 & year(as.Date(Fehlerhaft_Datum.x)) <= 2016) |
          (year(as.Date(Fehlerhaft_Datum.y)) >= 2014 & year(as.Date(Fehlerhaft_Datum.y)) <= 2016))) 

glimpse(part_T02)
```
##3
```{r, message=FALSE, warning = FALSE, cache = TRUE}
txt <- readLines("Einzelteil/Einzelteil/Einzelteil_T03.txt")
txt <- gsub("|",",",txt, fixed = TRUE)
txt <- gsub("\v","\n",txt, fixed = TRUE)
part_T03 <- fread(text = txt, sep = ",", data.table = FALSE) %>% 
  filter((Fehlerhaft== 1) &
         ((year(as.Date(Fehlerhaft_Datum)) >= 2014 & year(as.Date(Fehlerhaft_Datum)) <= 2016))) 

glimpse(part_T03)
```
##4
```{r, message=FALSE, warning = FALSE, cache = TRUE}
part_T04 <- fread("Einzelteil/Einzelteil/Einzelteil_T04.csv", header = TRUE, data.table = FALSE) %>% 
  filter(Fehlerhaft == 1 &
         ((year(as.Date(Fehlerhaft_Datum)) >= 2014 & year(as.Date(Fehlerhaft_Datum)) <= 2016)))
glimpse(part_T04)
```
##5
```{r, message=FALSE, warning = FALSE, cache = TRUE}
part_T05 <- fread("Einzelteil/Einzelteil/Einzelteil_T05.csv", header = TRUE, data.table = FALSE) %>% 
 filter((Fehlerhaft.x == 1 | Fehlerhaft.y == 1) &
         ((year(as.Date(Fehlerhaft_Datum.x)) >= 2014 & year(as.Date(Fehlerhaft_Datum.x)) <= 2016) |
          (year(as.Date(Fehlerhaft_Datum.y)) >= 2014 & year(as.Date(Fehlerhaft_Datum.y)) <= 2016))) 
glimpse(part_T05)
```
##6
```{r, message=FALSE, warning = FALSE, cache = TRUE}
part_T06 <- fread("Einzelteil/Einzelteil/Einzelteil_T06.csv", header = TRUE, data.table = FALSE) %>% 
  filter(Fehlerhaft == 1 &
         ((year(as.Date(Fehlerhaft_Datum)) >= 2014 & year(as.Date(Fehlerhaft_Datum)) <= 2016)))
glimpse(part_T06)
```
##7 (Fehlerhaft)
```{r, message=FALSE, warning = FALSE, cache = TRUE}
txt <- readLines("Einzelteil/Einzelteil/Einzelteil_T07.txt")
txt <- gsub("|",",",txt, fixed = TRUE)
txt <- gsub("\v","\n",txt, fixed = TRUE)
part_T07 <- fread(text = txt, sep = ",", data.table = FALSE) %>% 
  filter((Fehlerhaft== 1) &
         ((year(as.Date(Fehlerhaft_Datum)) >= 2014 & year(as.Date(Fehlerhaft_Datum)) <= 2016))) 

glimpse(part_T07)
```
##8
```{r, message=FALSE, warning = FALSE, cache = TRUE}
part_T08 <- fread("Einzelteil/Einzelteil/Einzelteil_T08.csv", header = TRUE, data.table = FALSE) %>% 
  filter(Fehlerhaft == 1 &
         ((year(as.Date(Fehlerhaft_Datum)) >= 2014 & year(as.Date(Fehlerhaft_Datum)) <= 2016)))
glimpse(part_T08)
```
##9 (Fehlerhaft)
```{r, message=FALSE, warning = FALSE, cache = TRUE}
txt <- readLines("Einzelteil/Einzelteil/Einzelteil_T09.txt")
txt <- gsub('\\\\', ',', txt, fixed = TRUE)
txt <- gsub('\"', '\n', txt, fixed = TRUE)
part_T09 <- fread(text = txt, sep = ",", data.table = FALSE) %>% 
 filter((Fehlerhaft.x == 1 | Fehlerhaft.y == 1) &
         ((year(as.Date(Fehlerhaft_Datum.x)) >= 2014 & year(as.Date(Fehlerhaft_Datum.x)) <= 2016) |
          (year(as.Date(Fehlerhaft_Datum.y)) >= 2014 & year(as.Date(Fehlerhaft_Datum.y)) <= 2016))) 

glimpse(part_T09)
```
##10
```{r, message=FALSE, warning = FALSE, cache = TRUE}
part_T10 <- fread("Einzelteil/Einzelteil/Einzelteil_T10.csv", header = TRUE, data.table = FALSE) %>% 
  filter(Fehlerhaft == 1 &
         ((year(as.Date(Fehlerhaft_Datum)) >= 2014 & year(as.Date(Fehlerhaft_Datum)) <= 2016)))
glimpse(part_T10)
```
##11
```{r, message=FALSE, warning = FALSE, cache = TRUE}
txt <- readLines("Einzelteil/Einzelteil/Einzelteil_T11.txt")
txt <- gsub("\t", ",", txt, fixed = TRUE)
txt <- gsub("\f", "\n", txt, fixed = TRUE)
part_T11 <- fread(text = txt, sep = ",", data.table = FALSE) %>% 
  filter(Fehlerhaft == 1 & 
         year(as.Date(Fehlerhaft_Datum)) >= 2014 & 
         year(as.Date(Fehlerhaft_Datum)) <= 2016) 

glimpse(part_T11)
```
##12, 13, 14, 15
```{r, message=FALSE, warning = FALSE, cache = TRUE}
part_T12 <- fread("Einzelteil/Einzelteil/Einzelteil_T12.csv", header = TRUE, data.table = FALSE) %>% 
  filter(Fehlerhaft == 1 &
         ((year(as.Date(Fehlerhaft_Datum)) >= 2014 & year(as.Date(Fehlerhaft_Datum)) <= 2016)))
glimpse(part_T12)
```
```{r, message=FALSE, warning = FALSE, cache = TRUE}
part_T13 <- fread("Einzelteil/Einzelteil/Einzelteil_T13.csv", header = TRUE, data.table = FALSE) %>% 
  filter(Fehlerhaft == 1 &
         ((year(as.Date(Fehlerhaft_Datum)) >= 2014 & year(as.Date(Fehlerhaft_Datum)) <= 2016)))
glimpse(part_T13)
```
```{r, message=FALSE, warning = FALSE, cache = TRUE}
part_T14 <- fread("Einzelteil/Einzelteil/Einzelteil_T14.csv", header = TRUE, data.table = FALSE) %>% 
  filter(Fehlerhaft == 1 &
         ((year(as.Date(Fehlerhaft_Datum)) >= 2014 & year(as.Date(Fehlerhaft_Datum)) <= 2016)))
glimpse(part_T14)
```
```{r, message=FALSE, warning = FALSE, cache = TRUE}
part_T15 <- fread("Einzelteil/Einzelteil/Einzelteil_T15.csv", header = TRUE, data.table = FALSE) %>% 
 filter((Fehlerhaft.x == 1 | Fehlerhaft.y == 1) &
         ((year(as.Date(Fehlerhaft_Datum.x)) >= 2014 & year(as.Date(Fehlerhaft_Datum.x)) <= 2016) |
          (year(as.Date(Fehlerhaft_Datum.y)) >= 2014 & year(as.Date(Fehlerhaft_Datum.y)) <= 2016))) 
glimpse(part_T15)
```
##16 (Ferhlerhaft)
```{r, message=FALSE, warning = FALSE, cache = TRUE}
txt <- readLines("Einzelteil/Einzelteil/Einzelteil_T16.txt")
txt <- gsub(" | | ",",",txt, fixed = TRUE)
txt <- gsub(" ","\n",txt, fixed = TRUE)
part_T16 <- fread(text = txt, sep = ",", data.table = FALSE) %>% 
  filter((Fehlerhaft.x == 1 | Fehlerhaft.y == 1 | Fehlerhaft == 1 ) &
         ((year(as.Date(Fehlerhaft_Datum)) >= 2014 & year(as.Date(Fehlerhaft_Datum)) <= 2016) |
          (year(as.Date(Fehlerhaft_Datum.x)) >= 2014 & year(as.Date(Fehlerhaft_Datum.x)) <= 2016) |
          (year(as.Date(Fehlerhaft_Datum.y)) >= 2014 & year(as.Date(Fehlerhaft_Datum.y)) <= 2016))) 

glimpse(part_T16)
```
##17
```{r, message=FALSE, warning = FALSE, cache = TRUE}
part_T17 <- fread("Einzelteil/Einzelteil/Einzelteil_T17.csv", header = TRUE, data.table = FALSE) %>% 
  filter((Fehlerhaft.x == 1 | Fehlerhaft.y == 1 | Fehlerhaft == 1 ) &
         ((year(as.Date(Fehlerhaft_Datum)) >= 2014 & year(as.Date(Fehlerhaft_Datum)) <= 2016) |
          (year(as.Date(Fehlerhaft_Datum.x)) >= 2014 & year(as.Date(Fehlerhaft_Datum.x)) <= 2016) |
          (year(as.Date(Fehlerhaft_Datum.y)) >= 2014 & year(as.Date(Fehlerhaft_Datum.y)) <= 2016))) 

glimpse(part_T17)
```
##18
```{r, message=FALSE, warning = FALSE, cache = TRUE}
part_T18 <- fread("Einzelteil/Einzelteil/Einzelteil_T18.csv", header = TRUE, data.table = FALSE) %>% 
  filter(Fehlerhaft == 1 &
         ((year(as.Date(Fehlerhaft_Datum)) >= 2014 & year(as.Date(Fehlerhaft_Datum)) <= 2016)))
glimpse(part_T18)
```
##19
```{r, message=FALSE, warning = FALSE, cache = TRUE}
part_T19 <- fread("Einzelteil/Einzelteil/Einzelteil_T19.csv", header = TRUE, data.table = FALSE) %>% 
  filter(Fehlerhaft == 1 &
         ((year(as.Date(Fehlerhaft_Datum)) >= 2014 & year(as.Date(Fehlerhaft_Datum)) <= 2016)))
glimpse(part_T19)
```
##20
```{r, message=FALSE, warning = FALSE, cache = TRUE}
txt <- readLines("Einzelteil/Einzelteil/Einzelteil_T20.txt")
txt <- gsub(" | | ",",",txt, fixed = TRUE)
txt <- gsub(" ","\n",txt, fixed = TRUE)
part_T20 <- fread(text = txt, sep = ",", data.table = FALSE) %>% 
    filter(Fehlerhaft == 1 &
         ((year(as.Date(Fehlerhaft_Datum)) >= 2014 & year(as.Date(Fehlerhaft_Datum)) <= 2016)))
glimpse(part_T20)
```
##21
```{r, message=FALSE, warning = FALSE, cache = TRUE}
part_T21 <- fread("Einzelteil/Einzelteil/Einzelteil_T21.csv", header = TRUE, data.table = FALSE) %>% 
  filter(Fehlerhaft == 1 &
         ((year(as.Date(Fehlerhaft_Datum)) >= 2014 & year(as.Date(Fehlerhaft_Datum)) <= 2016)))
glimpse(part_T21)
```
##22
```{r, message=FALSE, warning = FALSE, cache = TRUE}
txt <- readLines("Einzelteil/Einzelteil/Einzelteil_T22.txt")
txt <- gsub("\"\t\"",",",txt)
txt <- gsub("\"\t",",",txt)
txt <- gsub("\t\"",",",txt)
txt <- gsub("\t",",",txt)
txt <- gsub("\"\"","\n",txt)
txt <- gsub("\"","\n",txt)
part_T22 <- fread(text = txt, sep = ",", data.table = FALSE) %>% 
 filter((Fehlerhaft.x == 1 | Fehlerhaft.y == 1) &
         ((year(as.Date(Fehlerhaft_Datum.x)) >= 2014 & year(as.Date(Fehlerhaft_Datum.x)) <= 2016) |
          (year(as.Date(Fehlerhaft_Datum.y)) >= 2014 & year(as.Date(Fehlerhaft_Datum.y)) <= 2016))) 

glimpse(part_T22)
```
##
























