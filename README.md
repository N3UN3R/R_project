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



