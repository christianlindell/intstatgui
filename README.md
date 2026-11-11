# intstatgui

Hämta data från Eurostat och OECD med ett grafiskt gränssnitt, också som addin i RStudio och
Positron. Du väljer datakälla, bläddrar bland eller söker i tabellerna, väljer variabler och
värden och får färdig R-kod. Data kan också hämtas direkt och laddas ner som txt, Excel eller
parquet. Paketet bygger på CRAN-paketen [eurostat](https://ropengov.github.io/eurostat/) och
[OECDdata](https://github.com/alekrutkowski/OECDdata) och är en motsvarighet till pxweb2rgui.

## Installation

```r
# install.packages("devtools")
devtools::install("C:/Users/chris/OneDrive/R_claude/addin_eurostat/intstatgui")
```

Valfria paket: `OECDdata` (krävs för OECD), `writexl` eller `openxlsx` (Excel), `nanoparquet` eller `arrow` (parquet),
`janitor` (R-kompatibla kolumnnamn), `clipr` (kopiering i fönster där webbläsarens urklipp inte
fungerar) och `rstudioapi` (infoga koden i skriptet). Ingen inloggning behövs.

## Kom igång

Välj **Addins > Eurostat- eller OECD-tabell till R-kod** i RStudio, eller kör:

```r
library(intstatgui)
es_gui()
```

Koden från gränssnittet ser ut så här:

```r
library(intstatgui)

# Population change - Demographic balance and crude rates at national level
dataset_df <- es_get_data(
  id = "demo_gind",
  filters = list(
    indic_de = "JAN",
    geo = c("SE", "NO"),
    lastTimePeriod = 1
  ),
  code_variables = "geo",
  lang = "en",
  time_format = "raw"
)
```

Med OECD får koden också raden `source = "oecd"`.

Kryssa i "Kod som bara kräver paketet eurostat" om koden ska gå att köra utan det här paketet.
Då blir det ett anrop till `eurostat::get_eurostat()`, så länge frågan gäller
Eurostat och inga kodkolumner.

## Gränssnittet

1. **Hitta tabell.** Välj datakälla och språk (engelska eller franska, hos Eurostat också tyska),
   bläddra i ämnesträdet eller sök på namn eller id och klicka på en tabell. Hos Eurostat visas bara dataset, tills du avmarkerar rutan för det. Tabellistan sparas i
   24 timmar.
2. **Välj variabler.** Ett klick väljer ett värde och avmarkerar övriga, Ctrl+klick lägger till eller tar
   bort ett värde och Skift+klick väljer ett intervall, som i SCB:s databas. Hos Eurostat kräver varje variabel ett val, eftersom en utelämnad variabel
   betyder alla värden och inte en summa. Hos OECD betyder en tom lista alla möjliga värden. Senaste perioden
   är förvald. För
   regioner hos Eurostat går listan att begränsa till länder, NUTS 1, NUTS 2, NUTS 3 eller aggregat.
3. **Kod och data.** Färdig kod, val av tidsformat och kodkolumner, hämtning och nedladdning.
4. **Information.** Knappen "Information" i flik 1 visar uppgifter om tabellen och länkar: till
   Eurostats metadata, fotnoter, DOI och Data Browser, eller till OECD Data Explorer.

## Funktioner

| Funktion | Vad den gör | Obligatoriska parametrar |
|---|---|---|
| `es_gui()` | Grafiskt gränssnitt för Eurostat och OECD | – |
| `es_gui_addin()` | RStudio-addin: tabell från Eurostat eller OECD till R-kod | – |
| `es_get_data()` | Hämta data från Eurostat eller OECD | `id` |
| `es_search_tables()` | Sök bland tabellerna hos Eurostat eller OECD | – |
| `es_get_codes()` | Hämta en tabells variabler och värden | `id` |
| `es_get_comments()` | Hämta uppgifter om en tabell | `id` |

## Bra att veta

* Eurostats flaggor, till exempel `p` för preliminära värden, följer inte med i `es_get_data()`.
  `eurostat::get_eurostat()` kan bara ge dem för en hel tabell, med `keepFlags = TRUE`.
* Antalet celler som gränssnittet visar är hela rutnätet. Många celler kan sakna data, och de
  raderna tas bort om inte `na_rm = FALSE` anges.
* Stora frågor delas automatiskt. Svarar Eurostat att en fråga är för stor för ett direkt svar
  hämtas hela tabellen som fil, och raderna väljs ur den.
* `es_get_data()` använder inte cachen i `get_eurostat()`, eftersom den inte skiljer på språk.

### Kombinationer av värden

Alla värden går inte att kombinera, till exempel en folkmängd i enheten PPP-dollar. Med OECD frågar
gränssnittet efter varje val vilka värden som är möjliga. Övriga blir gråa, redan valda sådana värden
blir röda och antalet värden med data visas. Lämna en lista tom, till exempel enheten, så får varje valt
mått sina egna värden. Not applicable (`_Z`) tas alltid med när ett annat värde är valt i variabeln, så att
mått där variabeln inte gäller blir kvar. Har en variabel flera valda värden och något inte ger någon rad säger ett
meddelande det efter hämtningen. Eurostat har ingen
motsvarande fråga, så där visar sig en kombination utan data först när du hämtar: resultatet blir tomt
och ett meddelande säger det.

### OECD

* OECD tillåter 60 hämtningar av data per timme och spärrar tillfälligt vid fler. Under test har spärren också kommit efter många frågor om möjliga värden, så det är inte säkert att bara datahämtningar räknas. Tabellen är
  en hämtning, och en till för varje sorts period om du väljer till exempel både år och månader.
* OECD anger bara vilken tid en tabell täcker. Perioderna i listan räknas fram från tabellens
  frekvenser, så alla behöver inte ha data. Tiden hämtas som ett intervall, och de valda perioderna
  väljs ur svaret.
* "Hämta alltid senaste perioden" gäller tabellens senaste period och inte varje serie för sig, så
  svaret kan bli tomt.
* Data har kolumnen `flagga` med värdets status och `UNIT_MULT`, som visar vilken tiopotens värdet
  anges i. Tabellistan saknar perioder och uppdateringsdatum, och det finns inga regionnivåer.

## Versionshistorik

### 0.6.1 (2026-11-05)

#### Rättat
* `es_gui()`: varningen "argument is of length zero" i konsolen när addinet öppnas är borta. Den kom från rutan
  med antal värden, som ritades innan en tabell var vald.

### 0.6.0 (2026-11-05)

#### Ändrat
* `es_gui()`: det går bara att välja en tabell åt gången i trädet. Ctrl+klick i trädet och sammanslagningen av
  flera tabeller i flik 2 är borttagna.
* `es_get_data()`: **Bakåtinkompatibelt:** `id` är en enda tabell, och kolumnen `tabell` finns inte längre. Skript
  som hämtar flera tabeller med ett anrop behöver göra ett anrop per tabell.

### 0.5.1 (2026-11-05)

#### Rättat
* `es_gui()` med OECD: `_Z` (Not applicable) tas alltid med när ett annat värde är valt i samma variabel. Väljer
  man till exempel Population och GDP med Constant prices blir Population kvar, eftersom det måttet saknar prisbas.
  Koden visar det, till exempel `PRICES = c("Q", "_Z")`.

### 0.5.0 (2026-11-05)

#### Nytt
* `es_gui()` med OECD: en tom lista betyder alla möjliga värden, så ingen variabel är obligatorisk. Det går
  att välja till exempel både Population och GDP, och varje mått får sina egna enheter. Automatiska val är
  borttagna, variabler med ett enda värde visas inte och antalet värden med data står högst upp.
* `es_get_data()` med OECD: ett meddelande och attributet `saknas` visar valda värden som inte gav någon rad.

#### Ändrat
* `es_gui()`: klicken i listorna i flik 2 fungerar som i SCB:s databas. Ett klick väljer ett värde och
  avmarkerar övriga, Ctrl+klick lägger till eller tar bort ett värde och Skift+klick väljer ett intervall.
* `es_get_data()` och `es_gui()`: **Bakåtinkompatibelt:** kodkolumnerna heter nu `<variabel>_code` i stället
  för `<variabel>_kod`, till exempel `geo_code`. Skript som använder de gamla namnen behöver ändras.

### 0.4.3 (2026-11-04)

#### Rättat
* `es_get_data()` med OECD: om OECD:s server tillfälligt inte svarar (status 502, 503 eller 504) görs ett nytt
  försök. Hjälper inte det säger ett meddelande på svenska att felet brukar vara tillfälligt. Vid en spärr
  (status 429) görs inga nya försök.

### 0.4.2 (2026-11-04)

#### Rättat
* `es_get_data()` med OECD: ett urval utan data ger ett tomt resultat och ett meddelande i stället för ett
  tekniskt fel (status 404).

### 0.4.1 (2026-11-04)

#### Ändrat
* `es_gui()` med OECD: färre frågor. Listorna kontrolleras bara när flik 2 är öppen och med längre väntan
  efter ett klick. Svarar OECD att anropen är för många skickas inga fler sådana frågor under sessionen,
  och ett meddelande förklarar det. Vid fel görs inga nya försök mot OECD.

#### Rättat
* `es_gui()` med Eurostat: mappar utan namn (`---` eller `...` i Eurostats lista) visas inte längre.
  Det de innehåller står i mappen ovanför.
* `es_gui()` med OECD: "Markera alla" markerar bara möjliga värden, och markeringen följer de möjliga
  värdena när andra val ändras.
* `es_gui()` med OECD: appen avslutades när väldigt många värden valdes i en variabel, till exempel
  med "Markera alla" bland regionerna. I RStudio såg det ut som en krasch.
* `es_get_data()` med OECD: mycket långa listor med värden kan hämtas. Variabeln hämtas då med alla
  värden, och de valda väljs ur svaret.

### 0.4.0 (2026-11-04)

#### Nytt
* `es_gui()` med OECD: värden som inte går att kombinera med övriga val blir gråa och går inte att
  välja. Redan valda sådana värden blir röda. Finns bara ett möjligt värde markeras det. Antalet
  värden med data visas, och ett urval utan data går inte att hämta.

#### Ändrat
* `es_gui()`: rutan "Visa bara dataset, inte färdiga tabeller" är förvald för Eurostat. Avmarkera den
  för att se de färdiga tabellerna.

#### Rättat
* `es_get_data()` med Eurostat: en kombination utan data i ett litet urval laddar inte längre ner
  hela tabellen, utan ger ett tomt resultat och ett meddelande. I gränssnittet visas samma besked.

### 0.3.0 (2026-11-04)

#### Ändrat
* **Bakåtinkompatibelt:** paketet har bytt namn från eurostatgui till intstatgui. Skript behöver
  `library(intstatgui)`. Inställningarna heter nu `intstatgui.*`, till exempel
  `options(intstatgui.source = "oecd")`. Funktionerna heter som förut, med prefixet `es_`.

### 0.2.0 (2026-11-04)

#### Nytt
* Stöd för OECD, med paketet OECDdata. I gränssnittet finns valet "Datakälla" i flik 1.
* `es_gui()`, `es_get_data()`, `es_search_tables()`, `es_get_codes()` och `es_get_comments()`:
  ny parameter `source`, `"eurostat"` (standard) eller `"oecd"`. Befintlig kod fungerar som förut.
* `es_get_data()` med OECD ger kolumnen `flagga` med värdets status och `UNIT_MULT`.

### 0.1.0 (2026-11-04)

* Första versionen.
* `es_gui()` och `es_gui_addin()`: grafiskt gränssnitt och addin för RStudio och Positron.
  Bläddra bland eller sök i Eurostats tabeller, välj variabler och värden och få färdig kod,
  eller hämta data direkt och ladda ner det som txt, Excel eller parquet.
* `es_get_data()`: hämtar en eller flera tabeller med namn på värdena och valfria kodkolumner.
* `es_search_tables()`, `es_get_codes()` och `es_get_comments()`: tabellistan, en tabells
  variabler och värden samt uppgifter om en tabell.
