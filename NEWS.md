# intstatgui 0.6.1

## Rättat
* `es_gui()`: varningen "argument is of length zero" i konsolen när addinet öppnas är borta. Den kom från rutan
  med antal värden, som ritades innan en tabell var vald.

# intstatgui 0.6.0

## Ändrat
* `es_gui()`: det går bara att välja en tabell åt gången i trädet. Ctrl+klick i trädet och sammanslagningen av
  flera tabeller i flik 2 är borttagna.
* `es_get_data()`: **Bakåtinkompatibelt:** `id` är en enda tabell, och kolumnen `tabell` finns inte längre. Skript
  som hämtar flera tabeller med ett anrop behöver göra ett anrop per tabell.

# intstatgui 0.5.1

## Rättat
* `es_gui()` med OECD: `_Z` (Not applicable) tas alltid med när ett annat värde är valt i samma variabel. Väljer
  man till exempel Population och GDP med Constant prices blir Population kvar, eftersom det måttet saknar prisbas.
  Koden visar det, till exempel `PRICES = c("Q", "_Z")`.

# intstatgui 0.5.0

## Nytt
* `es_gui()` med OECD: en tom lista betyder alla möjliga värden, så ingen variabel är obligatorisk. Det går
  att välja till exempel både Population och GDP, och varje mått får sina egna enheter. Automatiska val är
  borttagna, variabler med ett enda värde visas inte och antalet värden med data står högst upp.
* `es_get_data()` med OECD: ett meddelande och attributet `saknas` visar valda värden som inte gav någon rad.

## Ändrat
* `es_gui()`: klicken i listorna i flik 2 fungerar som i SCB:s databas. Ett klick väljer ett värde och
  avmarkerar övriga, Ctrl+klick lägger till eller tar bort ett värde och Skift+klick väljer ett intervall.
* `es_get_data()` och `es_gui()`: **Bakåtinkompatibelt:** kodkolumnerna heter nu `<variabel>_code` i stället
  för `<variabel>_kod`, till exempel `geo_code`. Skript som använder de gamla namnen behöver ändras.

# intstatgui 0.4.3

## Rättat
* `es_get_data()` med OECD: om OECD:s server tillfälligt inte svarar (status 502, 503 eller 504) görs ett nytt
  försök. Hjälper inte det säger ett meddelande på svenska att felet brukar vara tillfälligt. Vid en spärr
  (status 429) görs inga nya försök.

# intstatgui 0.4.2

## Rättat
* `es_get_data()` med OECD: ett urval utan data ger ett tomt resultat och ett meddelande i stället för ett
  tekniskt fel (status 404).

# intstatgui 0.4.1

## Ändrat
* `es_gui()` med OECD: färre frågor. Listorna kontrolleras bara när flik 2 är öppen och med längre väntan
  efter ett klick. Svarar OECD att anropen är för många skickas inga fler sådana frågor under sessionen,
  och ett meddelande förklarar det. Vid fel görs inga nya försök mot OECD.

## Rättat
* `es_gui()` med Eurostat: mappar utan namn (`---` eller `...` i Eurostats lista) visas inte längre.
  Det de innehåller står i mappen ovanför.
* `es_gui()` med OECD: "Markera alla" markerar bara möjliga värden, och markeringen följer de möjliga
  värdena när andra val ändras.
* `es_gui()` med OECD: appen avslutades när väldigt många värden valdes i en variabel, till exempel
  med "Markera alla" bland regionerna. I RStudio såg det ut som en krasch.
* `es_get_data()` med OECD: mycket långa listor med värden kan hämtas. Variabeln hämtas då med alla
  värden, och de valda väljs ur svaret.

# intstatgui 0.4.0

## Nytt
* `es_gui()` med OECD: värden som inte går att kombinera med övriga val blir gråa och går inte att
  välja. Redan valda sådana värden blir röda. Finns bara ett möjligt värde markeras det. Antalet
  värden med data visas, och ett urval utan data går inte att hämta.

## Ändrat
* `es_gui()`: rutan "Visa bara dataset, inte färdiga tabeller" är förvald för Eurostat. Avmarkera den
  för att se de färdiga tabellerna.

## Rättat
* `es_get_data()` med Eurostat: en kombination utan data i ett litet urval laddar inte längre ner
  hela tabellen, utan ger ett tomt resultat och ett meddelande. I gränssnittet visas samma besked.

# intstatgui 0.3.0

## Ändrat
* **Bakåtinkompatibelt:** paketet har bytt namn från eurostatgui till intstatgui. Skript behöver
  `library(intstatgui)`. Inställningarna heter nu `intstatgui.*`, till exempel
  `options(intstatgui.source = "oecd")`. Funktionerna heter som förut, med prefixet `es_`.

# intstatgui 0.2.0

## Nytt
* Stöd för OECD, med paketet OECDdata. I gränssnittet finns valet "Datakälla" i flik 1.
* `es_gui()`, `es_get_data()`, `es_search_tables()`, `es_get_codes()` och `es_get_comments()`:
  ny parameter `source`, `"eurostat"` (standard) eller `"oecd"`. Befintlig kod fungerar som förut.
* `es_get_data()` med OECD ger kolumnen `flagga` med värdets status och `UNIT_MULT`.

# intstatgui 0.1.0

* Första versionen.
* `es_gui()` och `es_gui_addin()`: grafiskt gränssnitt och addin för RStudio och Positron.
  Bläddra bland eller sök i Eurostats tabeller, välj variabler och värden och få färdig kod,
  eller hämta data direkt och ladda ner det som txt, Excel eller parquet.
* `es_get_data()`: hämtar en eller flera tabeller med namn på värdena och valfria kodkolumner.
* `es_search_tables()`, `es_get_codes()` och `es_get_comments()`: tabellistan, en tabells
  variabler och värden samt uppgifter om en tabell.
