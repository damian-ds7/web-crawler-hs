### 1. Architektura ogólna

System zostanie zaprojektowany jako pipeline przetwarzania danych, gdzie każda
faza odpowiada za konkretny etap przetwarzania strony.

#### Wejście:

- Pojedynczy URL (seed)

#### Wyjście:

- Lista odwiedzonych URL-i

### 2. Pipeline przetwarzania

Dla każdego URL-a wykonywany będzie następujący ciąg operacji:

#### 2.1 Pobranie strony

- Wysłanie zapytania HTTP GET
- Biblioteka: `req`

#### 2.2 Parsowanie HTML

- Konwersja surowego HTML do struktury możliwej do analizy
- Wyszukiwanie elementów (np. `<a href="...">`)
- Wyciąganie linków i innych danych
- Zamiana linków względnych na absolutne
- Usuwanie duplikatów i fragmentów URL
- Bibbliteki: `scalpel`, `network-uri`

#### 2.3 Filtrowanie

- Sprawdzenie, czy URL był już odwiedzony
- Ograniczenie do wybranej domeny

#### 2.6. Dodanie do kolejki

- Nowe URL-e trafiają do kolejki przetwarzania

### 3. Zarządzanie stanem

Crawler będzie utrzymywał globalny stan:

- Zbiór odwiedzonych URL-i
- Kolejkę URL-i do odwiedzenia

Stan współdzielony będzie zarządzany przy użyciu STM, co zapewni bezpieczną
współbieżność.

### 4. Współbieżność

System będzie wykorzystywał lekkie wątki:

- Wiele workerów pobierających strony równolegle
- Możliwość ograniczenia liczby równoległych zapytań

### 6. Planowane dodatkowe funkcjonalności

- Parsowanie pliku `robots.txt`
