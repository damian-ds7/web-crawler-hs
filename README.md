# Programowanie Funkcyjne - Web Crawler (Haskell)

## Opis projektu

Projekt to wielowątkowy **Web Crawler** napisany w języku Haskell. Aplikacja
służy do automatycznego przeszukiwania sieci, ekstrakcji linków oraz zarządzania
stanem odwiedzonych adresów URL, kładąc szczególny nacisk na bezpieczeństwo
wątkowe.

## Funkcjonalności:

- **Wielowątkowość i współbieżność** - wykorzystanie biblioteki `async` do
  równoległego przetwarzania wielu stron jednocześnie. Liczba wątków jest
  konfigurowalna.
- **Zarządzanie stanem (STM)** - użycie *Software Transactional Memory* do
  bezpiecznego współdzielenia kolejki URL-i, zbioru odwiedzonych oraz
  zablokowanych stron między wątkami, co eliminuje ryzyko zakleszczeń.
- **Obsługa Robots.txt** - automatyczne pobieranie i parsowanie plików zasad dla
  robotów. Crawler respektuje reguły `Disallow` i zapisuje je, aby minimalizować
  ruch sieciowy.
- **Automatyczne blokowanie domen (Anti-Spam/Rate Limit)** - mechanizm
  wykrywający błąd HTTP 429 (*Too Many Requests*). Jeśli domena zacznie
  limitować połączenia, zostaje ona automatycznie dodana do zbioru
  zablokowanych, a dalsze zapytania do niej są przerywane.
- **Ekstrakcja linków za pomocą Regex** - wyszukiwanie znaczników
  `<a href="...">` przy użyciu biblioteki `regex-tdfa` bezpośrednio na surowym
  strumieniu danych (ByteString).
- **Normalizacja adresów URL** - konwersja ścieżek względnych na absolutne oraz
  ekstrakcja domen bazowych dla potrzeb weryfikacji zasad dostępu.
- **System logowania** - wbudowany logger z precyzyjnym czasem, kategoryzujący
  zdarzenia (`Info`, `Warn`, `Error`).

## Przykłady paradygmatu funkcyjnego w projekcie:

- **Algebraiczne Typy Danych (ADT)** - modelowanie błędów pobierania
  (`FetchError`) oraz poziomów logowania jako sumarycznych typów danych, co
  wymusza pełną obsługę przypadków.

```haskell
data FetchError
  = TransportError HttpException
  | HttpStatusError Int
  | DomainBlocked
  deriving (Show)
```

- **Software Transactional Memory (STM)** - bezpieczne współbieżnie zarządzanie
  stanem, transakcje STM są atomowe i kompozytowalne, co eliminuje potrzebę
  ręcznych blokad i ryzyko deadlocków.

```haskell
blockDomain :: State -> URL -> IO Bool
blockDomain state baseURL = atomically $ do
  blocked <- readTVar (blockedDomains state)
  if Set.member baseURL blocked
    then return False
    else do
      modifyTVar (blockedDomains state) (Set.insert baseURL)
      return True
```

- **Funkcje wyższego rzędu i transformacje** - intensywne użycie `fmap`,
  `filter` oraz `map` do transformacji danych z sieci na listy znormalizowanych
  linków.

```haskell
let foundURLs  = parseLinks body                          -- Ekstrakcja surowych linków
    nonEmpty   = filter (not . BS.null) foundURLs         -- Usunięcie pustych wpisów
    normalized = map (normalizeURL baseURL) nonEmpty      -- Zamiana na adresy absolutne
atomically $ do
  visited <- readTVar (visitedURLs state)
  let unvisited = filter (`Set.notMember` visited) normalized -- Filtrowanie unikalnych
```

## Ograniczenia:

- **Głębokość przeszukiwania** - crawler posiada limit głębokości, aby zapobiec
  nieskończonej rekurencji w przypadku "pułapek na roboty".
- **Pamięć podręczna** - cache plików `robots.txt` oraz lista zablokowanych
  domen są przechowywane w pamięci RAM (ulotne po restarcie aplikacji).
