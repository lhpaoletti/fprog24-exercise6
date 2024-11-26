-- Menge Typklasse Datei
-- Datum: 26.11.24

module Menge where
import Types


class Menge m where
    leereMenge :: m
    allMenge   :: m
    istMenge :: m -> Bool
    vereinige :: m -> m -> m
    schneide  :: m -> m -> m
    zieheab   :: m -> m -> m
    komplementiere :: m -> m
    sindGleich   :: m -> m -> Bool
    istTeilmenge :: m -> m -> Bool
    istObermenge :: m -> m -> Bool
    istEchteTeilmenge :: m -> m -> Bool
    istEchteObermenge :: m -> m -> Bool
    sindElementeFremd :: m -> m -> Bool
    sindQuerUeberlappend :: m -> m -> Bool
    istKeinGueltigerMengenwert :: Fehlermeldung -> m
    nichtImplementierbar :: Fehlermeldung -> m
    zeige :: m -> MengeAlsZeichenreihe

    -- Protoimplementierungen
    istKeinGueltigerMengenwert = error
    nichtImplementierbar       = error
