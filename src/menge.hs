-- Menge Typklasse Datei
-- Datum: 26.11.24

module Menge where


type Fehlermeldung        = String
type MengeAlsZeichenreihe = String

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


    -- PROTOIMPLEMENTIERUNGEN --

    istKeinGueltigerMengenwert = error
    nichtImplementierbar = error
    komplementiere = zieheab allMenge

    -- Zwei Mengen sind gleich, wenn sie Teilmengen voneinander sind
    sindGleich m1 m2 =
        istTeilmenge m1 m2
        && istTeilmenge m2 m1

    -- Eine Menge ist echte Teilmenge einer Anderen, wenn sie Teilmenge aber nicht gleich ist
    istEchteTeilmenge m1 m2 = istTeilmenge m1 m2 && not (sindGleich m1 m2)

    -- Wenn A (echte) Obermenge von B ist, ist dann B (echte) Teilmenge von A
    istObermenge m1 m2 = istTeilmenge m2 m1
    istEchteObermenge m1 m2 = istEchteTeilmenge m2 m1

    -- Zwei Mengen sind elementefremd, wenn ihrer Schnitt die Leeremenge ist
    sindElementeFremd m1 = sindGleich leereMenge . schneide m1

    -- Zwei Mengen sind quer-ueberlappend, wenn sie...
    --   ... mindestens ein Element gemeinsam haben
    --   ... jeweils keine Teilmenge voneinander sind
    sindQuerUeberlappend m1 m2 =
        not (sindElementeFremd m1 m2)
        && not (istTeilmenge m1 m2)
        && not (istTeilmenge m2 m1)
