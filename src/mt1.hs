-- MT1 Typ Datei
-- Datum: 26.11.24

module MT1 where
import Menge


newtype MT1 e = MT1 [e]

instance Menge (MT1 Char) where
    leereMenge = MT1 []
    allMenge   = MT1 $ ['a'..'z'] ++ ['A'..'Z']

    istMenge (MT1     []) = True
    istMenge (MT1 (_:[])) = True
    istMenge (MT1 (e:es)) = all (/= e) es && istMenge (MT1 es)

    vereinige m1@(MT1 elems1) m2@(MT1 elems2)
        | not (istMenge m1 && istMenge m2) = fehlermeldung
        -- Entferne die Duplikate der Konkatenation der beiden Mengen
        | otherwise = MT1 . nub $ elems1 ++ elems2

    schneide m1@(MT1 elems1) m2@(MT1 elems2)
        | not (istMenge m1 && istMenge m2) = fehlermeldung
        -- Lasse nur die Duplikate der Konkatenation der beiden Mengen drin
        | otherwise = MT1 . dup $ elems1 ++ elems2

    zieheab m1@(MT1 elems1) m2@(MT1 elems2)
        | not (istMenge m1 && istMenge m2) = fehlermeldung
        -- Nimm nur die Elemente der ersten Menge, die nicht in der Zweiten vorkommen
        | otherwise = MT1 $ [e | e <- elems1, e `notElem` elems2]

    istTeilmenge m1@(MT1 elems1) m2@(MT1 elems2)
        | not (istMenge m1 && istMenge m2) = error "Argument muss Menge sein (keine Duplikate)"
        | otherwise = all (`elem` elems2) elems1

    zeige (MT1 elems) = "{" ++ formatElems elems ++ "}"



-- Fehlermeldung fuer wenn ein oder mehrere Argumente nicht Menge sind.
fehlermeldung :: (MT1 Char)
fehlermeldung = istKeinGueltigerMengenwert "Argument muss Menge sein (keine Duplikate)"

-- Entferne Duplikate einer Liste.
nub :: [Char] -> [Char]
nub []     = []
nub (e:es) = e : (nub $ filter (/= e) es)

-- Lasse nur Duplikate einer Liste bleiben.
dup :: [Char] -> [Char]
dup [] = []
dup (e:es)
    | e `elem` es = e : (dup es)
    | otherwise   = dup es

-- Formatiere Chars, um sie auszudrucken.
formatElems :: [Char] -> String
formatElems []     = ""
formatElems [e]    = show e
formatElems (e:es) = show e ++ ", " ++ formatElems es
