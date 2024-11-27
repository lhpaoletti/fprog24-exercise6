-- MT1 Typ Datei
-- Datum: 26.11.24

module MT1 where
import Menge


newtype MT1 e = MT1 [e]

instance Menge (MT1 Char) where
    leereMenge = leereMenge'
    allMenge   = MT1 $ ['a'..'z'] ++ ['A'..'Z']
    istMenge   = istMenge'

    vereinige = vereinige'
    schneide  = schneide'
    zieheab   = zieheab'

    istTeilmenge = istTeilmenge'

    zeige = zeige'


instance Menge (MT1 Int) where
    leereMenge = leereMenge'
    allMenge = MT1 [(-100)..100]
    istMenge = istMenge'

    vereinige = vereinige'
    schneide  = schneide'
    zieheab   = zieheab'

    istTeilmenge = istTeilmenge'

    zeige = zeige'


leereMenge' :: MT1 a
leereMenge' = MT1 []

istMenge' :: Eq a => MT1 a -> Bool
istMenge' (MT1     []) = True
istMenge' (MT1 (_:[])) = True
istMenge' (MT1 (e:es)) = all (/= e) es && istMenge' (MT1 es)

vereinige' :: Eq a => MT1 a -> MT1 a -> MT1 a
vereinige' m1@(MT1 elems1) m2@(MT1 elems2)
    | not (istMenge' m1 && istMenge' m2) = fehlermeldung
    -- Entferne die Duplikate der Konkatenation der beiden Mengen
    | otherwise = MT1 . nub $ elems1 ++ elems2

schneide' :: Eq a => MT1 a -> MT1 a -> MT1 a
schneide' m1@(MT1 elems1) m2@(MT1 elems2)
    | not (istMenge' m1 && istMenge' m2) = fehlermeldung
    -- Lasse nur die Duplikate der Konkatenation der beiden Mengen drin
    | otherwise = MT1 . dup $ elems1 ++ elems2

zieheab' :: Eq a => MT1 a -> MT1 a -> MT1 a
zieheab' m1@(MT1 elems1) m2@(MT1 elems2)
    | not (istMenge' m1 && istMenge' m2) = fehlermeldung
    -- Nimm nur die Elemente der ersten Menge, die nicht in der Zweiten vorkommen
    | otherwise = MT1 $ [e | e <- elems1, e `notElem` elems2]

istTeilmenge' :: Eq a => MT1 a -> MT1 a -> Bool
istTeilmenge' m1@(MT1 elems1) m2@(MT1 elems2)
    | not (istMenge' m1 && istMenge' m2) = fehlermeldung
    | otherwise = all (`elem` elems2) elems1

zeige' :: Show a => MT1 a -> MengeAlsZeichenreihe
zeige' (MT1 elems) = "{" ++ formatElems elems ++ "}"



-- Fehlermeldung fuer wenn ein oder mehrere Argumente nicht Menge sind.
fehlermeldung :: a
fehlermeldung = error "Argument muss Menge sein (keine Duplikate)"

-- Entferne Duplikate einer Liste.
nub :: Eq a => [a] -> [a]
nub []     = []
nub (e:es) = e : (nub $ filter (/= e) es)

-- Lasse nur Duplikate einer Liste bleiben.
dup :: Eq a => [a] -> [a]
dup [] = []
dup (e:es)
    | e `elem` es = e : (dup es)
    | otherwise   = dup es

-- Formatiere Elemente, um sie auszudrucken.
formatElems :: Show a => [a] -> String
formatElems []     = ""
formatElems [e]    = show e
formatElems (e:es) = show e ++ ", " ++ formatElems es



main = do
    putStrLn "------------------------------Char------------------------------"
    putStrLn ""
    putStrLn $ "leereMenge: " ++ zeige (leereMenge :: MT1 Char)
    putStrLn $ "allMenge  : " ++ zeige (allMenge   :: MT1 Char)
    putStrLn ""
    putStrLn $ "istMenge         {}: " ++ (show $ istMenge (leereMenge :: MT1 Char))
    putStrLn $ "istMenge {'a', 'a'}: " ++ (show $ istMenge $ MT1 "aa")
    putStrLn $ "istMenge {'a', 'b'}: " ++ (show $ istMenge $ MT1 "ab")
    putStrLn ""
    putStrLn $ "vereinige    {} {'a'}: " ++ (zeige . vereinige leereMenge $ MT1 "a")
    putStrLn $ "vereinige {'a'} {'a'}: " ++ (zeige $ vereinige (MT1 "a") (MT1 "a"))
    putStrLn $ "vereinige {'a'} {'b'}: " ++ (zeige $ vereinige (MT1 "a") (MT1 "b"))
    putStrLn ""
    putStrLn $ "schneide         {} {'a'}: " ++ (zeige . schneide leereMenge $ MT1 "a")
    putStrLn $ "schneide      {'a'} {'a'}: " ++ (zeige $ schneide (MT1 "a") (MT1 "a"))
    putStrLn $ "schneide {'a'} {'a', 'b'}: " ++ (zeige $ schneide (MT1 "a") (MT1 "ab"))
    putStrLn ""
    putStrLn $ "zieheab         {} {'a'}: " ++ (zeige . zieheab leereMenge $ MT1 "a")
    putStrLn $ "zieheab      {'a'} {'a'}: " ++ (zeige $ zieheab (MT1 "a") (MT1 "a"))
    putStrLn $ "zieheab {'a', 'b'} {'a'}: " ++ (zeige $ zieheab (MT1 "ab") (MT1 "a"))
    putStrLn ""
    putStrLn $ "komplementiere . zieheab allMenge $ {'a'}: " ++ (zeige . komplementiere . zieheab allMenge $ MT1 "a")
    putStrLn ""
    putStrLn $ "sindGleich           {'a'} {'a'}: " ++ (show $ sindGleich (MT1  "a") (MT1  "a"))
    putStrLn $ "sindGleich           {'a'} {'b'}: " ++ (show $ sindGleich (MT1  "a") (MT1  "b"))
    putStrLn $ "sindGleich {'a', 'b'} {'b', 'a'}: " ++ (show $ sindGleich (MT1 "ab") (MT1 "ba"))
    putStrLn ""
    putStrLn $ "istTeilmenge           {'a'} {'a'}: " ++ (show $ istTeilmenge (MT1  "a") (MT1  "a"))
    putStrLn $ "istTeilmenge {'a', 'b'} {'b', 'a'}: " ++ (show $ istTeilmenge (MT1 "ab") (MT1 "ba"))
    putStrLn $ "istTeilmenge      {'a'} {'a', 'b'}: " ++ (show $ istTeilmenge (MT1  "a") (MT1 "ab"))
    putStrLn $ "istTeilmenge      {'a', 'b'} {'a'}: " ++ (show $ istTeilmenge (MT1 "ab") (MT1  "a"))
    putStrLn ""
    putStrLn $ "istObermenge           {'a'} {'a'}: " ++ (show $ istObermenge (MT1  "a") (MT1  "a"))
    putStrLn $ "istObermenge {'a', 'b'} {'b', 'a'}: " ++ (show $ istObermenge (MT1 "ab") (MT1 "ba"))
    putStrLn $ "istObermenge      {'a'} {'a', 'b'}: " ++ (show $ istObermenge (MT1  "a") (MT1 "ab"))
    putStrLn $ "istObermenge      {'a', 'b'} {'a'}: " ++ (show $ istObermenge (MT1 "ab") (MT1  "a"))
    putStrLn ""
    putStrLn ""
    putStrLn "------------------------------Int------------------------------"
    putStrLn ""
    putStrLn $ "leereMenge: " ++ zeige (leereMenge :: MT1 Int)
    putStrLn $ "allMenge  : " ++ zeige (allMenge   :: MT1 Int)
    putStrLn ""
    putStrLn $ "istMenge     {}: " ++ (show $ istMenge (leereMenge :: MT1 Int))
    putStrLn $ "istMenge {1, 1}: " ++ (show $ istMenge $ MT1 [1, 1 :: Int])
    putStrLn $ "istMenge {1, 2}: " ++ (show $ istMenge $ MT1 [1, 2 :: Int])
    putStrLn ""
    putStrLn $ "vereinige  {} {1}: " ++ (zeige . vereinige leereMenge $ MT1 [1 :: Int])
    putStrLn $ "vereinige {1} {1}: " ++ (zeige $ vereinige (MT1 [1]) (MT1 [1 :: Int]))
    putStrLn $ "vereinige {1} {2}: " ++ (zeige $ vereinige (MT1 [1]) (MT1 [2 :: Int]))
    putStrLn ""
    putStrLn $ "schneide     {} {1}: " ++ (zeige . schneide leereMenge $ MT1 [1 :: Int])
    putStrLn $ "schneide    {1} {1}: " ++ (zeige $ schneide (MT1 [1]) (MT1    [1 :: Int]))
    putStrLn $ "schneide {1} {1, 2}: " ++ (zeige $ schneide (MT1 [1]) (MT1 [1, 2 :: Int]))
    putStrLn ""
    putStrLn $ "zieheab     {} {1}: " ++ (zeige . zieheab leereMenge $ MT1 [1 :: Int])
    putStrLn $ "zieheab    {1} {1}: " ++ (zeige $ zieheab (MT1    [1]) (MT1 [1 :: Int]))
    putStrLn $ "zieheab {1, 2} {1}: " ++ (zeige $ zieheab (MT1 [1, 2]) (MT1 [1 :: Int]))
    putStrLn ""
    putStrLn $ "komplementiere . zieheab allMenge $ {1}: " ++ (zeige . komplementiere . zieheab allMenge $ MT1 [1 :: Int])
    putStrLn ""
    putStrLn $ "sindGleich       {1} {1}: " ++ (show $ sindGleich (MT1    [1]) (MT1    [1 :: Int]))
    putStrLn $ "sindGleich       {1} {2}: " ++ (show $ sindGleich (MT1    [1]) (MT1    [2 :: Int]))
    putStrLn $ "sindGleich {1, 2} {2, 1}: " ++ (show $ sindGleich (MT1 [1, 2]) (MT1 [2, 1 :: Int]))
    putStrLn ""
    putStrLn $ "istTeilmenge       {1} {1}: " ++ (show $ istTeilmenge (MT1    [1]) (MT1    [1 :: Int]))
    putStrLn $ "istTeilmenge {1, 2} {2, 1}: " ++ (show $ istTeilmenge (MT1 [1, 2]) (MT1 [2, 1 :: Int]))
    putStrLn $ "istTeilmenge    {1} {1, 2}: " ++ (show $ istTeilmenge (MT1    [1]) (MT1 [1, 2 :: Int]))
    putStrLn $ "istTeilmenge    {1, 2} {1}: " ++ (show $ istTeilmenge (MT1 [1, 2]) (MT1    [1 :: Int]))
    putStrLn ""
    putStrLn $ "istObermenge       {1} {1}: " ++ (show $ istObermenge (MT1    [1]) (MT1    [1 :: Int]))
    putStrLn $ "istObermenge {1, 2} {2, 1}: " ++ (show $ istObermenge (MT1 [1, 2]) (MT1 [2, 1 :: Int]))
    putStrLn $ "istObermenge    {1} {1, 2}: " ++ (show $ istObermenge (MT1    [1]) (MT1 [1, 2 :: Int]))
    putStrLn $ "istObermenge    {1, 2} {1}: " ++ (show $ istObermenge (MT1 [1, 2]) (MT1    [1 :: Int]))
