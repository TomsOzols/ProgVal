listLengthRecursion :: [a] -> Int -> Int
listLengthRecursion [] counter = counter
listLengthRecursion (head:tail) counter = listLengthRecursion tail (counter + 1)

-- Parada manu slinkumu apskatit dokumentaciju
-- Update: apskatijis dokumentaciju - izradas ka bazes length algoritms prieks sarakstiem tapatas ir O(n).
listLength :: [a] -> Int
listLength list = listLengthRecursion list 0

getKeyFromTuple :: (a, a) -> a
getKeyFromTuple (key, _) = key

getValueFromTuple :: (a, a) -> a
getValueFromTuple (_, value) = value

-- Ideala varianta - te deretu kadu Maybe saveidot, vai ari blaut ka klume.
takeByKey :: Eq a => [(a, a)] -> a -> a
takeByKey [] key = key -- Sheit atstaju mulkibas slinkuma del
takeByKey (head:tail) key =
    if key == getKeyFromTuple head
        then getValueFromTuple head
        else takeByKey(tail) key

existsInList :: (a -> a -> Bool) -> [a] -> a -> Bool
existsInList compareFunction [] lookingFor = False
existsInList compareFunction (head:tail) lookingFor =
    if compareFunction lookingFor head
        then True
        else existsInList compareFunction tail lookingFor
        
myUnique :: (a -> a -> Bool) -> [a] -> [a]
myUnique compareFunction [] = []
myUnique compareFunction (head:tail) =
    if (existsInList compareFunction tail head)
        then myUnique compareFunction tail
        else head : myUnique compareFunction tail


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter predicateFunction [] = []
myFilter predicateFunction (head:tail) =
    if (predicateFunction head)
        then head : filter predicateFunction tail
        else filter predicateFunction tail

myMap :: (a -> b) -> [a] -> [b]
myMap function [] = []
myMap function (head:tail) =
    function head : myMap function tail

-- Slinkums uz doto momentu petit specigakus valodas lidzeklus lai nebutu nepieciesams pardefinet funkciju tik vien prieks papildus argumenta ieguves
myMapIndexedRecursion :: (a -> Int -> b) -> Int -> [a] -> [b]
myMapIndexedRecursion function index [] = []
myMapIndexedRecursion function index (head:tail) =
    function head index : myMapIndexedRecursion function (index + 1) tail

myMapIndexed :: (a -> Int -> b) -> [a] -> [b]
myMapIndexed function list = myMapIndexedRecursion function 0 list

equalToTupleKey :: Eq a => a -> (a, a) -> Bool
equalToTupleKey compareValue tuple =
    if (getKeyFromTuple tuple) == compareValue
        then True
        else False

constructFromBoth :: (a, a) -> (a, a) -> (a, a)
constructFromBoth tuple_A tuple_B =
    (getKeyFromTuple tuple_A, getValueFromTuple tuple_B)

-- Vajadzetu atgriezties te un novienkarsot pierakstu. Komentars - gada ironija.
takeAllWithKey :: Eq a => [(a, a)] -> (a, a) -> [(a, a)]
takeAllWithKey dictionary keyValue =
    myMap
        (constructFromBoth keyValue)
        (myFilter (
            equalToTupleKey (
                getValueFromTuple keyValue
            )
        ) dictionary)

-- Varetu parveidot uz kadu redukcijas funkciju kas pienem paskaidrojumu tam ka kombinet jaunos sarakstus.
forEachA :: ((a, a) -> [(a, a)]) -> [(a, a)] -> [(a, a)]
forEachA function [] = []
forEachA function (head:tail) = function head ++ forEachA function tail

tupleCompare :: Eq a => (a, a) -> (a, a) -> Bool
tupleCompare tuple_A tuple_B =
    getKeyFromTuple tuple_A == getKeyFromTuple tuple_B &&
    getValueFromTuple tuple_A == getValueFromTuple tuple_B

prettyResult :: a -> Int -> (Int, a)
prettyResult value index = (index + 1, value)

-- Pievienoju katru jaunako elementu saraksta sakuma
-- Noslinkots - atstaju 2 let.
-- goTillPA :: [[(String, String)]] -> [[(String, String)]]
-- goTillPA (head:tail) =
--     let newDictionary = bb head head in
--         let combined = myUnique tupleCompare (head ++ newDictionary) in
--             if listLength head == listLength combined
--                 then (head:tail)
--                 else goTillPA (combined:head:tail)

aa :: Eq a => [a] -> [(a, a)] -> [a]
aa keys dictionary = myUnique (==) (myMap (takeByKey dictionary) keys)

-- Seit drosvien jau diezgan acimredzami ka butu labak definet savu tipu, bet rokas necelas.
bb :: Eq a => [(a, a)] -> [(a, a)] -> [(a, a)]
bb dictionary_A dictionary_B = myUnique (tupleCompare) (forEachA (takeAllWithKey dictionary_B) dictionary_A)

-- Slinkums rakstit un formalizet funkciju kas nosaka kads ir skaitlis P(A)
-- Bet vismaz idejas limeni - vispirms varam noverot to ka P(A) nekad nebus lielaks par originalas vardnicas elementu skaitu
-- Talak padomaju, bet neizdomaju.
-- Bet attiecigi, ja akli laizham algoritmu, P(A) = n, ir lielakais, tad ja |A<n>| == |A<n+1>|, vai vienkarsak, ja nakamaja iteracija nepieaug elementu skaits.
-- kk :: [(String, String)] -> [(Int, [(String, String)])]
-- kk dictionary = myMapIndexed prettyResult (reverse (goTillPA [dictionary]))

aa1 :: [String]
aa1 = aa 
    ["b", "c", "d", "e", "f"]
    [("a", "My"), ("b", "cat"), ("c", "ate"), ("d", "my"), ("e", "cat"), ("f", "My")]
    -- ["ate","my","cat","My"]

aa2 :: [Int]
aa2 = aa
    [2, 3, 4, 5, 9, 8]
    [(1, 100), (2, 200), (3, 300), (4, 200), (5, 600), (6, 700), (9, 1), (8, 5)]
    -- [300,200,600,1,5]

bb1 :: [(String, String)]
bb1 = bb
    [("CAT", "DOG"), ("CAT", "SNAKE"), ("ELEPHANT", "SNAKE"), ("!", "?"), ("?", "!")]
    [("DOG", "MOUSE"), ("DOG", "GIRAFFE"), ("SNAKE", "GIRAFFE"), ("?", "PIG")]
    -- [("CAT","MOUSE"),("CAT","GIRAFFE"),("ELEPHANT","GIRAFFE"),("!","PIG")]

bb2 :: [(Int, Int)]
bb2 = bb
    [(1, 100), (2, 300), (4, 5), (5, 4), (6, 300)]
    [(300, 1000), (300, 2000), (300, 5000), (5, 8), (5, 100)]
    -- [(2,1000),(2,2000),(2,5000),(4,8),(4,100),(6,1000),(6,2000),(6,5000)]

-- kkTest :: [(Int, [(String, String)])]
-- kkTest = kk [("1","2"),("2","3"),("3","1")]

-- kkTest2 :: [(Int, [(String, String)])]
-- kkTest2 = kk [("1","2"),("2","3"),("4","2")]
-- -- [(1,[("1","2"),("2","3"),("4","2")]),
-- -- (2,[("1","2"),("2","3"),("4","2"),("1","3"),("4","3")])]

-- kkTest3 :: [(Int, [(String, String)])]
-- kkTest3 = kk [("1","2"),("2","3"),("4","1")]
-- -- [(1,[("1","2"),("2","3"),("4","1")]),
-- -- (2,[("1","2"),("2","3"),("4","1"),("1","3"),("4","2")]),
-- -- (3,[("1","2"),("2","3"),("4","1"),("1","3"),("4","2"),("4","3")])]
