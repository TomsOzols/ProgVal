listLengthRecursion :: [a] -> Int -> Int
listLengthRecursion [] counter = counter
listLengthRecursion (head:tail) counter = listLengthRecursion tail (counter + 1)

-- Parada manu slinkumu apskatit dokumentaciju
-- Update: apskatijis dokumentaciju - izradas ka bazes length algoritms prieks sarakstiem tapatas ir O(n).
listLength :: [a] -> Int
listLength list = listLengthRecursion list 0

getKeyFromTuple :: (String, String) -> String
getKeyFromTuple (key, _) = key

getValueFromTuple :: (String, String) -> String
getValueFromTuple (_, value) = value

-- Ideala varianta - te deretu kadu Maybe saveidot, vai ari blaut ka klume.
takeByKey :: [(String, String)] -> String -> String
takeByKey [] key = "NOTHING"
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

equalToTupleKey :: String -> (String, String) -> Bool
equalToTupleKey compareValue tuple =
    if (getKeyFromTuple tuple) == compareValue
        then True
        else False

constructFromBoth :: (String, String) -> (String, String) -> (String, String)
constructFromBoth tuple_A tuple_B =
    (getKeyFromTuple tuple_A, getValueFromTuple tuple_B)

-- Vajadzetu atgriezties te un novienkarsot pierakstu. Komentars - gada ironija.
takeAllWithKey :: [(String, String)] -> (String, String) -> [(String, String)]
takeAllWithKey dictionary keyValue =
    myMap
        (constructFromBoth keyValue)
        (myFilter (
            equalToTupleKey (
                getValueFromTuple keyValue
            )
        ) dictionary)

-- Varetu parveidot uz kadu redukcijas funkciju kas pienem paskaidrojumu tam ka kombinet jaunos sarakstus.
forEachA :: ((String, String) -> [(String, String)]) -> [(String, String)] -> [(String, String)]
forEachA function [] = []
forEachA function (head:tail) = function head ++ forEachA function tail

tupleCompare :: (String, String) -> (String, String) -> Bool
tupleCompare tuple_A tuple_B =
    getKeyFromTuple tuple_A == getKeyFromTuple tuple_B &&
    getValueFromTuple tuple_A == getValueFromTuple tuple_B

prettyResult :: a -> Int -> (Int, a)
prettyResult value index = (index + 1, value)

-- Pievienoju katru jaunako elementu saraksta sakuma
-- Noslinkots - atstaju 2 let.
goTillPA :: [[(String, String)]] -> [[(String, String)]]
goTillPA (head:tail) =
    let newDictionary = bb head head in
        let combined = myUnique tupleCompare (head ++ newDictionary) in
            if listLength head == listLength combined
                then (head:tail)
                else goTillPA (combined:head:tail)


aa :: [String] -> [(String, String)] -> [String]
aa keys dictionary = myUnique (==) (myMap (takeByKey dictionary) keys)

-- Seit drosvien jau diezgan acimredzami ka butu labak definet savu tipu, bet rokas necelas.
bb :: [(String, String)] -> [(String, String)] -> [(String, String)]
bb dictionary_A dictionary_B = myUnique (tupleCompare) (forEachA (takeAllWithKey dictionary_B) dictionary_A)

-- Slinkums rakstit un formalizet funkciju kas nosaka kads ir skaitlis P(A)
-- Bet vismaz idejas limeni - vispirms varam noverot to ka P(A) nekad nebus lielaks par originalas vardnicas elementu skaitu
-- Talak padomaju, bet neizdomaju.
-- Bet attiecigi, ja akli laizham algoritmu, P(A) = n, ir lielakais, tad ja |A<n>| == |A<n+1>|, vai vienkarsak, ja nakamaja iteracija nepieaug elementu skaits.
kk :: [(String, String)] -> [(Int, [(String, String)])]
kk dictionary = myMapIndexed prettyResult (reverse (goTillPA [dictionary]))

aaTest :: [String]
aaTest =
    aa 
        ["b", "c", "d", "e", "f"]
        [("a", "My"), ("b", "cat"), ("c", "ate"), ("d", "my"), ("e", "cat"), ("f", "My")]
        -- ["ate","my","cat","My"]

bbTest :: [(String, String)]
bbTest =
    bb
        [("CAT", "DOG"), ("CAT", "SNAKE"), ("ELEPHANT", "SNAKE")]
        [("DOG", "MOUSE"), ("DOG", "GIRAFFE"), ("SNAKE", "GIRAFFE")]
        -- [("CAT","MOUSE"),("CAT","GIRAFFE"),("ELEPHANT","GIRAFFE")]

kkTest :: [(Int, [(String, String)])]
kkTest = kk [("1","2"),("2","3"),("3","1")]

kkTest2 :: [(Int, [(String, String)])]
kkTest2 = kk [("1","2"),("2","3"),("4","2")]
-- [(1,[("1","2"),("2","3"),("4","2")]),
-- (2,[("1","2"),("2","3"),("4","2"),("1","3"),("4","3")])]

kkTest3 :: [(Int, [(String, String)])]
kkTest3 = kk [("1","2"),("2","3"),("4","1")]
-- [(1,[("1","2"),("2","3"),("4","1")]),
-- (2,[("1","2"),("2","3"),("4","1"),("1","3"),("4","2")]),
-- (3,[("1","2"),("2","3"),("4","1"),("1","3"),("4","2"),("4","3")])]
