
getKeyFromTuple :: (String, String) -> String
getKeyFromTuple (key, _) = key

getValueFromTuple :: (String, String) -> String
getValueFromTuple (_, value) = value

-- Seit deretu parveidot uz Maybe monadi
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

-- myUnique :: [String] -> [String]
-- myUnique [] = []
-- myUnique (head:tail) =
--     if (stringExistsInList tail head)
--         then myUnique tail
--         else head : myUnique tail

myMap :: (a -> b) -> [a] -> [b]
myMap function [] = []
myMap function (head:tail) =
    function head : myMap function tail

-- Slinkums uz doto momentu petit specigakus valodas lidzeklus lai nebutu nepieciesams pardefinet funkciju tik vien prieks papildus argumenta ieguves
myMapIndexedRecursion :: (a -> Int -> b) -> Int -> [a] -> [b]
myMapIndexedRecursion function index [] = []
myMapIndexedRecursion function index (head:tail) =
    function index head : myMap function (index + 1) tail

myMapIndexed :: (a -> Int -> b) -> [a] -> [b]
myMapIndexed function list = myMapIndexedRecursion function 1 list

aa :: [String] -> [(String, String)] -> [String]
aa keys dictionary = myUnique (==) (myMap (takeByKey dictionary) keys)

aaTest :: [String]
aaTest =
    aa 
        ["b", "c", "d", "e", "f"]
        [("a", "My"), ("b", "cat"), ("c", "ate"), ("d", "my"), ("e", "cat"), ("f", "My")]

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

forEachA :: ((String, String) -> [(String, String)]) -> [(String, String)] -> [(String, String)]
forEachA function [] = []
forEachA function (head:tail) = function head ++ forEachA function tail

tupleCompare :: (String, String) -> (String, String) -> Bool
tupleCompare tuple_A tuple_B =
    getKeyFromTuple tuple_A == getKeyFromTuple tuple_B &&
    getValueFromTuple tuple_A == getValueFromTuple tuple_B

-- Seit drosvien jau diezgan acimredzami ka varbut butu labak definet savu tipu, bet rokas necelas.
bb :: [(String, String)] -> [(String, String)] -> [(String, String)]
bb dictionary_A dictionary_B = myUnique (tupleCompare) (forEachA (takeAllWithKey dictionary_B) dictionary_A)

bbTest :: [(String, String)]
bbTest =
    bb
        [("CAT", "DOG"), ("CAT", "SNAKE"), ("ELEPHANT", "SNAKE")]
        [("DOG", "MOUSE"), ("DOG", "GIRAFFE"), ("SNAKE", "GIRAFFE")]

cycleTillNoMore :: (String, String) -> Int -> (Int, [(String, String)])


kk :: [(String, String)] -> [(Int, [(String, String)])]
kk dictionary = myMapIndexed cycleTillNoMore dictionary