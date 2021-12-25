-- 1
myenumFromTo :: Int -> Int -> [Int]
myenumFromTo x y 
    | y >= x = x : myenumFromTo (x+1) y
    | otherwise = []
-- 2
myenumFromThenTo :: Int -> Int -> Int -> [Int]
myenumFromThenTo x y z 
    | z >= x = x : myenumFromThenTo (y) (2*y-x) z
    | otherwise = []

-- 3
(+++) :: [a] -> [a] -> [a]
(+++) [] l = l
(+++) (x:xs) l = x : (+++) xs l


-- 4 
(!!!) :: [a] -> Int -> a
(!!!) (x:xs) pos
    | pos == 0 = x
    | otherwise = (!!!) xs (pos-1)


-- 5
myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = (myreverse xs) ++ [x]


-- 6
mytake :: Int -> [a] -> [a]
mytake y [] = []
mytake y (x:xs)
    | y <= 0 = []
    | otherwise = x : mytake (y-1) xs


-- 7
mydrop :: Int -> [a] -> [a]
mydrop y [] = []
mydrop y (x:xs)
    | y <= 0 = (x:xs)
    | otherwise = mydrop (y-1) xs


-- 8
myzip :: [a] -> [b] -> [(a,b)]
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys
myzip _ _ = []


-- 9
myreplicate :: Int -> a -> [a]
myreplicate y x
    | y == 0 = []
    | otherwise = x : myreplicate (y-1) x


-- 10
myintersperse :: a -> [a] -> [a]
myintersperse y [] = []
myintersperse y [x] = [x]
myintersperse y (x:xs) = x : y : myintersperse y xs


-- 11


-- 12
myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs


-- 13
myinits :: [a] -> [[a]]
myinits [] = [[]]
myinits l = myinits (init l) ++ [l]


-- 14
mytails :: [a] -> [[a]]
mytails [] = [[]]
mytails l = [l] ++ mytails (tail l)


-- 15
myheads :: [[a]] -> [a]
myheads [] = []
myheads ([]:xs) = myheads xs
myheads (x:xs) = (head x) : myheads xs


-- 16
conta :: [a] -> Int
conta [] = 0
conta (x:xs) = 1 + conta xs


total :: [[a]] -> Int
total [] = 0
total (x:xs) = (conta x) + total xs


-- 17
fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((a,b,c):xs) = (a,c) : fun xs


-- 18
cola :: [(String,b,c)] -> String
cola [] = ""
cola ((a,b,c):xs) = a ++ cola xs


-- 19
idade :: Int -> Int -> [(String,Int)] -> [String]
idade ano age [] = []
idade ano age ((a,b):xs)
    | ano - b >= age = a : idade ano age xs
    | otherwise = idade ano age xs


-- 20
powerEnumFromCount :: Int -> Int -> Int -> [Int]
powerEnumFromCount n m count 
    | m == count = []
    | otherwise = n^count : powerEnumFromCount n m (count+1)


powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n m = powerEnumFromCount n m 0 


-- 21
isPrimeAux :: Int -> Int -> Bool
isPrimeAux n m
    | m == n  = False
    | mod n m == 0 = True
    | otherwise = False || isPrimeAux n (m+1)

isPrime :: Int -> Bool
isPrime n = (isPrimeAux n 2) == False


-- 22
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] l = True
isPrefixOf l [] = False
isPrefixOf (x:xs) (y:ys)
    | x == y = True && isPrefixOf xs ys
    | otherwise = False


-- 23 
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] l = True
isSuffixOf l [] = False
isSuffixOf l (x:xs)
    | l == (x:xs) = True
    | otherwise = isSuffixOf l xs


-- 24
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) (y:ys) 
    | x == y = isSubsequenceOf xs ys
    | otherwise = isSubsequenceOf (x:xs) ys


-- 25
elemIndicesAux :: Eq a => Int -> a -> [a] -> [Int]
elemIndicesAux ind x [] = []
elemIndicesAux ind x (y:ys)
    | x == y = ind : elemIndicesAux (ind+1) x ys
    | otherwise = elemIndicesAux (ind+1) x ys


elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices x l = elemIndicesAux 0 x l


-- 26
nubAux :: Eq a => a -> [a] -> [a]
nubAux y [] = []
nubAux y (x:xs) 
    | y == x = nubAux y xs
    | otherwise = x : nubAux y xs


nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (nubAux x xs)


-- 27
delete :: Eq a => a -> [a] -> [a]
delete y [] = []
delete y (x:xs) | y==x = xs
                | otherwise = x : delete y xs


-- 28
(\\) :: Eq a => [a] -> [a] -> [a]
(\\) [] _ = []
(\\) l [] = l
(\\) l1 (h:t) = (\\) (delete h l1) t


-- 29
union :: Eq a => [a] -> [a] -> [a]
union l []= l
union l (x:xs)
    | elem x l = union l xs
    | otherwise = union (l ++ [x]) xs


-- 30
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect (x:xs) l
    | elem x l = x : intersect xs l
    | otherwise = intersect xs l


-- 31
insert :: Ord a => a -> [a] -> [a]
insert y [] = [y]
insert y (x:xs) 
    | y <= x = y:x:xs
    | otherwise = x : insert y xs 


-- 32
myunwords :: [String] -> String
myunwords [] = []
myunwords [x] = x
myunwords (x:xs) = x ++ " " ++ myunwords xs


-- 33
myunlines :: [String] -> String
myunlines [] = []
myunlines (x:xs) = x ++ "\n" ++ myunlines xs


-- 34
pMaior :: Ord a => [a] -> Int
pMaior [x] = 0
pMaior (x:xs) = if x > (sel xs a) then 0
                  else a + 1
      where a = pMaior xs 

sel :: [a] -> Int -> a
sel (x:xs) 0 = x
sel (x:xs) y = sel xs (y-1)


-- 35
mylookup :: Eq a => a -> [(a,b)] -> Maybe b
mylookup x [] = Nothing
mylookup x ((a,b):t)
    | x == a = Just b
    | otherwise = mylookup x t


-- 37
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort [x] = [x]
iSort (x:xs) = insert x (iSort xs)


-- 38
menor :: String -> String -> Bool
menor [] _ = True
menor _ [] = False
menor (x:xs) (y:ys) = x < y || menor xs ys 


-- 39
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet y [] = False
elemMSet y ((a,b):xs) 
    | y == a = True
    | otherwise = False || elemMSet y xs


-- 40
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet (y:ys) = auxConverte y ++ converteMSet ys
   where auxConverte :: (a,Int) -> [a]
         auxConverte (a,0) = []
         auxConverte (a,x) = a : auxConverte (a,x-1)


-- 41
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet y [] = [(y,1)]
insereMSet y ((a,b):xs) 
    | y==a = (a,b+1):xs
    | otherwise = (a,b): insereMSet y xs


-- 42
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet y [] = []
removeMSet y ((a,b):xs) 
    | y == a && b-1 > 0 = (a,b-1) : xs
    | y == a && b-1 == 0 = xs
    | otherwise = (a,b) : removeMSet y xs


-- 43
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (l:ls) = insereMSet l (constroiMSet ls)


-- 44
partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers ((Left a):xs) = (a:as , bs)
          where (as,bs) = partitionEithers xs
partitionEithers ((Right b):xs) = (as , b:bs)
          where (as,bs) = partitionEithers xs


-- 45
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just a:xs) = a : catMaybes xs
catMaybes (Nothing:xs) = catMaybes xs


-- 46
data Movimento = Norte | Sul | Este | Oeste deriving Show

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x1,y1) (x2,y2)
    | x1 < x2 = Este : caminho (x1+1,y1) (x2,y2)
    | x1 > x2 = Oeste : caminho (x1-1,y1) (x2,y2)
    | y1 < y2 = Norte : caminho(x1,y1+1) (x2,y2)
    | y1 > y2 = Sul : caminho(x1,y1-1) (x2,y2)
    | otherwise = [] 


--47
posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (Norte:ls) = posicao (x,y+1) ls
posicao (x,y) (Sul:ls) = posicao (x,y-1) ls
posicao (x,y) (Este:ls) = posicao (x+1,y) ls
posicao (x,y) (Oeste:ls) = posicao (x-1,y) ls


hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops (x1,y1) [] = False
hasLoops (x1,y1) l
    | (x1==x2 && y1==y2) = True
    | otherwise = False || hasLoops (x1,y1) (init l)
        where (x2,y2) = posicao (x1,y1) l


-- 48
type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto


contaQuadradosAux :: Rectangulo -> Bool
contaQuadradosAux (Rect p1 p2) = abs(x2-x1) == abs(y2-y1)
    where (x1,y1) = p1
          (x2,y2) = p2

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados (x:xs)
    | contaQuadradosAux x = 1 + contaQuadrados xs
    | otherwise = contaQuadrados xs


-- 49
areaTotalAux :: Rectangulo -> Float
areaTotalAux (Rect p1 p2) = abs(x2-x1) * abs(y2-y1)
    where (x1,y1) = p1
          (x2,y2) = p2

areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal (x:xs) = areaTotalAux x + areaTotal xs


-- 50
data Equipamento = Bom | Razoavel | Avariado
            deriving Show


naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (Avariado : xs) = naoReparar xs
naoReparar (Bom : xs) = 1 + naoReparar xs
naoReparar (Razoavel : xs) = 1 + naoReparar xs