module Lib
    ( someFunc,
      convert
    ) where

import Data.Ratio
import Data.Char


someFunc :: IO ()
someFunc = putStrLn "someResult"

{-exercises from the book on haskell : chapter 1-}

convert :: [Int] -> [Bool]
convert (0:xs) = False:convert xs
convert (1:xs) = True:convert xs
convert [] =[]

ismember0 :: String -> Bool
ismember0 xs =  let blist = map (\y -> if y == '0' then True else False) xs
                --in foldl (\acc x -> acc || x) False blist
                --in foldl (||) False blist
                   in or blist


data Colour = Red | Orange | Yellow
                   | Green | Blue | Violet
                   deriving Show
data Animal a b = Cat a | Dog b | Rat
                  deriving Show
data BreedOfCat = Siamese | Persian | Moggie
                  deriving Show
{-
:type Cat Siamese
Cat Siamese          :: Animal BreedOfCat b
Cat Persian         -- :: Animal BreedOfCat b
Cat "moggie"        -- :: Animal String b
Dog 15              -- :: Animal a Integer
Rat                 -- :: Animal a b
 -}

addJust :: [Maybe Int] -> [Maybe Int] ->[Maybe Int] 
addJust (Just x : xs) ( Just y : ys) = Just (x+y) : addJust xs ys 
addJust (Nothing : xs) ( Just y : ys) = Just y : addJust xs ys 
addJust (Just x:xs) ( Nothing : ys) = Just x : addJust xs ys 
addJust (Nothing : xs) ( Nothing : ys) = Nothing : addJust xs ys 
addJust [] [] = []
-- addJust [Just 2, Nothing, Just 3]   [Nothing, Nothing, Just 5]

data Metals = Fe | Alu | Copper | Zinc 
                   deriving  (Eq, Show)


data Piles  = PileOne  | PileTwo | PileThree  
                deriving  (Eq, Show)                 
data Coins = Dollar5 Piles | Dollar10 Piles | Dollar15 Piles | Dollar20 Piles 
                 deriving  (Eq, Show)
{-

myfun :: Eq b => a -> b -> b -> Bool
myfun a b c = if a then b == c else False 

-}                

bitOr :: Int -> Int -> Int
bitOr 0 0 = 0
bitOr x y = 1

bitAnd :: Int -> Int -> Int
bitAnd 1 1 = 1
bitAnd x y = 0

bitwiseAnd :: [Int]->[Int]->[Int]
bitwiseAnd xs ys =  zipWith bitAnd xs ys
                    
f1 :: Num a => (a,a) -> a
f1 (x,y) = x + y

parseNumerals ::Num a => [Maybe a] -> [a]
parseNumerals (Nothing:xs) = parseNumerals xs
parseNumerals (Just x:xs) = x:parseNumerals xs
parseNumerals [] = []

takeBigger :: [Int]->Int->[Int]
takeBigger xs n = [x | x <- xs, n < x]
                 
--let maxIndex xs = head $ filter ((== maximum xs) . (xs !!)) [0..]

getAllIndexesOfNo :: [Int] -> Int -> [Int]
getAllIndexesOfNo xs x = foldr insertHelper [] (zip [0..] xs) 
                                   where
                                    insertHelper (i,y) acc = if y == x then (i:acc) else acc
-- getAllIndexesOfNo [1,3,4,56,4,78,2,4] 4

{-Exercise 23 not working-}
allNotSquares::[Int] -> [Int]
allNotSquares xs = [x| x<-xs , y<-[1..20] , x>0 , x /= (y*y)]


countLetters :: String -> Char -> Int
countLetters xs letter = foldr (\nextChar count ->if nextChar == letter then (count+1) else count) 0 xs 
--countLetters xs letter = foldl (\count nextChar -> if nextChar == letter then (count + 1) else count) 0 xs
--countLetter "ababnjhga" 'a'

removeLetter xs letter = foldr (\nextChar newString ->if nextChar == letter then newString else nextChar:newString) [] xs 
-- removeLetter  "ababnjhga" 'a'

rev :: [a] -> [a]
rev xs = foldl (\newList x  -> x:newList ) [] xs
-- rev  "abcdefghij"

maybeLast :: [a] -> Maybe a
maybeLast []= Nothing
maybeLast (x:[])= Just x
maybeLast (x:xs) = maybeLast xs

{-exercises from the book on haskell : chapter 2-Recursion-}

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (splitter:xs) = quicksort [y | y <- xs, y<splitter]
                          ++ [splitter]
                          ++ quicksort [y | y <- xs, y>=splitter]

inverse :: [(Int,Int)] -> [(Int,Int)]
inverse [] = []
inverse (x:xs) = (snd x,fst x):inverse xs
-- inverse [(1,2),(3,4)] 

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if (x <= y) then  x:(merge xs (y:ys)) else  y:(merge (x:xs) ys)
          
whatIndex ::Eq a=> [a] -> a -> Maybe Int
whatIndex xs y = foo xs 0
  where
    foo [] _ = Nothing
    foo (x:xs) c
        | x == y    = Just c
        | otherwise = foo xs (c+1)

whatIndex2 ::Eq a=> [a] -> a -> Maybe Int
whatIndex2 xs y = foo xs 0
  where
    foo [] _ = Nothing
    foo (x:xs)  c  = if x==y then Just c else foo xs (c+1)    

myLookup :: Eq t => t -> [(t,a)] -> Maybe a
myLookup index [] = Nothing  
myLookup index (x:xs) = if (index == fst x) then Just $ snd x else myLookup index xs
-- myLookup 5 [(1,2),(5,3)] 
-- myLookup 6 [(1,2),(5,3)]   

countOcurrences :: (Eq a,Num n) => a -> [a] -> n
countOcurrences y xs = foldl (\countNumber element -> if y == element then countNumber+1 else countNumber ) 0 xs
-- countOcurrences 5 [4,5,6,78,5,5,43,5]
countOcurrences2 y []   =  0
countOcurrences2 y (x:xs) =   if y == x  then  (1 + countOcurrences2 y xs) else (countOcurrences2 y xs)
-- countOcurrences 5 [4,5,6,78,5,5,43,5]

removeAlternatingElements :: [a] -> [a]
removeAlternatingElements xs = reverse ( removeNext [] xs 1)
                                      where
                                        removeNext ::(Integral n ) => [a] -> [a] -> n -> [a]
                                        removeNext ys [] indexNumber = ys
                                        removeNext ys (x:xs) indexNumber = if indexNumber `mod` 2  == 0 
                                                                           then removeNext ys xs (indexNumber+1) 
                                                                           else removeNext (x:ys) xs (indexNumber+1) 
 --removeAlternatingElements [1,2,3,4,5,6,7] returns [2,4,6]

extract :: [Maybe a] -> [a]
extract xs = reverse (removeNonNumbers xs [])
             where 
             removeNonNumbers [] ys = ys
             removeNonNumbers (Nothing:xs) ys = removeNonNumbers xs ys
             removeNonNumbers (Just element:xs) ys = removeNonNumbers xs (element:ys)
--extract [Just 3, Nothing, Just 7] = [3, 7]

getIndexOfSubstring :: String -> String ->Maybe Int
getIndexOfSubstring xs ys =   getStartIndex xs ys 0 0
                              where 
                              getStartIndex [] (zs) indexNumber indexLength =  Nothing --Nothing
                              getStartIndex xs [] indexNumber indexLength =  Just indexNumber
                              getStartIndex (x:xs) (z:zs) indexNumber indexLength = if x == z 
                                                                                    then  getStartIndex xs (zs) indexNumber (indexLength+1)
                                                                                    else  getStartIndex xs (ys) (indexLength+1)  (indexLength+1)

{-
getIndexOfSubstring "abcde" "bc" ==> Just 1
getIndexOfSubstring "abcde" "fg" ==> Nothing
getIndexOfSubstring "012b456bcde" "bc"
-}




{-exercises from the book on haskell : chapter 3-}


