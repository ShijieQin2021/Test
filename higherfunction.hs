import qualified Data.Map as Map 
import qualified Data.Set as Set

divideByTen :: (Floating a) => a -> a  
divideByTen = (/10)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x-y+z

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain x
        | x <= 0 = error"smaller than zero"
        | even x = x : chain (x `div` 2)
        | otherwise = x : chain (3*x+1)

numLongChains :: Int  
numLongChains = length (filter isLong (map chain [1..100]))  
    where isLong xs = length xs > 15

elem' :: (Eq a) => a -> [a] -> Bool
elem' x xs = foldl (\acc y -> if x==y then True else acc) False xs

oddSquareSum :: Integer  
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]


