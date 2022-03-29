--Nev: Fulop Koppany-Bence
--Feladat linkje: https://www.codingame.com/ide/puzzle/folding-paper

import System.IO
import Control.Monad

megold :: (Eq a1, Num b) => (a1, b) -> (a1, b) -> (a1, b) -> (a2, b) -> [a1] -> [b]
megold (r,v1) (l,v2) (u,v3) (d,v4) [] = v1 : v2 : v3 : v4 : []
megold (r,v1) (l,v2) (u,v3) (d,v4) (x:xs)
    | x == r = megold (r,1) (l,(v2+v1)) (u,(v3*2)) (d,(v4*2)) xs
    | x == l = megold (r,(v1+v2)) (l,1) (u,(v3*2)) (d,(v4*2)) xs
    | x == u = megold (r,(v1*2)) (l,(v2*2)) (u,1) (d,(v4+v3)) xs
    | otherwise = megold (r,(v1*2)) (l,(v2*2)) (u,(v3+v4)) (d,1) xs

kiertekel :: [a] -> [Char] -> a
kiertekel xs kif
    | kif == "R" = xs !! 0
    | kif == "L" = xs !! 1
    | kif == "U" = xs !! 2
    | otherwise =  xs !! 3

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    
    order <- getLine
    side <- getLine

    let eredmeny2 = megold ('R',1) ('L',1) ('U',1) ('D',1) order
    
    print (kiertekel eredmeny2 side)
    return ()