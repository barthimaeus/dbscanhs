module Main where

import Data.Char
import Data.List
import Text.ParserCombinators.Parsec

data Point a = Pt a a deriving (Show, Eq)
manhattan (Pt x y) (Pt x' y') = abs (x' - x) + abs (y' - y)

main =  do  input <- getContents
            let points = case   parse parsepoints "stdin" input of 
                                Left err -> error $ "Parse error"
                                Right result -> result
            print points
            print $ simplescan points
        

parsepoints =
    do
        points <- many parsepoint
        eof :: Parser ()
        return points

parsepoint =
    do 
        x <- many1 digit
        spaces
        char ','
        spaces
        y <- many1 digit
        spaces
        return $ Pt (read x::Int) (read y::Int)

simplescan = dbscan 2 3
dbscan eps minpts points = filter (not.null) $ map (\p -> (cluster p eps minpts points)) points

cluster x eps minpts points 
    | length neighbors < minpts  = []
    | otherwise = x : (expand neighbors eps minpts points [x])
    where neighbors = region x eps points

region x eps points = filter (\p -> (manhattan x p) <= eps && not (x == p)) points

expand [] _ _ _ _ = []
expand c eps minpts points visited
    | (head c) `elem` visited 
        = nub $ expand (tail c) eps minpts points visited
    | length (region (head c) eps points) < minpts 
        = nub $ (head c) : rest
    | otherwise 
        = nub $ c 
            ++ (expand (region (head c) eps points) eps minpts points ((head c) : visited))
            ++ rest
    where
        rest = (expand (tail c) eps minpts points ((head c) : visited))
