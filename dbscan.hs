import Data.Char
import Data.List
import Text.ParserCombinators.Parsec

data Point a = Pt a a deriving (Show, Eq)
manhattan (Pt x y) (Pt x' y') = abs (x' - x) + abs (y' - y)

main = interact $ show . simplescan . parsepoints


split :: Char -> String -> [String]
split _ "" = [""]
split sp (c:cs)
    | c == sp = "": rest
    | c == '\n' = "": rest
    | otherwise = (c : head rest) : tail rest
    where
       rest = split sp cs

splitcomma' = split ','
splitcomma l = filter (\x -> not (x == "")) (splitcomma' l)

makepoints [] = []
makepoints (x:y:ys) = (Pt (read x::Int) (read y::Int)) : (makepoints ys)

parsepoints = makepoints . splitcomma

simplescan = dbscan 2 3
dbscan eps minpts points = filter (not.null) $ map (\p -> (cluster p eps minpts points)) points

region x eps points = filter (\p -> (manhattan x p) <= eps && not (x == p)) points

cluster x eps minpts points 
    | length neighbors < minpts  = []
    | otherwise = x : (expand neighbors eps minpts points [x])
    where neighbors = region x eps points

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
