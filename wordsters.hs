import Data.List (nub, sort, sortBy, group, elemIndex)
import Data.Char (isUpper)
import Data.Maybe (fromMaybe)
import System.Environment

listTriads :: Eq a => [a] -> [[a]]
listTriads [] = error "Cannot listTriads on empty list!"
listTriads [x] = error "Cannot listTriads on list length < 3!"
listTriads [x, y] = error "Cannot listTriads on list length < 3!"
listTriads [x, y, z] = [[x, y, z]]
listTriads (x:xs) = nub $ (map (x:) . listPairs $ xs) ++ listTriads xs

listPairs :: Eq a => [a] -> [[a]]
listPairs [] = error "Cannot listPairs on empty list!"
listPairs [x] = error "Cannot listPairs on list length < 2!"
listPairs [x, y] = [[x, y]]
listPairs (x:xs) = nub $ (map ((x:) . (:[])) xs) ++ listPairs xs

removeBadWords :: [String] -> [String]
removeBadWords = filter (\s -> ((>3) . length $ s) && isLowerAlpha s)
    where isLowerAlpha = foldr (&&) True . map (`elem` ['a'..'z'])

sortCommonTriad :: [String] -> [String]
sortCommonTriad = map head . sortBy (\a b -> compare (length a) (length b)) . group . sort . concat . map listTriads . removeBadWords

bestWords :: [String] -> [String]
bestWords words = map (\a -> fst a) . sortBy (\a b -> compare (snd b) (snd a)) . zip words . map (length . listTriads) $ words

--bestWords = sortBy (\a b -> compare (length $ listTriads b) (length $ listTriads a)) . removeBadWords

-- Given a list of sorted strings, return the percentile at which a string lands in the list.
percentile :: (Eq a, Fractional b) => [a] -> a -> b
percentile xs x = 100 * ((fromIntegral . fromMaybe (-1) $ elemIndex x xs) + 1) / (fromIntegral . length $ xs)

interactBestWords :: String -> String
interactBestWords = unlines . take 100 . bestWords . removeBadWords . lines

interactCommonTriad :: String -> String
interactCommonTriad = unlines . take 100 . sortCommonTriad . lines

-- Interact on the command line. Type in a triad to see its percentile ranking.
interactPercentile :: [String] -> String -> String
interactPercentile xs = unlines . map (show . percentile xs) . lines

main = do  
  --triadLines <- readFile "sorted_triads.txt"
  --let triads = lines triadLines
  --interact (interactPercentile triads)
  
  words <- readFile "words_alpha.txt"
  --writeFile "sorted_triads.txt" (unlines . sortCommonTriad . lines $ words)
  writeFile "sorted_words.txt" (unlines . bestWords . removeBadWords . lines $ words)
  --writeFile "output.txt" ((interactCommonTriad words) ++ "\n" ++ (interactBestWords words))
