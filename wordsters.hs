import Data.List (nub, sort, sortBy, group, elemIndex)
import Data.Char (isUpper)
import Data.Maybe (fromMaybe)
import System.Environment

wordsters3 :: Eq a => [a] -> [[a]]
wordsters3 [] = error "Cannot wordsters3 on empty list!"
wordsters3 [x] = error "Cannot wordsters3 on list length < 3!"
wordsters3 [x, y] = error "Cannot wordsters3 on list length < 3!"
wordsters3 [x, y, z] = [[x, y, z]]
wordsters3 (x:xs) = nub $ (map (x:) . wordsters2 $ xs) ++ wordsters3 xs

wordsters2 :: Eq a => [a] -> [[a]]
wordsters2 [] = error "Cannot wordsters2 on empty list!"
wordsters2 [x] = error "Cannot wordsters2 on list length < 2!"
wordsters2 [x, y] = [[x, y]]
wordsters2 (x:xs) = nub $ (map ((x:) . (:[])) xs) ++ wordsters2 xs

removeBadWords :: [String] -> [String]
removeBadWords = filter (\s -> ((>2) . length $ s) && (not . isUpper . head $ s) && (not ('\'' `elem` s)))

sortCommonTriad :: [String] -> [String]
sortCommonTriad = map head . sortBy (\a b -> compare (length a) (length b)) . group . sort . concat . map wordsters3 . removeBadWords

bestWords :: [String] -> [String]
bestWords words = map (\a -> fst a) . sortBy (\a b -> compare (length $ snd b) (length $ snd a)) . zip words . map wordsters3 . removeBadWords $ words

--bestWords = sortBy (\a b -> compare (length $ wordsters3 b) (length $ wordsters3 a)) . removeBadWords

-- Given a list of sorted strings, return the percentile at which a string lands in the list.
percentile :: (Eq a, Fractional b) => [a] -> a -> b
percentile xs x = 100 * ((fromIntegral . fromMaybe (-1) $ elemIndex x xs) + 1) / (fromIntegral . length $ xs)

interactBestWords :: String -> String
interactBestWords = unlines . take 100 . bestWords . lines

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
  writeFile "sorted_triads.txt" (unlines . sortCommonTriad . lines $ words)
  writeFile "sorted_words.txt" (unlines . bestWords . lines $ words)
  --writeFile "output.txt" ((interactCommonTriad words) ++ "\n" ++ (interactBestWords words))
