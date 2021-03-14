import Data.List (nub, sort, sortBy, group)
import Data.Char (isUpper)

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
sortCommonTriad = map head . sortBy (\a b -> compare (length b) (length a)) . group . sort . concat . map wordsters3 . removeBadWords


bestWords :: [String] -> [String]
bestWords = sortBy (\a b -> compare (length $ wordsters3 b) (length $ wordsters3 a)) . removeBadWords

interactBestWords :: String -> String
interactBestWords = unlines . take 100 . bestWords . lines

interactCommonTriad :: String -> String
interactCommonTriad = unlines . take 100 . sortCommonTriad . lines

main = do
  interact interactCommonTriad
