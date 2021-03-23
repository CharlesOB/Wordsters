# Wordsters

<img src="https://raw.githubusercontent.com/CharlesOB/Wordsters/main/wordsters_box.jpg" height="250"> <img src="https://raw.githubusercontent.com/CharlesOB/Wordsters/main/wordsters_parts.jpg" height="250">

## Orientation
When I was a kid, my family used to play word games. This included Scrabble, Boggle, and others. One of these was Wordsters. Wordsters was by far the most frequestly played word game we owned and was always the "go-to" game for our family. Was I particularly good at it? I would like to say I was, but I was average at best. To be good at the game seemed to require a memorized dictionary. How could I improve my game play intelligently without resorting to such a feat? Surely not every word was as valuable as the next. A computer program could definitely help me improve my game, but first, how does the game work?

Here is the gist: Wordsters is a board game published in 1991 by Milton Bradley in which players compete by thinking of words that follow a given three-letter pattern. Players then write the words on a secret personal score sheet. During a round of word declaration, players take turns declaring words on their score sheets. Players are awarded 1 point for declaring words that other players do not have, and 2 points for declaring words that other players do have on their score sheets. Once a player has declared a word, no other player may declare the same word for the rest of the round. 

Words follow the three-letter pattern if they contain the three letters in order, but with any number of letters in between them. For example, if the pattern was "OTI", acceptable matches could include "MOTION", "CONSTELLATION", "OSTRICH", "LOTIS", etc. Each of these words contains the letters o, t, and i in that order, but may have letters between them. 

The full explanation of the rules can be found [here](https://www.ultraboardgames.com/wordsters/game-rules.php).

## Problem
From this simple set of rules, a few answerable questions come to mind. <br/>
In this game, what are the words that match the most three letter patterns? I want to memorize these before we play the game since they are the most likely to match any given pattern. <br/>
What are the easiest and hardest three-letter patterns? I could memorize words that belong to the hardest patterns. <br/>
Given a three-letter pattern, how does it rank in difficulty against all other possible patterns? I am just curious. <br/> 
Given a word, how does it rank in usefulness against all other possible words? I am also curious about this one. 

## Solution
To begin solving any of these problems let us first consider all pairs of letters that a word could possibly have in order. Given a string of text, we will generate all possible pairs with this definition: a list of all pairs contained in a two-character string is the string itself; in a longer string, it is the first character paired with each remaining character followed by a list of all pairs in the string without the first character, removing duplicates.
```haskell
listPairs :: Eq a => [a] -> [[a]]
listPairs [] = error "Cannot listPairs on empty list!"
listPairs [x] = error "Cannot listPairs on list length < 2!"
listPairs [x, y] = [[x, y]]
listPairs (x:xs) = nub $ (map ((x:) . (:[])) xs) ++ listPairs xs
```
This definition allows us to define a list of all triads in terms of lists of pairs with added characters: a list of all triads contained in a three-letter string is the string itself; in a longer string, it is the first character combined with each pair made from the remaining characters followed by a list of all triads in the string without the first character, removing duplicates.
```haskell
listTriads :: Eq a => [a] -> [[a]]
listTriads [] = error "Cannot listTriads on empty list!"
listTriads [x] = error "Cannot listTriads on list length < 3!"
listTriads [x, y] = error "Cannot listTriads on list length < 3!"
listTriads [x, y, z] = [[x, y, z]]
listTriads (x:xs) = nub $ (map (x:) . listPairs $ xs) ++ listTriads xs
```
Here's an auxilliary function that removes words that would not adhere to Wordsters' rules as valid responses during the game: the words cannot be fewer than 4 characters and can not contain punctuation or uppercase characters (denoting a proper noun).
```haskell
removeInvalidWords :: [String] -> [String]
removeInvalidWords = filter (\s -> ((>3) . length $ s) && isLowerAlpha s)
    where isLowerAlpha = foldr (&&) True . map (`elem` ['a'..'z'])
```
With this definition, it becomes easy to sort all triads in a list of words by how common they are by filtering out invalid words and then finding, grouping, and counting all triads in each word. 
```haskell
sortCommonTriad :: [String] -> [String]
sortCommonTriad = map head . sortBy (\a b -> compare (length a) (length b)) . group . sort . concat . map listTriads . removeBadWords
```
We can also sort a list of words by how many different triads a word satisfies. This can be done by listing all triads that apply to each word, attaching the word to each list, sorting the lists by their size, and then removing the lists of triads to leave only a sorted list of words.
```haskell
bestWords :: [String] -> [String]
bestWords words = map (\a -> fst a) . sortBy (\a b -> compare (snd b) (snd a)) . zip validWords . map (length . listTriads) $ validWords
    where validWords = removeInvalidWords words
```
With these functions defined, we now know what are the most useful words in a game of Wordsters. The top five in our dictionary are dichlorodiphenyltrichloroethane, formaldehydesulphoxylate, pneumoventriculography, formaldehydesulphoxylic, and desoxyribonucleoprotein. The easiest triad to play the game with is "ati" and the hardest is "zzq."
