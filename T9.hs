module T9 (calculate, calcMult, dictionary, messages) where

import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import Dictionary
import Messages

-- Algorithm
-- Take out first word and begin translating to buttonpresses
getDigit :: Char -> Int
getDigit a = 	if(a `elem` ['A'..'C']) then 2
		else if(a `elem` ['D'..'F']) then 3
		else if(a `elem` ['G'..'I']) then 4
		else if(a `elem` ['J'..'L']) then 5
		else if(a `elem` ['M'..'O']) then 6
		else if(a `elem` ['P'..'S']) then 7
		else if(a `elem` ['T'..'V']) then 8
		else if(a `elem` ['W'..'Z']) then 9
		else if(a `elem` [' ']) then 0
		else -1
--For testing
sentence = messages !! 11

--Gets words from string into list of words
getElemIndex  :: [Char] -> [Int] -> [([Char], Integer)] -> Maybe Int
getElemIndex word button dic = 
	elemIndex word $ map getTupleWord $
		sortDictionary $ filterWordsMatching button $
		dictionaryWords dic
	where 
		getTupleWord (a,(b,c)) = b
		sortDictionary :: (Ord c, Ord b) => [(a, (b, c))] -> [(a, (b, c))]
		sortDictionary a = reverse $ sortBy (comparing getPrio) (reverse $ sortBy(comparing getAl) a)
			where 
				getPrio (a,(b,c)) = c --gets the prio from snd tuple
				getAl (a,(b,c)) = b --gets the alphabetical prio
		filterWordsMatching :: Eq a => [a] -> [([a], b)] -> [([a], b)]
		filterWordsMatching searchFor dic = 
			filter(\(x,y) -> searchFor `isPrefixOf` x) dic
		dictionaryWords :: [([Char], Integer)] -> [([Int],([Char],Integer))]
		dictionaryWords a = zip (map (map getDigit) (map fst a)) a

--Filter the words matching keypress



minimumPresses dic iteration word = 
	if adder(current) >= adder(next) then next 
	else current
	where 	
		presses = wordToButton word
		current = (length(fst currentPress), just, (fst currentPress))
		next = 	if length (snd currentPress) > 0 then
				minimumPresses dic (iteration + 1) word
			else current
		just = 	if(isJust check) then (fromJust check) 
			else 1000000
		currentPress = (splitAt (iteration+1) presses)
		adder (a,b,c) = a+b
		check = (getElemIndex word (fst currentPress) dic)
		wordToButton a = map getDigit a
			
calculate dic message = buttonPress tup les
	where
		wordsList = getWords message
		tup:les = map (minimumPresses dic 0) wordsList
			
		buttonPress t [] = (addPostFix 	--Fix last tuple, to not 
						--misss out on last one
			(intToString (getList t)) $ getNumber t)		
		buttonPress t (l:s) = (addPostFix 
			(intToString (getList t)) $ getNumber t) 
			++ ['0'] ++ buttonPress l s
		addPostFix prefix 0 = prefix
		addPostFix prefix number = if(number == 1000000) then ['?']
			else addPostFix (prefix ++ ['^']) (number - 1)
		getNumber (a,b,c) = b
		getList (a,b,c) = c
		intToString :: [Int] -> [Char]
		intToString [] = []
		intToString a = [chr (head(a) + ord '0')] ++ intToString (tail a)
		getWords :: String -> [String]
		getWords a = words a

calcMult dic messages =  zip  buttonpressMess messages
	where	buttonpressMess = (map (calculate dic) messages)

facit = "846^094^030604^04602^036608096703^0846^094^03"		 



