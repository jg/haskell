filterSpaces :: String -> String
filterSpaces = filter (not . isSpace)
  where isSpace c = c == ' '

filterVowels :: String -> String
filterVowels = filter (not . isVowel)

pickVowels :: String -> String
pickVowels = filter isVowel

isVowel :: Char -> Bool
isVowel v = elem v ['a', 'e', 'i', 'o', 'u']

disemvowel :: String -> [String]
disemvowel = sequence [f, g]
  where f = filterVowels . filterSpaces
        g = pickVowels
