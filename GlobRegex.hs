module GlobRegex
    (
      globToRegex
    , matchesGlob
    ) where

import Text.Regex.Base.RegexLike
import Text.Regex.Posix ((=~))

-- Bool is for case sensitivity
globToRegex :: String -> Bool -> String
globToRegex cs caseSensitive = '^' : globToRegex' cs ++ "$"               

globToRegex' :: String -> Bool -> String
globToRegex' "" _ = ""

globToRegex' ('*':cs) _ = ".*" ++ globToRegex' cs

globToRegex' ('?':cs) _ = '.' : globToRegex' cs

globToRegex' ('[':'!':c:cs) _ = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs)     _ = '['  :  c : charClass cs
globToRegex' ('[':_)        _ = error "unterminated character class"

globToRegex' (c:cs) = escape c ++ globToRegex' cs                 


escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
    where regexChars = "\\+()^$.{}]|"


charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs)   = c : charClass cs
charClass []       = error "unterminated character class"          


matchesGlob :: FilePath -> String -> Bool
name `matchesGlob` pat = name =~ globToRegex pat


malformed = globToRegex "["
