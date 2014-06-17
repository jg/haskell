{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log


-- | Check whether string contains an error message
-- >>> errorMessageP "E 2 562 help help"
-- True
errorMessageP :: String -> Bool
errorMessageP s = "E " == take 2 s

-- | Check whether string contains an error message
-- >>> infoMessageP "I 147 mice in the air, I’m afraid, but you might catch a bat, and"
-- True
infoMessageP :: String -> Bool
infoMessageP s = "I " == take 2 s

-- | Parse error message from string
-- >>> parseErrorMessage "E 2 148 #56k istereadeat lo d200ff] BOOTMEM"
-- LogMessage (Error 2) 148 "#56k istereadeat lo d200ff] BOOTMEM"
parseErrorMessage :: String -> LogMessage
parseErrorMessage s = LogMessage (Error errno) timestamp text
                      where
                        text = unwords $ drop 3 parts
                        timestamp = read $ parts !! 2
                        errno = read $ parts !! 1
                        parts = words s

-- | Parse info message from string
-- >>> parseInfoMessage "I 147 mice in the air, I'm afraid, but you might catch a bat, and"
-- LogMessage Info 147 "mice in the air, I'm afraid, but you might catch a bat, and"
parseInfoMessage :: String -> LogMessage
parseInfoMessage s = LogMessage Info timestamp text
                      where
                        text = unwords $ drop 2 parts
                        timestamp = read $ parts !! 1
                        parts = words s

-- | Parse log message string
-- 
-- >>> parseMessage "I 147 mice in the air, I'm afraid, but you might catch a bat, and"
-- LogMessage Info 147 "mice in the air, I'm afraid, but you might catch a bat, and"
-- >>> parseMessage "E 2 562 help help"
-- LogMessage (Error 2) 562 "help help"
-- >>> parseMessage "I 29 la la la"
-- LogMessage Info 29 "la la la"
-- >>> parseMessage "This is not in the right format"
-- Unknown "This is not in the right format"
parseMessage :: String -> LogMessage
parseMessage s | errorMessageP s = parseErrorMessage s
               | infoMessageP s = parseInfoMessage s
               | otherwise = Unknown s


parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

getTimestamp :: LogMessage -> Int
getTimestamp (LogMessage _ timestamp _) = timestamp
getTimestamp (Unknown _) = -1

severity :: LogMessage -> Int
severity (LogMessage (Error sev) _ _) = sev
severity (LogMessage _ _ _) = 0
severity (Unknown _) = 0

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert newMessage Leaf = Node Leaf newMessage Leaf
insert newMessage (Node left message right) =
  if getTimestamp newMessage < getTimestamp message then
     Node (insert newMessage left) message right
  else
     Node left message (insert newMessage right)


build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages =
  map show $ filter (\m -> (severity m) > 50) (inOrder (build messages))
