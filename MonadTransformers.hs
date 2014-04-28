import Data.Char

getPassword :: IO (Maybe String)
getPassword = do s <- getLine
                 if isValid s then return $ Just s
                              else return Nothing
 
-- The validation test could be anything we want it to be.
isValid :: String -> Bool
isValid s = length s >= 8 && any isAlpha s && any isNumber s && any isPunctuation s


askPassword :: IO ()
askPassword = do putStrLn "Insert your new password:"
                 maybe_value <- getPassword
                 if isJust maybe_value 
                     then do putStrLn "Storing in database..."
                     -- ... other stuff, including 'else'



newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }                         

instance Monad m => Monad (MaybeT m) where
    return  = MaybeT . return . Just                     



