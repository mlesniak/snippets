-- A simple interpreter for the beginning of a basic like language (command
-- interpeter). This approach won't work later when we need to handle line
-- numbers etc... 
--
-- TODO
--   * error handling for command line.
--   * parsing is not as beautiful as it could be. but when is parsing code
--     beautiful anyway?
--   * implement this using a parser library (parsec).
--   * think about defining a monad, in which the Commands can be executed
--     instead of having an interpreter. Would this allow simpler
--     experimentation?
module Main where
import System.Environment
import Data.Char
import Data.List
import Data.Maybe


-- Defines our commands. To illustrate multiple commands, e also have a
-- Comment-command, which does nothing.
data Command = 
      Print String
    | Comment
    deriving (Show, Eq)


main :: IO ()
main = do
    -- Read code (linewise), filename specificed on the command line.
    [fname] <- getArgs
    code    <- lines `fmap` readFile fname

    -- Create tokens and interpret them.
    interpreter (tokenizer code)


-- Takes a list of commands and executes them.
interpreter :: [Command] -> IO ()
interpreter []     = return ()
interpreter (c:cs) = do
    case c of
        Print s -> putStrLn s
        Comment -> return ()
    interpreter cs     


-- Transforms strings into our type-based description of Basic.
tokenizer :: [String] -> [Command]
tokenizer = map parseLine
    

-- Interprets a string and transforms into a command. For real cases we would
-- use a real parser and not define it by hand, because it quickly becomes a
-- mess.
--
-- For simplification, empty lines become comments.
parseLine :: String -> Command
parseLine line' =     
    let line      = dropWhile isSpace line'
        firstWord = case (words line) of
                        []    -> ""
                        (w:_) -> w
        parseFunc = lookup firstWord commandTable 
        rest      = fromJust $ stripPrefix firstWord line
    in case parseFunc of
        Nothing -> error $ "syntax error: " ++ line'
        Just f  -> f rest
  where -- This table contains parsing functions for the different commands.
        commandTable = [("",      handleComment)
                       ,("--",    handleComment)
                       ,("print", handlePrint)]        
        handleComment _ = Comment
        handlePrint str = Print (findString str)


-- Find the beginning and end of a string quoted by "...".
findString :: String -> String
findString s =
    let begin = tail $ dropWhile (/= '\"') s
    in takeWhile (/= '\"') begin

