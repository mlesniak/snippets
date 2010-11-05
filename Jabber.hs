-- A small jabber client to allow programs to be controlled and send status
-- messages using chat clients like pidgin. Uses the XMPP library (available
-- through cabal).
--
-- The credentials file has the format
--      
--      Credentials {
--          username  = "<username>",
--          password  = "<password>", 
--          server    = "<server>", 
--          ressource = "<some identifier>"
--      }

module Main where
import Network
import Network.XMPP
import System.Directory
import Control.Monad
import System.Environment


type Handler = String -> IO String

data Credentials = Credentials {
    username  :: String,
    password  :: String,
    server    :: String,
    ressource :: String
} deriving (Eq, Show, Read)


-- Start a jabber bot by reading the credentials from a file.
startJabber :: FilePath -> Handler -> IO ()
startJabber fname handler = do
    e <- doesFileExist fname
    unless e $ 
        error ("Jabber: credentials file " ++ fname ++ " does not exist.")
    cred <- read `liftM` readFile fname :: IO Credentials
    startJabber' cred handler


-- Start a jabber bot by passing both credentials and handler directly.
startJabber' :: Credentials -> Handler -> IO ()
startJabber' cred handler = do
    c <- openStream (server cred)
    getStreamStart c

    runXMPP c $ do
        startAuth (username cred) (server cred) (password cred) (ressource cred)
        sendPresence Nothing Nothing

        -- Say hello to someone.
        sendMessage "michael.lesniak@gmail.com" "Online."

        run
  where run :: XMPP ()
        run = do
          input <- waitForStanza (isChat `conj` hasBody)
          let sender = maybe "" id (getAttr "from" input)
              msg    = maybe "" id (getMessageBody input)
          response <- liftIO $ handler msg
          sendMessage sender response
          run


-- For testing purposes. A small echo bot.
main :: IO ()
main = do
    args <- getArgs
    when (null args) $ error "Jabber: no credentials file given on commandline"
    startJabber (head args) return

