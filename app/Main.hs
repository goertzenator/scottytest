-- NOTE: extensions defined in package.yaml

module Main (main) where

import Web.Scotty.Trans hiding (header)
import Options.Applicative (Parser, auto, execParser, fullDesc, header, help, helper, info, long, metavar, option, progDesc, short, value, (<**>))
import Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Text.Lazy (Text)

-- Application environment
data Env = Env
  { port :: Int
  , conn :: Int -- a dummy database connection
  , firstName :: Text
  , lastName :: Text
  }

-- Application monad.  ReaderT wraps IO.
type AppM = ReaderT Env IO

-- | Parse command line params.  This is normally a record for multiple params,
-- but in this case we just need an Int.
portParser :: Parser Int
portParser = option auto (long "port" <> short 'p' <> metavar "PORT" <> value 3000 <> help "Port to listen on")

main :: IO ()
main = do
  -- parse the port. Example usage
  --   stack run -- --port 8080
  port <- execParser $
    info
      (portParser <**> helper)
      (fullDesc <> progDesc "Run the server" <> header "server - a simple server")

  -- pretend to connect to a database
  conn <- pure 12345

  let
    env = Env
      { port
      , conn
      , firstName = "John"
      , lastName = "Doe"
      }

  -- run the server and discharge the ReaderT
  runReaderT runServer env



runServer :: AppM ()
runServer = do
  env :: Env <- ask
  liftIO $ putStrLn $ "Starting server on port " <> show (env.port)

  -- It seems scottyT needs to be able to discharge our monad down to IO for
  -- every request, so we provide the function that does this.
  scottyT (env.port) (\action -> runReaderT action env) scottyAction

-- A gotcha: stack uses an older version of scotty with a different ScottyT
-- type.  Hackage always shows the latest version by default.  This screwed me
-- up for a long time.

--scottyAction :: ScottyT AppM () -- new scotty
scottyAction :: ScottyT Text AppM () -- old scotty
scottyAction = do
  get "/" $ do
    html "Hello World!"

  -- show off a few more ways to get at Env
  get "/first" $ do -- ActionT Text AppM ()
    x <- lift $ asks firstName -- Have to `lift` AppM into ActionT
    html $ "First name is " <> x

  get "/last" $ do
    Env{lastName} <- lift ask
    html $ "Last name is " <> lastName

  get "/both" $ do
    Env{..} <- lift ask
    html $ "Full name is " <> firstName <> " " <> lastName
