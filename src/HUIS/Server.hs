module HUIS.Server where


import HUIS.Dispatcher
import HUIS.ConfigParser
import Happstack.Server
import Data.ConfigFile
import Data.Either.Utils
import Text.Regex
import Data.Map




main = do
  conf <- readConfig "huis.conf"
  simpleHTTP (Conf 8080 Nothing) (runDispatcher)
