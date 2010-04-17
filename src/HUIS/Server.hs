module HUIS.Server where


import HUIS.Dispatcher
import HUIS.ConfigParser
import Happstack.Server
import Text.Regex
import Data.Map
import Data.Maybe
import Paths_HUIS

main = do
  cfile <- getDataFileName "huis.conf"
  --let cfile = "huis.conf"
  conf <- readConfig cfile
  simpleHTTP (Conf (read (conf ! "port")::Int) Nothing) runDispatcher
