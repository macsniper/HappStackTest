module HUIS.Server where


import HUIS.Dispatcher
import HUIS.ConfigParser
import HUIS.Database
import Happstack.Server
import Text.Regex
import Data.Map
import Data.Maybe
import Network.Gitit
import System.Posix.User (setUserID, UserEntry(..), getUserEntryForName)
import Control.Concurrent
import System.Posix.Process (createSession)
import Paths_HUIS

main = do
  cfile <- getDataFileName "huis.conf"
  cdir <- getDataDir
  conf <- readConfig cfile
  let rdir = conf ! "ressourcedir"
      wikidir = conf ! "wikidir"
      conf' = update (\x-> Just (cdir ++ (tail rdir))) "ressourcedir" conf
      httpconf = nullConf{ port = (read (conf ! "port")::Int) }
  socket <- bindPort httpconf
  getUserEntryForName (conf ! "user") >>= setUserID . userID
  -- initialisation for gitit-wiki
  wikiconf <- getWikiConfiguration rdir wikidir
  -- initialise database
  connection <- connectDatabase conf'
  -- spawn as Daemon
  createSession
  forkIO $ simpleHTTPWithSocket socket httpconf (runDispatcher conf' wikiconf)
  

-- initialises the gitit-wiki and returns the configuration-file
getWikiConfiguration::String-> String-> IO Network.Gitit.Config
getWikiConfiguration resdir wikidir = do
  conf <- getDefaultConfig
  tpldir' <- getDataDir
  let tpldir = tpldir' ++ (tail resdir) ++ wikidir
      conf' = conf{ --, withUser        = myWithUser,
                   repositoryPath  = tpldir ++ "/repo"
                  , repositoryType  = Git
                  , defaultPageType = Markdown
                  , templatesDir    = tpldir ++ "/tpl"
                  , cacheDir        = tpldir ++ "/cache"
                  , staticDir       = tpldir ++ "/static"}
  createStaticIfMissing conf'
  createRepoIfMissing conf'
  createTemplateIfMissing conf'
  initializeGititState conf'
  return conf'

