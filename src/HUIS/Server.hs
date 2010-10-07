module HUIS.Server where


import HUIS.Dispatcher
import HUIS.ConfigParser
import HUIS.Database
import Happstack.Server
import Data.Map
import Data.Maybe
import Network.Gitit
import System.Posix.User (setUserID, UserEntry(..), getUserEntryForName)
import Paths_HUIS

-- |Main function. This loads the configuration file, opens the port, initializes the wiki, connects to database and finally starts happstack.
main = do
  -- spawn as Daemon
  cfile <- getDataFileName "huis.conf"
  cdir  <- getDataDir
  conf  <- readConfig cfile
  let rdir = conf ! "ressourcedir"
      wikidir = conf ! "wikidir"
      conf' = update (\x-> Just (cdir ++ (tail rdir))) "ressourcedir" conf
      httpconf = nullConf{ port = (read (conf ! "port")::Int) }
  -- bind socket (still as root)
  socket <- bindPort httpconf
  -- initialisation for gitit-wiki
  wikiconf <- getWikiConfiguration rdir wikidir
  -- drop user rights
  --getUserEntryForName (conf ! "user") >>= setUserID . userID
  -- initialise database
  connection <- connectDatabase conf'
  connection2 <- connectDatabaseReminder
  simpleHTTPWithSocket socket httpconf (runDispatcher conf' wikiconf connection connection2)


-- |Initialises the gitit-wiki and returns the configuration-file.
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
