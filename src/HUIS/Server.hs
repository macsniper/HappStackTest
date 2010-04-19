module HUIS.Server where


import HUIS.Dispatcher
import HUIS.ConfigParser
import Happstack.Server
import Text.Regex
import Data.Map
import Data.Maybe
import Network.Gitit
import Paths_HUIS

main = do
  cfile <- getDataFileName "huis.conf"
  conf <- readConfig cfile
  -- initialisation for gitit-wiki
  wikiconf <- getWikiConfiguration
  simpleHTTP (Conf (read (conf ! "port")::Int) Nothing) (runDispatcher conf wikiconf)

-- initialises the gitit-wiki and returns the configuration-file
getWikiConfiguration:: IO Network.Gitit.Config
getWikiConfiguration = do
  conf <- Network.Gitit.getDefaultConfig
  tpldir <- getDataDir ++ "ressources"
  let conf' = conf{ --authHandler     = myAuthHandler
                  --, withUser        = myWithUser,
                    repositoryPath  = (tpldir ++ "/wiki/repo")
                  , repositoryType  = Network.Gitit.Git
                  , defaultPageType = Network.Gitit.Markdown
                  , templatesDir    = (tpldir ++ "/wiki/tpl") --does not work yet :(
                  , cacheDir        = (tpldir ++ "/wiki/cache")
                  , staticDir       = (tpldir ++ "/wiki/static")}
  Network.Gitit.createStaticIfMissing conf'
  Network.Gitit.createRepoIfMissing conf'
  Network.Gitit.createTemplateIfMissing conf'
  Network.Gitit.initializeGititState conf'
  return conf'
 
