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
  cdir <- getDataDir
  conf <- readConfig cfile
  putStrLn $ "Starting up HUIS on 0.0.0.0:" ++ (conf ! "port") ++ "..."
  let rdir = conf ! "ressourcedir"
      wikidir = conf ! "wikidir"
      conf' = update (\x-> Just (cdir ++ (tail rdir))) "ressourcedir" conf
  -- initialisation for gitit-wiki
  putStrLn $ "Ressource-dir: " ++ (conf' ! "ressourcedir")
  wikiconf <- getWikiConfiguration rdir wikidir
  putStrLn $ "Wiki-repository: " ++ (repositoryPath wikiconf)
  putStrLn "HUIS is now ready for takeoff!"
  simpleHTTP (Conf (read (conf ! "port")::Int) Nothing) (runDispatcher conf' wikiconf)

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

