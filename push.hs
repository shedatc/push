import Data.Char
import Data.List
import System.Directory
import System.FilePath.Posix

-- Locate the configuration directory starting from the current directory.
locateConf :: IO (Maybe FilePath)
locateConf = do d <- getCurrentDirectory
                locateConf' d

-- Locate the configuration directory starting from the given directory (d).
locateConf' :: FilePath -> IO (Maybe FilePath)
locateConf' d = do d'    <- canonicalizePath d
                   found <- doesDirectoryExist $ d' </> ".push"
                   if found                                 -- Hit!
                   then return (Just (d' </> ".push"))
                   else if d' == "/"                        -- Miss! Cannot dig deeper, we hit the filesystem root.
                        then return Nothing
                        else locateConf' $ d' </> ".."      -- Miss! Dig deeper.

-- Locate the root directory bound to the given configuration directory (conf).
locateRoot :: FilePath -> IO FilePath
locateRoot conf = canonicalizePath $ conf </> ".."

-- Return the value of the required configuration parameter with the given name
-- (name), looking at the given configuration directory (conf).
param' :: FilePath -> String -> IO String
param' conf name = do v <- option name
                      case v of Just v' -> return v'
                                Nothing -> error $ "Missing parameter: " ++ name
                        where option = option' conf

-- Return the value of the optional configuration parameter with the given name
-- (name), looking at the given configuration directory (conf).
option' :: FilePath -> String -> IO (Maybe String)
option' conf name = do found <- doesFileExist f
                       if found
                         then do v <- readFile f
                                 return $ Just (dropWhileEnd isControl v)
                         else return Nothing
                         where f = optionPath' conf name

optionPath' :: FilePath -> String -> FilePath
optionPath' conf name = conf </> name

haveOption' :: FilePath -> String -> IO Bool
haveOption' conf name = do doesFileExist f
                             where f = optionPath' conf name

-- Explode the configuration into a list of configuration.
explodeConf :: FilePath -> IO [FilePath]
explodeConf conf = do wantToDelegate <- haveOption "delegate"
                      if wantToDelegate
                        then explodeDelegateFile $ optionPath "delegate"
                        else return [conf]
  where haveOption = haveOption' conf
        optionPath = optionPath' conf

explodeDelegateFile :: FilePath -> IO [FilePath]
explodeDelegateFile df = do content <- readFile df
                            return $ lines content

rsyncBaseOptions :: FilePath -> IO [String]
rsyncBaseOptions conf = do options <- option "rsync-options"
                           case options of Nothing -> return defaultOptions
                                           Just o  -> return $ words o
                             where option         = option' conf
                                   defaultOptions = ["-vv", "-a", "--no-group", "--no-perms", "-T /tmp"]

rsyncDeleteOption :: FilePath -> IO [String]
rsyncDeleteOption conf = do noDelete <- haveOption "no-delete"
                            if noDelete
                              then return []
                              else return ["--delete"]
                              where haveOption = haveOption' conf

rsyncAdditionalOptions :: FilePath -> IO [String]
rsyncAdditionalOptions conf = do options <- option "additional-rsync-options"
                                 case options of Nothing -> return []
                                                 Just o  -> return $ words o
                                   where option = option' conf

rsyncIncludes :: FilePath -> IO [String]
rsyncIncludes conf = do haveIncludes <- haveOption "includes"
                        if haveIncludes
                          then return ["--include-from=" ++ (optionPath "includes")]
                          else return []
                          where haveOption = haveOption' conf
                                optionPath = optionPath' conf

rsyncExcludes :: FilePath -> IO [String]
rsyncExcludes conf = do haveIncludes <- haveOption "excludes"
                        if haveIncludes
                          then return ["--exclude-from=" ++ (optionPath "excludes"), "--exclude=*"]
                          else return ["--exclude=*"]
                          where haveOption = haveOption' conf
                                optionPath = optionPath' conf

accumulateOptions :: String -> [String] -> String
accumulateOptions acc    []  = acc
accumulateOptions ""  (o:os) = accumulateOptions o os
accumulateOptions acc (o:os) = accumulateOptions (concat [acc, " ", o]) os

rsyncOptionsBuilders :: FilePath -> [IO [String]]
rsyncOptionsBuilders conf = do builders <- [ rsyncBaseOptions,
                                             rsyncDeleteOption,
                                             rsyncAdditionalOptions,
                                             rsyncIncludes,
                                             rsyncExcludes ]
                               return $ builders conf

rsyncOptions :: FilePath -> IO String
rsyncOptions conf = do options <- sequence $ rsyncOptionsBuilders conf
                       return $ foldl accumulateOptions "" options

remotePath :: FilePath -> IO String
remotePath conf = do haveRemotePath <- haveOption "remote-path"
                     if haveRemotePath
                       then param "remote-path"
                       else locateRoot conf
                       where haveOption = haveOption' conf
                             param      = param' conf

buildRsyncCommandLine :: FilePath -> IO String
buildRsyncCommandLine conf = do options <- rsyncOptions conf
                                host    <- param "target-host"
                                path    <- remotePath conf
                                root    <- locateRoot conf
                                return $ intercalate " " ["cd", root, "&&", "rsync", options, "./", concat [host, ":", path ++ "/"]]
                                  where param = param' conf

-- Trigger a push from the given root directory (root).
push :: FilePath -> IO ()
push conf = do putStrLn $ "Conf: " ++ conf
               cmdline <- buildRsyncCommandLine conf
               putStrLn cmdline

sequencePush :: IO [FilePath] -> IO ()
sequencePush cs = do confs <- cs
                     sequence_ $ map push confs

main :: IO ()
main = do bc <- locateConf
          case bc of Nothing       -> error "Missing configuration."
                     Just baseConf -> sequencePush $ explodeConf baseConf
