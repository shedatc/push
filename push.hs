import Data.Char
import qualified Data.Foldable as F
import Data.List
import System.Process
import System.Directory
import System.FilePath.Posix

-- Locate the root directory starting from the current directory.
locateRoot :: IO (Maybe FilePath)
locateRoot = do d <- getCurrentDirectory
                locateRoot' d

-- Locate the root directory starting from the given directory (d).
locateRoot' :: FilePath -> IO (Maybe FilePath)
locateRoot' d = do d'    <- canonicalizePath d
                   found <- doesDirectoryExist $ d' </> ".push"
                   if found                                 -- Hit!
                   then return (Just d')
                   else if d' == "/"                        -- Miss! Cannot dig deeper, we hit the filesystem root.
                        then return Nothing
                        else locateRoot' $ d' </> ".."      -- Miss! Dig deeper.

-- Locate the configuration directory bound to the given root directory (root).
locateConf :: FilePath -> FilePath
-- locateConf root = root </> ".push"
locateConf _ = ".push"

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

rsyncOptions :: FilePath -> IO String
rsyncOptions conf = do s <- sequence $ map (\f -> f conf) [rsyncBaseOptions,
                                                           rsyncDeleteOption,
                                                           rsyncAdditionalOptions,
                                                           rsyncIncludes,
                                                           rsyncExcludes]
                       return . (intercalate " ") . concat $ s

remotePath :: FilePath -> IO String
remotePath root = do haveRemotePath <- haveOption "remote-path"
                     if haveRemotePath
                       then param "remote-path"
                       else return $ root
                       where conf       = locateConf root
                             haveOption = haveOption' conf
                             param      = param' conf

buildRsyncCommandLine :: FilePath -> IO String
buildRsyncCommandLine root = do options <- rsyncOptions conf
                                host    <- param "target-host"
                                path    <- remotePath root
                                return $ intercalate " " ["cd", root, "&&", "rsync", options, "./", concat [host, ":", path]]
                                  where conf  = locateConf root
                                        param = param' conf

-- Trigger a push from the given root directory (root).
push :: FilePath -> IO ()
push root = do putStrLn $ "Root: " ++ root
               cmdline <- buildRsyncCommandLine root
               putStrLn cmdline

main :: IO ()
main = do r <- locateRoot
          case r of Nothing   -> error "Missing root."
                    Just root -> push root
