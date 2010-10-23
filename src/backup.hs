module Main
    (
    ) where

import System.Process
import GHC.IO.Exception
import Control.Monad
import Control.Monad.State
import System.Console.ParseArgs
import System.Environment (getArgs)
import HSH

arguments =
    [
     Arg "dryrun" (Just 'd') (Just "dryrun") Nothing
       "Do a dry a run instead and print what will be done",
     Arg "server" (Just 's') (Just "server")
       (argDataRequired "server address" ArgtypeString) "Address of server",
     Arg "folder" (Just 'f') (Just "folder")
       (argDataRequired "path" ArgtypeString) "Absolute path of backup folder on destination",
     Arg "src" (Just 'r') (Just "src")
       (argDataRequired "path" ArgtypeString) "Absolute path to source folder",
     Arg "exclude" (Just 'e') (Just "exclude")
       (argDataRequired "file" ArgtypeString) "Path to exclude file",
     Arg "schedule" (Just 'u') (Just "schedule")
       (argDataRequired "num per week:num per month" ArgtypeString)
       "The schedule to use"
    ]

data Backup = Backup
    {
      user :: String,
      server :: String,
      folder :: FilePath,
      srcFolder :: FilePath,
      exclude :: FilePath,
      dryrun :: Bool
    }

type BSt = StateT Backup IO

data Status = Okay | Error | DryRun deriving (Show)

sample = Backup "narens" "192.168.2.3" "/home/narens/junk" "/home/narens/Desktop/junk" "/home/narens/Desktop/backup_exclude" False

runSample f = evalStateT f sample

main = do
  ags <- parseArgsIO ArgsComplete arguments
  let backup = Backup
               {
                 user = getRequiredArg ags "user",
                 server = getRequiredArg ags "server",
                 folder = getRequiredArg ags "folder",
                 srcFolder = getRequiredArg ags "src",
                 exclude = getRequiredArg ags "exclude",
                 dryrun = gotArg ags "dryrun"
               }
  return ()
{-
  --check if a backup is running already
  running <- fmap (elem "RUNNING".words) $
             readFile "~/.backup_status"
  let real = atype == "real"
  --check destination
  b <- checkExists real server (folder ++ "/SERVER_EXISTENCE_DIR")
  if not running
    then
      writeFile "~/.backup_status" "RUNNING" >>
      run real server folder absSrc excludes num >>= \r ->
      writeFile "~/.backup_status" "DONE" >> return r
    else
      putStrLn "Already Running" >> return False
-}

{-
run real server folder absSrc excludes numHourly = do
  --delete oldest hour
  delete real server folder (fol (numHourly -1))
  --move n-1 to n
  mapM_ (\(a,b) -> shift real server folder (fol a) (fol b)) $
        zip (reverse [1..numHourly-2]) (reverse [2..numHourly-1])
  --make hard link of latest
  makeHardLinkCopy real server folder (fol 0) (fol 1)
  --sync
  sync real server folder absSrc (fol 0) excludes
  --touch
  touch real server folder (fol 0)
    where fol x = "hourly." ++ show x

sync real server folder absSrc dst excludes =
    printCmd cmd >> if real then system cmd >>= checkPassed else return True
    where cmd = "rsync -va --delete --delete-excluded --exclude-from="
                ++ excludes ++ " " ++ absSrc
                ++ " narens@" ++ server ++ ":" ++ folder ++ "/" ++ dst

makeHardLinkCopy real server folder src dst = do
  e <- checkExists real server (src')
  if e then printCmd cmd >> (if real then system cmd >>=
     checkPassed else return True) else return True
    where cmd = "ssh " ++ server ++ " 'cp -al " ++ src' ++ " " ++ dst' ++ "'"
          src' = folder ++ "/" ++ src
          dst' = folder ++ "/" ++ dst

touch real server folder name =
    printCmd cmd >> if real then system cmd >>= checkPassed else return True
    where cmd = "ssh " ++ server ++ " 'touch " ++ folder ++ "/" ++ name ++ "'"

shift real server folder src dst = do
  e <- checkExists real server (folder ++ "/" ++ src)
  if e then move real server folder src dst else return True

move real server folder nameSrc nameDst =
    printCmd cmd >> if real then system cmd >>= checkPassed else return True
    where cmd = "ssh " ++ server ++ " 'mv " ++ src ++ " " ++ dst ++ "'"
          src = folder ++ "/" ++ nameSrc
          dst = folder ++ "/" ++ nameDst

delete real server folder name =
    printCmd cmd >> if real then system cmd >>= checkPassed else return True
    where cmd = "ssh " ++ server ++ " 'rm -rf " ++ folder
                ++ "/" ++ name ++ "'"

checkExists :: BSt Bool
checkExists = do
    printCmd cmd >> if real then system cmd >>= checkPassed else return True
    where cmd = "ssh " ++ server ++ " '[ -d " ++ folder ++ " ]'"
-}

exec :: String -> BSt Status
exec cmd = do
  liftIO.putStrLn $ "RUNNING: " ++ cmd
  dry <- gets dryrun
  usr <- gets user
  srv <- gets server
  let cmd' = "ssh " ++ usr ++ "@" ++ srv ++ " " ++ cmd ++ " > /dev/null"
  if dry
    then return DryRun
    else liftIO (fmap fromBool $ run cmd')
    where fromBool True = Okay
          fromBool False = Error
