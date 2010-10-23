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
import System.FilePath
import Data.List (foldl')

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
type Action = (Where,String)
type Actions = [Action]

data Where = Remote | Local | Nowhere deriving (Show)
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
-}

sync dest = do
  ex <- gets exclude
  src <- gets srcFolder
  srvr <- gets server
  usr <- gets user
  fol <- gets folder
  let cmd = "rsync -vaz --delete --delete-excluded --exclude-from="
            ++ ex ++ " " ++ addTrailingPathSeparator src ++ " "
            ++ usr ++ "@" ++ srvr ++ ":" ++ (fol </> dest)
  create cmd

makeHardLinkCopy src dst = do
  fol <- gets folder
  let cp = create $ "cp -al " ++ (fol </> src) ++ " " ++ (fol </> dst)
  checkExists src <*> cp

touch name = do
  fol <- gets folder
  create $ "touch " ++ (fol </> name)

shift src dst = checkExists src <*> move src dst

move src dest = do
  fol <- gets folder
  create $ "mv " ++ (fol </> src) ++ " " ++ (fol </> dest)

delete name = do
  fol <- gets folder
  create $ "rm -rf " ++ (fol </> name)

checkExists name = do
  fol <- gets folder
  create $ "[ -e " ++ (fol </> name) ++ " ]"

at :: BSt Actions -> Where -> BSt Actions
at a w = fmap (map (\(_,c) -> (w,c))) a

exec :: BSt Actions -> BSt Status
exec a = do
  acts <- a
  foldl' (\m act -> m <> exec' act) (return Okay) acts
exec' (loc,cmd) = do
  dry <- gets dryrun
  cmd' <- command loc cmd
  liftIO.putStrLn $ "RUNNING (" ++ show loc ++ "): " ++ cmd'
  if dry
    then return DryRun
    else liftIO (fmap fromBool $ run cmd')
    where fromBool True = Okay
          fromBool False = Error

command Nowhere cmd = do
  error $ "NO LOCATION SPECIFIED FOR: " ++ cmd
command Local cmd = return (cmd ++ " > /dev/null")
command Remote cmd = do
  usr <- gets user
  srv <- gets server
  return $ "ssh " ++ usr ++ "@" ++ srv ++ " " ++ cmd ++ " > /dev/null"

create :: String -> BSt Actions
create cmd = return [(Nowhere,cmd)]

(<*>) :: BSt Actions -> BSt Actions -> BSt Actions
(<*>) ma mb = liftM2 (++) ma mb

(<>) :: BSt Status -> BSt Status -> BSt Status
(<>) ma mb = do
  resA <- ma
  case resA of
    Okay -> mb
    Error -> return Error
    DryRun -> mb