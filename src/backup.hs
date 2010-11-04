{-
Inspired totally by (because it works charmingly well!):
http://www.mikerubel.org/computers/rsync_snapshots/
-}

module Main
    (
      main
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
import System.Directory (getHomeDirectory)

arguments =
    [
     Arg "dryrun" (Just 'd') (Just "dryrun") Nothing
       "Do a dry a run instead and print what will be done",
     Arg "server" (Just 's') (Just "server")
       (argDataRequired "HOSTNAME" ArgtypeString) "Address of server",
     Arg "user" (Just 'x') (Just "user")
       (argDataRequired "USERNAME" ArgtypeString) "Username",
     Arg "folder" (Just 'f') (Just "folder")
       (argDataRequired "PATH" ArgtypeString) "Absolute path of backup folder on destination",
     Arg "src" (Just 'r') (Just "src")
       (argDataRequired "PATH" ArgtypeString) "Absolute path to source folder",
     Arg "exclude" (Just 'e') (Just "exclude")
       (argDataRequired "FILE" ArgtypeString) "Path to exclude file",
     Arg "numHourly" (Just 'u') (Just "numHourly")
       (argDataRequired "NUM" ArgtypeInt)
       "Number of hourly backups to keep"
    ]

data Backup = Backup
    {
      user :: String,
      server :: String,
      folder :: FilePath,
      srcFolder :: FilePath,
      exclude :: FilePath,
      dryrun :: Bool,
      numHourly :: Int
    }

type BSt = StateT Backup IO
type Action = (Where,String)
type Actions = [Action]

data Where = Remote | Local | Nowhere deriving (Show)
data Status = Okay | Error | DryRun deriving (Show,Eq)

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
                 dryrun = gotArg ags "dryrun",
                 numHourly = getRequiredArg ags "numHourly"
               }
  statusf <- fmap (</> ".backup_status") getHomeDirectory
  --check if a backup is running already
  running <- fmap (elem "RUNNING".words) $ readFile statusf
  if not running
    then
      writeFile statusf "RUNNING" >>
      fmap (==Okay) (evalStateT (exec runSync) backup) >>= \r ->
      writeFile statusf "DONE" >> return r
    else
      putStrLn "Already Running" >> return False


{-
--delete oldest hour
--move n-1 to n
--make hard link of latest
--sync
--touch
-}
runSync = do
  numH <- gets numHourly
  let fol x = "hourly." ++ show x
      chk = checkExists "SERVER_EXISTENCE_DIR"
      del = delete (fol (numH -1))
      mvs = foldl' (\c (a,b) -> c <*> move (fol a) (fol b)) (return []) $
            zip (reverse [1..numH-2]) (reverse [2..numH-1])
      link = makeHardLinkCopy (fol 0) (fol 1)

  (chk <*> del <*> mvs <*> link) `at` Remote
   <*> sync (fol 0) `at` Local
   <*> touch (fol 0) `at` Remote

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
  cp `on` checkExists src

touch name = do
  fol <- gets folder
  create $ "touch " ++ (fol </> name)

move src dest = do
  fol <- gets folder
  let mv = create $ "mv -u " ++ (fol </> src) ++ " " ++ (fol </> dest)
  mv `on` checkExists src

delete name = do
  fol <- gets folder
  create $ "rm -rf " ++ (fol </> name)

checkExists name = do
  fol <- gets folder
  create $ "[ -e " ++ (fol </> name) ++ " ]"

infixl 2 `at`
at :: BSt Actions -> Where -> BSt Actions
at a w = fmap (map (\(_,c) -> (w,c))) a

--do act on cond otherwise true
--There should only be one action in each
--sorry, I know its ugly
on act cond = do
  (_,cm) <- fmap last cond
  (_,am) <- fmap head act
  let cmd = cm ++ " && " ++ am ++ " || true" 
  create cmd

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
  return $ "ssh " ++ usr ++ "@" ++ srv ++ " '" ++ cmd ++ "' > /dev/null"

create :: String -> BSt Actions
create cmd = return [(Nowhere,cmd)]

infixl 1 <*>
(<*>) :: BSt Actions -> BSt Actions -> BSt Actions
(<*>) ma mb = liftM2 (++) ma mb

(<>) :: BSt Status -> BSt Status -> BSt Status
(<>) ma mb = do
  resA <- ma
  case resA of
    Okay -> mb
    Error -> return Error
    DryRun -> mb