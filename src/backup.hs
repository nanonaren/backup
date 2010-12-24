{-# LANGUAGE DeriveDataTypeable #-}
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
import System.Console.CmdArgs
import HSH
import System.FilePath
import Data.List (foldl')
import System.Directory (getHomeDirectory)

data Options = Options
    {
      dryrun :: Bool,
      server :: String,
      user :: String,
      bakFolder :: FilePath,
      src :: [FilePath],
      exclude :: FilePath,
      statusFile :: FilePath,
      numHourly :: Int
    } deriving (Show,Data,Typeable)

opts = Options
  {
    dryrun = def &= help "Show the commands that will be executed",
    server = def &= help "Server address" &= typ "ADDRESS",
    user = def &= help "Username at server" &= typ "NAME",
    bakFolder = def &= help "Absolute path to backup folder at server" &= typDir,
    src = def &= help "Absolute path to source folder. Use this flag \
                        \multiple times for multiple folders" &= typDir,
    exclude = def &= help "Rsync excludes file" &= typFile,
    statusFile = def &= help "Status file" &= typFile,
    numHourly = def &= help "Number of backups to keep" &= typ "NUM"
  } &= program "backup"
    &= summary "Clean and simple Rsync backup"
    &= details ["Visit http://github.com/nanonaren for source."]

type BSt = StateT Options IO
type Action = (Where,String)
type Actions = [Action]

data Where = Remote | Local | Nowhere deriving (Show)
data Status = Okay | Error | DryRun deriving (Show,Eq)

main = do
  options <- cmdArgs opts
  putStrLn (show options)
  --checkOptions options
  let statusf = statusFile options
  --check if a backup is running already
  running <- fmap (elem "RUNNING".words) $ readFile statusf
  if not running
    then
      writeFile statusf "RUNNING" >>
      fmap (==Okay) (evalStateT (exec runSync) options) >>= \r ->
      writeFile statusf "DONE" >> return r
    else
      putStrLn "Already Running" >> return False

--checkOptions 

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
   <*> syncAll (fol 0) `at` Local
   <*> touch (fol 0) `at` Remote

syncAll dest = do
  (s:sources) <- gets src
  foldl' (\acc x -> acc <*> sync x dest) (sync s dest) sources

sync src dest = do
  ex <- gets exclude
  srvr <- gets server
  usr <- gets user
  fol <- gets bakFolder
  let cmd = "rsync -vaz --delete --delete-excluded --exclude-from="
            ++ ex ++ " " ++ dropTrailingPathSeparator src ++ " "
            ++ usr ++ "@" ++ srvr ++ ":" ++ (fol </> dest)
  create cmd

makeHardLinkCopy src dst = do
  fol <- gets bakFolder
  let cp = create $ "cp -al " ++ (fol </> src) ++ " " ++ (fol </> dst)
  cp `on` checkExists src

touch name = do
  fol <- gets bakFolder
  create $ "touch " ++ (fol </> name)

move src dest = do
  fol <- gets bakFolder
  let mv = create $ "mv -u " ++ (fol </> src) ++ " " ++ (fol </> dest)
  mv `on` checkExists src

delete name = do
  fol <- gets bakFolder
  create $ "rm -rf " ++ (fol </> name)

checkExists name = do
  fol <- gets bakFolder
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