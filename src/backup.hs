module Main
    (
    ) where

import System.Process
import System.Environment (getArgs)
import GHC.IO.Exception
import Control.Monad (when)

main = do
  atype <- fmap (!!0) getArgs
  server <- fmap (!!1) getArgs
  folder <- fmap (!!2) getArgs
  absSrc <- fmap (!!3) getArgs
  excludes <- fmap (!!4) getArgs
  num <- fmap (read.(!!5)) getArgs

  --check if a backup is running already
  running <- fmap (elem "RUNNING".words) $
             readFile "/home/narens/Desktop/backup_status"
  let real = atype == "real"
  --check destination
  b <- checkExists real server (folder ++ "/SERVER_EXISTENCE_DIR")
  if not running
    then
      writeFile "/home/narens/Desktop/backup_status" "RUNNING" >>
      run real server folder absSrc excludes num >>= \r ->
      writeFile "/home/narens/Desktop/backup_status" "DONE" >> return r
    else
      putStrLn "Already Running" >> return False

{-
runHigher server folder prefix num = do
  delete server folder (fol prefix (num-1))
  mapM_ (\(a,b) -> shift server folder (fol prefix a) (fol prefix b)) $
        zip (reverse [0..num-2]) (reverse [1..num-1])
  makeHardLinkCopy server folder (fol "hourly" 0) (fol prefix 1)
-}

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
                ++ " narens@192.168.2.5:" ++ folder ++ "/" ++ dst

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

checkExists real server folder =
    printCmd cmd >> if real then system cmd >>= checkPassed else return True
    where cmd = "ssh " ++ server ++ " '[ -d " ++ folder ++ " ]'"

printCmd cmd = putStrLn cmd

checkPassed ExitSuccess = return True
checkPassed _ = return False