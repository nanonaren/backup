module Main
    (
    ) where

import System.Process
import System.Environment (getArgs)
import GHC.IO.Exception

main = do
  server <- fmap (!!0) getArgs
  folder <- fmap (!!1) getArgs
  absSrc <- fmap (!!2) getArgs
  excludes <- fmap (!!2) getArgs
  num <- fmap (read.(!!4)) getArgs
  doWhich <- fmap (!!5) getArgs

  --check if a backup is running already
  running <- fmap (elem "RUNNING".words) $
             readFile "/home/narens/Desktop/backup_status"
  --check destination
  b <- checkExists server (folder ++ "/SERVER_EXISTENCE_DIR")
  if not running then
      writeFile "/home/narens/Desktop/backup_status" "RUNNING" >>
      case doWhich of
        "hourly" ->
            run server folder absSrc excludes num >>= \r ->
      writeFile "/home/narens/Desktop/backup_status" "DONE" >> return r
  else
      putStrLn "Already Running" >> return False

runHigher server folder prefix num = do
  delete server folder (fol prefix (num-1))
  mapM_ (\(a,b) -> shift server folder (fol prefix a) (fol prefix b)) $
        zip (reverse [0..num-2]) (reverse [1..num-1])
  makeHardLinkCopy server folder (fol "hourly" 0) (fol prefix 1)

run server folder absSrc excludes numHourly = do
  --delete oldest hour
  delete server folder (fol (numHourly -1))
  --move n-1 to n
  mapM_ (\(a,b) -> shift server folder (fol a) (fol b)) $
        zip (reverse [1..numHourly-2]) (reverse [2..numHourly-1])
  --make hard link of latest
  makeHardLinkCopy server folder (fol 0) (fol 1)
  --sync
  sync server folder absSrc (fol 0) excludes
  --touch
  touch server folder (fol 0)
    where fol x = "hourly." ++ show x

sync server folder absSrc dst excludes =
    printCmd cmd >> system cmd >>= checkPassed
    where cmd = "rsync -va --delete --delete-excluded --exclude-from="
                ++ excludes ++ " " ++ absSrc
                ++ " narens@192.168.2.5:" ++ folder ++ "/" ++ dst

makeHardLinkCopy server folder src dst = do
  e <- checkExists server (src')
  if e then printCmd cmd >> system cmd >>= checkPassed else return True
    where cmd = "ssh " ++ server ++ " 'cp -al " ++ src' ++ " " ++ dst' ++ "'"
          src' = folder ++ "/" ++ src
          dst' = folder ++ "/" ++ dst

touch server folder name = printCmd cmd >> system cmd >>= checkPassed
    where cmd = "ssh " ++ server ++ " 'touch " ++ folder ++ "/" ++ name ++ "'"

shift server folder src dst = do
  e <- checkExists server (folder ++ "/" ++ src)
  if e then move server folder src dst else return True

move server folder nameSrc nameDst =
    printCmd cmd >> system cmd >>= checkPassed
    where cmd = "ssh " ++ server ++ " 'mv " ++ src ++ " " ++ dst ++ "'"
          src = folder ++ "/" ++ nameSrc
          dst = folder ++ "/" ++ nameDst

delete server folder name =
    printCmd cmd >> system cmd >>= checkPassed
    where cmd = "ssh " ++ server ++ " 'rm -rf " ++ folder
                ++ "/" ++ name ++ "'"

checkExists server folder =
    printCmd cmd >> system cmd >>= checkPassed
    where cmd = "ssh " ++ server ++ " '[ -d " ++ folder ++ " ]'"

printCmd cmd = putStrLn cmd

checkPassed ExitSuccess = return True
checkPassed _ = return False