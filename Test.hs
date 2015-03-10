{-# LANGUAGE LambdaCase #-}

module Main where

  import Prelude hiding (interact)

  import Control.Arrow
  import Control.Monad
  import Control.Monad.Except
  import Control.Monad.Trans  (lift)
  import Control.Concurrent
  import Data.ByteString      (hPut)
  import System.IO
  import System.Directory
  import System.Hardware.Serialport

  import Text.Parsec

  import Config
  import Parser (parseCfg, parseCmdLine)


  cfgFileName  = "engulidor.cfg"
  dataFileName = "engulidor.dat"

  serialPortSettings = defaultSerialSettings { commSpeed = CS2400 }

  packetSize = 27


  main = (\ result -> runExceptT result >>= either putStrLn return)
       $ loadFile cfgFileName
     >>= ExceptT . return . left show . parseCfg cfgFileName
     >>= lift . interactCLI


  loadFile file = lift (doesFileExist file)
              >>= \case True  -> lift (readFile file)
                        False -> throwError (cfgFileName ++ " not found!")

  interactCLI config =
    do port     <- openSerial (portName config) serialPortSettings
       dataFile <- openFile dataFileName WriteMode
       listener <- forkIO (listen port dataFile)

       interact port (bindings config) -- ← This will hang until the user
                                       -- issues the quit command.
       killThread listener
       hClose dataFile
       closeSerial port

    where
      listen port dataFile = recv port packetSize
                         >>= hPut dataFile
                          >> listen port dataFile
                  

      interact port binds = do line <- getLine
                               when (isQuitCmd line) $
                                either
                                  print -- Print error.
                                  (void . send port)
                                  (parseCmdLine binds line)
                                >> interact port binds

      isQuitCmd (':':cmd) = lex cmd == [("q", "")] 
      isQuitCmd _ = False
