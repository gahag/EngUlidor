{-# LANGUAGE LambdaCase #-}

module Main where

  import Prelude hiding (interact)

  import Control.Arrow              (left)
  import Control.Monad              (unless, void)
  import Control.Monad.Except       (ExceptT(..), runExceptT, throwError)
  import Control.Monad.Trans        (lift)
  import Control.Concurrent         (forkIO, killThread)
  import Data.ByteString            (hPut)
  import System.IO                  (IOMode(WriteMode), withFile)
  import System.Directory           (doesFileExist)
  import System.Hardware.Serialport (CommSpeed(CS115200)
                                     , SerialPortSettings(commSpeed)
                                     , closeSerial, defaultSerialSettings
                                     , openSerial, recv, send)

  import Config (Cfg(bindings, portName))
  import Parser (parseCfg, parseCmdLine)


  cfgFileName  = "engulidor.cfg"
  dataFileName = "engulidor.dat"

  serialPortSettings = defaultSerialSettings { commSpeed = CS115200 }

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
       listener <- forkIO (listen port)

       interact port (bindings config) -- ← This will hang until the user
                                       -- issues the quit command.
       killThread listener
       closeSerial port

    where
      listen port = withFile dataFileName WriteMode listen'
        where
          listen' dataFile = recv port packetSize
                         >>= hPut dataFile
                          >> listen' dataFile
                  

      interact port binds = do line <- getLine
                               unless (isQuitCmd line) $
                                either
                                  print -- Print error.
                                  (void . send port)
                                  (parseCmdLine binds line)
                                >> interact port binds

      isQuitCmd (':':cmd) = lex cmd == [("q", "")] 
      isQuitCmd _ = False
