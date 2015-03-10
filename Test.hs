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

  packetSize = 27


  main = (\ result -> runExceptT result >>= either putStrLn return)
       $ loadFile cfgFileName
     >>= ExceptT . return . left show . parseCfg cfgFileName
     >>= lift . \ config -> openSerial (portName config) defaultSerialSettings
                                                          { commSpeed = CS2400 }
                       >>= interactCLI (bindings config)
    where
      loadFile file = lift (doesFileExist file)
                  >>= \case True  -> lift (readFile file)
                            False -> throwError (cfgFileName ++ " not found!")

      interactCLI binds port =
        do dataFile <- openFile dataFileName WriteMode
           listener <- forkIO (listen dataFile)
           interact -- This will hang until the user issues the quit command.
           killThread listener
           hClose dataFile
        where
          listen dataFile = recv port packetSize
                        >>= hPut dataFile
                         >> listen dataFile
                      

          interact = getLine
                 >>= \ line -> when (line /= ":q") $
                                either
                                  print
                                  (void . send port)
                                  (parseCmdLine binds "<interactive>" line)
                                >> interact
