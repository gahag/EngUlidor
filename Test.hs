{-# LANGUAGE LambdaCase #-}

module Test where

  import Prelude hiding (interact)

  import Control.Arrow
  import Control.Monad
  import Control.Monad.Except
  import Control.Monad.Trans  (lift)
  import System.Directory
  import System.Hardware.Serialport

  import Text.Parsec

  import Config
  import Parser (parseCfg, parseCmdLine)


  cfgFileName = "engulidor.cfg"

  main = loadFile cfgFileName
     >>= ExceptT . return . left show . parseCfg cfgFileName
     >>= lift . \ config -> openSerial (portName config) defaultSerialSettings
                                                          { commSpeed = CS2400 }
                       >>= interact (bindings config)
    where
      loadFile file = lift (doesFileExist file)
                  >>= \case True  -> lift (readFile file)
                            False -> throwError ("'" ++ cfgFileName ++ "' not found")

      interact binds port = getLine
                        >>= either print (void . send port)
                          . parseCmdLine binds "<interactive>"
                         >> interact binds port
