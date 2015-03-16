-- This file is part of Engulidor.
-- 
-- Engulidor is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- Engulidor is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with Engulidor.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE LambdaCase #-}

module Main where

  import Prelude hiding (interact)

  import Control.Applicative        ((<$>))
  import Control.Arrow              (left)
  import Control.Monad.Except       (ExceptT(..), runExceptT, throwError)
  import Control.Monad.Trans        (lift)
  import Control.Concurrent         (forkIO, killThread)
  import Data.ByteString            (hPut)
  import System.IO                  (IOMode(WriteMode), hFlush, stdout, withBinaryFile)
  import System.Directory           (doesFileExist)
  import System.Hardware.Serialport (CommSpeed(CS115200)
                                     , SerialPortSettings(commSpeed)
                                     , closeSerial, defaultSerialSettings
                                     , openSerial, recv, send)

  import Data (Cfg(..), Cmd(..))
  import Parser (parseCfg, parseCmd, parseDataList)


  cfgFileName  = "engulidor.cfg"
  dataFileName = "engulidor.dat"

  serialPortSettings = defaultSerialSettings { commSpeed = CS115200 }

  packetSize = 27


  help = unlines [ "Engulidor - made by gahag."
                 , "Type hex data, bind or command."
                 , "All commands must be prefixed with a colon, one at a line."
                 , "The available commands are:"
                 , "q -> exit"
                 , "h -> show this text."
                 , "b -> show the current bindings" ]

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

       putStrLn help
       interact port (bindings config) -- ← This will hang until the user
                                       -- issues the quit command.
       killThread listener
       closeSerial port

    where
      listen port = withBinaryFile dataFileName WriteMode listen'
        where
          listen' dataFile = recv port packetSize
                         >>= hPut dataFile
                          >> listen' dataFile
                  

      interact port binds = let reinteract = (>> interact port binds) in
            putStr ">> " >> hFlush stdout >> getLine
        >>= either (reinteract . print) id
          . \case (':': cmd) -> (\case Quit  -> return ()
                                       Help  -> reinteract $ putStrLn help
                                       Binds -> reinteract $ print binds)
                            <$> parseCmd cmd

                  line -> reinteract . send port
                      <$> parseDataList binds line
