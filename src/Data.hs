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

{-# LANGUAGE FlexibleInstances, OverlappingInstances, ViewPatterns #-}

module Data where

  import Data.ByteString        (ByteString, unpack)
  import Data.Function          (on)
  import Data.List              (maximumBy)
  import qualified Numeric as N (showHex)


  type Binding = (String, ByteString)

  data Cfg = Cfg { portName :: String
                 , bindings :: [Binding] }


  data Cmd = Help
           | Quit
           | Binds

  instance Show [Binding] where
    show (unzip -> (binds, map unpack -> vals)) =
      let max     = length $ maximumBy (compare `on` length) binds
          binds'  = map (\ s -> s ++ replicate (max - length s) ' ') binds
          showHex = flip N.showHex []
      in unlines
         $ zipWith (\ s -> ((s ++ " = ") ++) . unwords . map showHex) binds' vals
