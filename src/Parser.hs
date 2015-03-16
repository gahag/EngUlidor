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

{-# LANGUAGE NoMonomorphismRestriction #-}

module Parser where

  import Prelude hiding (concat)

  import Control.Applicative  ((<$>), (<$), (<*>), (<*), (*>))
  import Data.ByteString      (concat, pack, singleton)
  import Data.Char            (digitToInt)

  import Text.Parsec          ((<?>), (<|>), endBy, eof, many1, noneOf, parse
                               , sepBy1, try)
  import Text.Parsec.Char     (char, endOfLine, hexDigit, oneOf)
  import Text.Parsec.Language (emptyDef)
  import qualified Text.Parsec.Token as Token (identLetter, identStart
                                               , identifier, makeTokenParser
                                               , symbol, whiteSpace)

  import Data (Cfg(Cfg), Cmd(..))


  cfgDef = emptyDef {
      Token.identStart  = noneOf " =\n\r"
    , Token.identLetter = noneOf " =\n\r"
  }

  lexer = Token.makeTokenParser cfgDef

  ident      = Token.identifier lexer
  symbol     = Token.symbol     lexer
  whiteSpace = Token.whiteSpace lexer


  blankSpace = many1 (oneOf " \t\r\f\v\xa0")


  hexNumber = foldl (\ x -> (16 * x +) . fromIntegral . digitToInt) 0
           <$> many1 hexDigit
           <?> "hexadecimal number"

  hexNumbers = sepBy1 hexNumber blankSpace
            <?> "hexadecimal numbers"


  binding = (,)
         <$> ident
         <*  symbol "="
         <*> (pack <$> hexNumbers)
         <?> "binding"

  bindings = endBy (whiteSpace *> binding) endOfLine
          <?> "bindings"


  portName = ident
          <?> "port name"


  cfg = Cfg
     <$> portName
     <*> bindings

  parseCfg = parse (cfg <* eof)


  dataList binds = concat
               <$> sepBy1 atom blankSpace
    where
      atom = try (ident >>= \ bind -> maybe (fail $ "undeclared binding: " ++ bind)
                                            return $ lookup bind binds)
          <|> (singleton <$> hexNumber)

  parseDataList binds = parse (dataList binds <* eof) "<interactive>"


  cmd =  Quit  <$ char 'q'
     <|> Help  <$ char 'h'
     <|> Binds <$ char 'b'

  parseCmd = parse (cmd <* eof) "<interactive>"
