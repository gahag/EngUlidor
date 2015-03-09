module Parser where

  import Prelude hiding (concat)

  import Control.Applicative  ((<$>), (<*>), (<*))
  import Data.ByteString      (concat, pack, singleton)
  import Data.Char            (digitToInt)

  import Text.Parsec          (Parsec, (<?>), (<|>), endBy, many1, noneOf, parse
                               , parserZero, sepBy1, try)
  import Text.Parsec.Char     (char, hexDigit, spaces)
  import Text.Parsec.Language (emptyDef)
  import qualified Text.Parsec.Token as Token (identLetter, identStart
                                               , identifier, makeTokenParser
                                               , symbol)

  import Config (Cfg(Cfg))


  cfgDef = emptyDef {
      Token.identStart  = noneOf " ="
    , Token.identLetter = noneOf " ="
  }

  lexer = Token.makeTokenParser cfgDef

  ident  = Token.identifier lexer
  symbol = Token.symbol     lexer


  hexNumber = foldl (\ x -> (16 * x +) . fromIntegral . digitToInt) 0
           <$> many1 hexDigit
           <?> "hexadecimal number"

  hexNumbers = sepBy1 hexNumber spaces
            <?> "hexadecimal numbers"


  binding = (,)
         <$> ident
         <*  symbol "="
         <*> (pack <$> hexNumbers)
         <?> "binding"

  bindings = endBy binding (char '\n')
          <?> "bindings"


  portName = many1 (noneOf " \n")
          <?> "port name"


  cfg = Cfg
     <$> portName
     <*> bindings

  parseCfg = parse cfg


  cmdLine binds = concat
               <$> sepBy1 atom spaces
    where
      atom = try bind
          <|> (singleton <$> hexNumber)

      bind = ident
         >>= maybe parserZero return . (`lookup` binds)

  parseCmdLine = parse . cmdLine
