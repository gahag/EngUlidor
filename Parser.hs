module Parser where

  import Prelude hiding (concat)

  import Control.Applicative  ((<$>), (<*>), (<*), (*>))
  import Data.ByteString      (concat, pack, singleton)
  import Data.Char            (digitToInt)

  import Text.Parsec          (Parsec, (<?>), (<|>), endBy, many1, noneOf, parse
                               , parserZero, sepBy1, try)
  import Text.Parsec.Char     (char, endOfLine, hexDigit, oneOf)
  import Text.Parsec.Language (emptyDef)
  import qualified Text.Parsec.Token as Token (identLetter, identStart
                                               , identifier, makeTokenParser
                                               , symbol, whiteSpace)

  import Config (Cfg(Cfg))


  cfgDef = emptyDef {
      Token.identStart  = noneOf " =\n\r"
    , Token.identLetter = noneOf " =\n\r"
  }

  lexer = Token.makeTokenParser cfgDef

  ident      = Token.identifier lexer
  symbol     = Token.symbol     lexer
  whiteSpace = Token.whiteSpace lexer


  hexNumber = foldl (\ x -> (16 * x +) . fromIntegral . digitToInt) 0
           <$> many1 hexDigit
           <?> "hexadecimal number"

  hexNumbers = sepBy1 hexNumber (many1 $ oneOf " \t")
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

  parseCfg = parse cfg


  cmdLine binds = concat
               <$> sepBy1 atom (many1 $ oneOf " \t")
    where
      atom = try (ident >>= maybe parserZero return . (`lookup` binds))
          <|> (singleton <$> hexNumber)

  parseCmdLine = parse . cmdLine
