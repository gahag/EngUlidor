module Config where

  import Data.ByteString  (ByteString)


  type Binding = (String, ByteString)

  data Cfg = Cfg { portName :: String
                 , bindings :: [Binding] }
