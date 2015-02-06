module Main where
  import Control.Monad
  import System.Environment
  import Control.Applicative 

  import Data.Attoparsec.ByteString.Char8
  import qualified Data.ByteString.Char8 as B


  import Minilang.Parser
  import Minilang.Check
  import Minilang.Types
  
  main :: IO ()
  main = do 
    args <- getArgs
    files <- mapM (liftM (parseOnly minilang)  <$> B.readFile) args
    print $ map (\x -> case x of 
      Right a -> a) files    
 