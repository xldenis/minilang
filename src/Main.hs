module Main where
  import Control.Applicative 
  import Control.Monad
  import System.Environment
  import System.FilePath.Posix

  import Data.Attoparsec.ByteString.Char8
  import qualified Data.ByteString.Char8 as B


  import Minilang.Parser
  import Minilang.Check
  import Minilang.Types
  import Minilang.Compile
  
  main :: IO ()
  main = do 
    args <- getArgs
    files <- mapM (liftM (parseOnly minilang)  <$> B.readFile) args
    case (head files) of
      Right a -> do
        let fname = takeFileName (head args)
        writeFile (replaceExtension (fname) "pretty.minl") $ show a
        case (check a) of 
          Left (a, t) -> do
            print a
            writeFile (replaceExtension (fname) "symbol.txt") $ (show t)
          Right t -> do
            writeFile (replaceExtension (fname) "symbol.txt") $ (show t)
            writeFile (replaceExtension (fname) "c") $ (compile t a)
      Left err -> putStrLn err
