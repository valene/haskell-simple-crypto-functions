--Program to decrypt ceasar cipher text
--takes commandline args for input cipher file
--cipher key is not known so goes through all possible combinations.

--Additional Notes: Optparse-applicative functions the following way
--define a data structure for the arguments expected
--Make a parser out of the data structure
--run it through and pass it for further eval.

--optparse applicative takes 4 kinds of options
--regular options: strOption || option auto , value "default value"
--flag : switch
--arguments: argument str 
--finally, commands : subparser ( datastructure) //complicated refer wiki if encountered.

import Data.Char
import System.IO
import Control.Monad
import Data.Maybe
import Data.Monoid 
import Options.Applicative

data Cliargs = Cliargs { infile :: String
                       , key :: Maybe Int
                       , outfile :: String }


turnwheel :: Int -> String -> String
turnwheel n xs = unwords . map turnwheel' . words $ xs
  where 
    turnwheel' [] = []
    turnwheel' (x:xs) | (isAlpha x) && (isUpper x) = (chr. (+) 65 $ rem (ord x - 65 + n) 26):(turnwheel' xs)
                      | isAlpha x = (chr . (+) 97 $ rem (ord x -97 + n) 26):(turnwheel' xs)
                      | otherwise = x:(turnwheel' xs)


decrypt:: Cliargs -> IO ()
decrypt (Cliargs infile k output) = do
                                      contents <- readFile infile
                                      case k of
                                        Just n -> writeFile output $ turnwheelof n contents
                                        Nothing -> writeFile output . concat $ map(\y -> turnwheelof y contents) [1..26]
                                      where 
                                        turnwheelof n xs = ("\n \n key value :" ++ (show n) ++ "\n") ++ (unlines . map(\x -> turnwheel n x) . lines $ xs)
                                        

readstr0 :: String -> String
readstr0 = read

cliargs :: Parser Cliargs
cliargs = Cliargs
  <$> strOption
        ( long "input"
        <> short 'i'
        <> metavar "FILE"
        <> help "Input file with cipher" )
  <*> ( optional $ option auto
                    ( long "key"
                  <> short 'k'
                  <> metavar "INTEGER"
                  <> help "Key value in Integer" ))
  <*> strOption
        ( long "output"
        <> short 'o'
        <> value "decryptedwriteFile.txt"
        <> metavar "FILE"
        <> help "Output derypted File " )


main :: IO ()
main = execParser opts >>= decrypt
  where 
    opts = info (helper <*> cliargs )
      ( fullDesc
      <> progDesc "Print the required list of arguments"
      <> header "A simple script for ceasar cipher decrypting" )
