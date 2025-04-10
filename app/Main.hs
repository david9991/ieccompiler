module Main where

import           Parser
import           System.Environment
import           System.IO

main :: IO ()
main = do
  args <- getArgs
  outputFile args (length args)

outputFile :: [String] -> Int -> IO ()
outputFile a x
  | x /= 1 = hPutStrLn stderr "no input"
  | otherwise = do
    let f = head a
    str <- readFile $ f
    ast <- processAst f str
    ir  <- processIR f str
    writeFile (f ++ ".ast") ast
    writeFile (f ++ ".ll")  ir
