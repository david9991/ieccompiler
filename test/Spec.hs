import           Parser

main :: IO ()
main = do
  str <- readFile "test/out/syns.st"
  ast <- processAst "test/out/syns.st" str
  ir  <- processIR "test/out/syns.st" str
  writeFile "test/out/syns.st.ast" ast
  writeFile "test/out/syns.st.ll"  ir
