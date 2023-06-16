import System.IO
import System.Environment

main :: IO ()
main = getArgs >>= readFile . head >>= putStrLn 

