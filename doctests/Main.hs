import System.FilePath.Glob
import Test.DocTest

main :: IO ()
main = glob "lib/**/*.hs" >>= doctest
