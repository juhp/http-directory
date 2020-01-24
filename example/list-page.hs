import Network.HTTP.Directory
import qualified Data.Text.IO as T
import System.Environment

main :: IO ()
main = do
  [url] <- getArgs
  mgr <- httpManager
  dirs <- httpDirectory mgr url
  mapM_ T.putStrLn dirs
