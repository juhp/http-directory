import Network.HTTP.Directory
import qualified Data.Text as T
import Data.Char
import Data.Maybe

main :: IO ()
main = do
  mgr <- httpManager
  let url = "https://downloads.haskell.org/~ghc/"
  dirs <- map T.unpack <$> httpDirectory mgr url
  let vs = filter (isDigit . head) dirs
  putStrLn $ "Latest ghc: " <> url <> last vs
