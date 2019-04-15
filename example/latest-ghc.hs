import Network.HTTP.Directory
import qualified Data.Text as T
import Data.Char
import Data.Maybe
import Text.Regex

main :: IO ()
main = do
  mgr <- httpManager
  let url = "https://downloads.haskell.org/~ghc/"
  dirs <- map T.unpack <$> httpDirectory mgr url
  let rg = mkRegex "[1-9][0-9.]+/"
      vs = filter (isJust . matchRegex rg) dirs
  putStrLn $ "Latest ghc: " <> url <> last vs
