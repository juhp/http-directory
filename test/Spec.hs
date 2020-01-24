import Data.Maybe
import Test.Hspec
import Network.HTTP.Directory

main :: IO ()
main = hspec $ parallel spec

spec :: Spec
spec = do
  describe "httpDirectory" $ do
    it "empty google" $ do
      mgr <- httpManager
      fs <- httpDirectory mgr "https://google.com/"
      fs `shouldBe` []

    it "empty httpbin" $ do
      mgr <- httpManager
      fs <- httpDirectory mgr "http://httpbin.org/"
      fs `shouldBe` []

    it "my src hackage" $ do
      fs <- httpDirectory' "https://hackage.haskell.org/package/http-directory/src/"
      null fs `shouldBe` False

    it "myself hackage" $ do
      fs <- httpDirectory' "https://hackage.haskell.org/package/http-directory"
      null fs `shouldBe` False

    it "404" $
      httpDirectory' "https://httpbin.org/404"
      `shouldThrow` anyException

  describe "httpExists" $ do
    it "google" $ do
      mgr <- httpManager
      exists <- httpExists mgr "https://google.com/"
      exists `shouldBe` True

    it "404" $ do
      mgr <- httpManager
      exists <- httpExists mgr "https://httpbin.org/404"
      exists `shouldBe` False

    it "domain" $ do
      mgr <- httpManager
      httpExists mgr "http://nowhereparticular"
      `shouldThrow` anyException

  describe "httpFileSize" $ do
    it "httpbin/get" $ do
      mgr <- httpManager
      msize <- httpFileSize mgr "https://httpbin.org/get"
      isJust msize `shouldBe` True

    it "httpbin/0B" $ do
      mgr <- httpManager
      msize <- httpFileSize mgr "https://httpbin.org/bytes/0"
      msize `shouldBe` Just 0

    it "httpbin/64B" $ do
      mgr <- httpManager
      msize <- httpFileSize mgr "https://httpbin.org/bytes/64"
      msize `shouldBe` Just 64

    it "cabal" $ do
      mgr <- httpManager
      msize <- httpFileSize mgr "https://raw.githubusercontent.com/juhp/http-directory/master/http-directory.cabal"
      isJust msize `shouldBe` True

  describe "httpLastModified" $
    it "httpbin" $ do
      mgr <- httpManager
      mtime <- httpLastModified mgr "https://haskell.org/"
      isJust mtime `shouldBe` True

  describe "httpRedirect" $
    it "fedora" $ do
      mredir <- httpRedirect' "http://dl.fedoraproject.org"
      isJust mredir `shouldBe` True

  describe "httpRedirect" $ do
    it "fedora" $ do
      mredir <- httpRedirect' "http://download.fedoraproject.org"
      isJust mredir `shouldBe` True

    it "httpbin" $ do
      mredir <- httpRedirect' "http://httpbin.org/relative-redirect/1"
      isJust mredir `shouldBe` True

    it "3 redirs" $ do
      mgr <- httpManager
      redirs <- httpRedirects mgr "http://httpbin.org/relative-redirect/2"
      length redirs `shouldBe` 2

  describe "isHttpUrl" $ do
    it "http" $
      all isHttpUrl
        ["http://dl.fedoraproject.org", "https://haskell.org/"]
          `shouldBe` True

    it "non-http" $
      any isHttpUrl
         ["mailto:one@where", "somefile", "an.iso", "package.tgz"]
          `shouldBe` False