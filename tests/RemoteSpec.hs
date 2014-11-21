module RemoteSpec where

import  Test.Hspec

import Remote

spec :: Spec
spec =

    describe "The Remote module" $ do

        it "returns all repos" $
            getRemoteRepoList' "" rawGitoliteInfo
                `shouldBe` [ "braun/14WS/algdatI/testing"
                           , "braun/14WS/compiler/testing"
                           , "braun/14WS/sweng/testing"
                           , "testing"
                           ]

        it "returns only repos containing \"algdat\"" $
            getRemoteRepoList' "algdat" rawGitoliteInfo
                `shouldBe` ["braun/14WS/algdatI/testing"]

        it "returns all repos containing \"14WS\"" $
            getRemoteRepoList' "14WS" rawGitoliteInfo
                `shouldBe` [ "braun/14WS/algdatI/testing"
                           , "braun/14WS/compiler/testing"
                           , "braun/14WS/sweng/testing"
                           ]


rawGitoliteInfo :: String
rawGitoliteInfo = "\
  \hello obraun, this is git@gitolite ..\n\
  \\n\
  \ R W braun/14WS/algdatI/testing\n\
  \ R W braun/14WS/compiler/testing\n\
  \ R W braun/14WS/sweng/testing\n\
  \ R W testing\n\
  \\n\
  \More Infos ..."
