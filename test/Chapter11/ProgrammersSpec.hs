module Chapter11.ProgrammersSpec where

import Chapter11.Programmers

import Test.Hspec

spec :: Spec
spec =
  describe "programmers" $
  do it "allProgrammers" $
       allProgrammers `shouldBe`
       [Programmer {os = GnuPlusLinux
                   ,lang = Haskell}
       ,Programmer {os = GnuPlusLinux
                   ,lang = Agda}
       ,Programmer {os = GnuPlusLinux
                   ,lang = Idris}
       ,Programmer {os = GnuPlusLinux
                   ,lang = PureScript}
       ,Programmer {os = OpenBSDPlusNevermindJustBSDStill
                   ,lang = Haskell}
       ,Programmer {os = OpenBSDPlusNevermindJustBSDStill
                   ,lang = Agda}
       ,Programmer {os = OpenBSDPlusNevermindJustBSDStill
                   ,lang = Idris}
       ,Programmer {os = OpenBSDPlusNevermindJustBSDStill
                   ,lang = PureScript}
       ,Programmer {os = Mac
                   ,lang = Haskell}
       ,Programmer {os = Mac
                   ,lang = Agda}
       ,Programmer {os = Mac
                   ,lang = Idris}
       ,Programmer {os = Mac
                   ,lang = PureScript}
       ,Programmer {os = Windows
                   ,lang = Haskell}
       ,Programmer {os = Windows
                   ,lang = Agda}
       ,Programmer {os = Windows
                   ,lang = Idris}
       ,Programmer {os = Windows
                   ,lang = PureScript}]
     it "adds up" $
       length allProgrammers `shouldBe` length allOperatingSystems * length allLanguages
