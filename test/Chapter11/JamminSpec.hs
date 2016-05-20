module Chapter11.JamminSpec where

import Chapter11.Jammin

import Test.Hspec

spec :: Spec
spec =
  describe "jammin" $
  let row1 =
        Jam {fruit = Peach
            ,jars = 1}
      row2 =
        Jam {fruit = Apple
            ,jars = 2}
      row3 =
        Jam {fruit = Plum
            ,jars = 1}
      row4 =
        Jam {fruit = Blackberry
            ,jars = 10}
      row5 =
        Jam {fruit = Apple
            ,jars = 1}
      row6 =
        Jam {fruit = Plum
            ,jars = 1}
      allJam = [row1,row2,row3,row4,row5,row6]
  in do it "totalJars counts all jars" $ totalJars allJam `shouldBe` 16
        it "mostJars gets the most row" $ mostJars allJam `shouldBe` row4
        it "groupJam groups the jams" $
          groupJam allJam `shouldBe` [[row1],[row2,row5],[row3,row6],[row4]]
