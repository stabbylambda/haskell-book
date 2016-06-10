module Chapter11.ChapterExercisesSpec where

import Chapter11.ChapterExercises

import Test.Hspec

phone :: Phone
phone =
  Phone [(1,"")
        ,(2,"ABC")
        ,(3,"DEF")
        ,(4,"GHI")
        ,(5,"JKL")
        ,(6,"MNO")
        ,(7,"PQRS")
        ,(8,"TUV")
        ,(9,"WXYZ")
        ,(10,"*^")
        ,(0,"+_")
        ,(11,"#.,")]

convo :: [String]
convo =
  ["Wanna play 20 questions"
  ,"Ya"
  ,"U 1st haha"
  ,"Lol ok. Have u ever tasted alcohol lol"
  ,"Lol ya"
  ,"Wow ur cool haha. Ur turn"
  ,"Ok. Do u think I am pretty Lol"
  ,"Lol ya"
  ,"Haha thanks just making sure rofl ur turn"]

spec :: Spec
spec =
  do describe "isSubsequenceOf" $
       do it "1" $ isSubsequenceOf "blah" "blahwoot" `shouldBe` True
          it "2" $ isSubsequenceOf "blah" "wootblah" `shouldBe` True
          it "3" $ isSubsequenceOf "blah" "wboloath" `shouldBe` True
          it "4" $ isSubsequenceOf "blah" "wootbla" `shouldBe` False
     describe "capitalizeWords" $
       do it "words" $
            let input = "hello world"
                output = [("hello","Hello"),("world","World")]
            in capitalizeWords input `shouldBe` output
          it "word" $ capitalizeWord "twitter" `shouldBe` "Twitter"
          it "para" $
            let input = "blah. woot ha."
                output = "Blah. Woot ha."
            in capitalizeParagraph input `shouldBe` output
     describe "keyPresses" $
       do it "A" $ keyPresses phone 'A' `shouldBe` (2, 1)
          it "B" $ keyPresses phone 'B' `shouldBe` (2, 2)
          it "Z" $ keyPresses phone 'Z' `shouldBe` (9, 4)
     describe "reverseTaps" $
       do it "one press for a" $ reverseTaps phone 'a' `shouldBe` [(2, 1)]
          it "caps and b for B" $ reverseTaps phone 'B' `shouldBe` [(10, 2), (2, 2)]
