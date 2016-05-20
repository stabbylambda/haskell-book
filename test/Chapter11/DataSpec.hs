module Chapter11.DataSpec where

import Chapter11.Data

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  let myCar = Car Mini (Price 14000)
      urCar = Car Mazda (Price 20000)
      clownCar = Car Tata (Price 7000)
      doge = Plane PapuAir 1000
  in do describe "vehicles" $
          do it "isCar should get cars" $
               (isCar myCar,isCar doge) `shouldBe` (True,False)
             it "isPlane should get planes" $
               (isPlane myCar,isPlane doge) `shouldBe` (False,True)
             it "areCars should get allCars" $
               areCars [myCar,urCar,doge] `shouldBe` [True,True,False]
             it "getManu should get car manufacturers" $
               (getManu myCar, getManu urCar, getManu clownCar) `shouldBe` (Just Mini, Just Mazda, Just Tata)
             it "getManu should not get plane manufacturers" $
               getManu doge `shouldBe` Nothing
