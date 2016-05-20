module Chapter11.Data where

data Price =
  Price Integer
  deriving (Eq,Show)

data Manufacturer
  = Mini
  | Mazda
  | Tata
  deriving (Eq,Show)

data Airline
  = PapuAir
  | CatapultsRUs
  | TakeYourChancesUnited
  deriving (Eq,Show)

data Vehicle
  = Car Manufacturer Price
  | Plane Airline Integer
  deriving (Eq, Show)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: (Monad m) => Vehicle -> m Manufacturer
getManu (Car manu _) = return manu
getManu _ = fail "Can't get manufacturer of a plane"
