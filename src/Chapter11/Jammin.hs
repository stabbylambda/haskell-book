module Chapter11.Jammin  where
import Data.List

data Fruit
  = Peach
  | Apple
  | Plum
  | Blackberry
  deriving (Eq,Ord,Show)

data JamJars =
  Jam {fruit :: Fruit
      ,jars :: Int}
  deriving (Eq,Ord,Show)


totalJars :: [JamJars] -> Int
totalJars = sum . map jars

mostJars :: [JamJars] -> JamJars
-- mostJars = head . reverse . sort
mostJars = maximum

compareKind :: JamJars -> JamJars -> Ordering
compareKind (Jam k _) (Jam k' _) = compare k k'

sameKind :: JamJars -> JamJars -> Bool
sameKind j j' = compareKind j j' == EQ

groupJam :: [JamJars] -> [[JamJars]]
groupJam = groupBy sameKind . sortBy compareKind
