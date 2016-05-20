module Chapter11.Programmers where

data OperatingSystem
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq,Enum,Bounded,Show)

data ProgrammingLanguage
  = Haskell
  | Agda
  | Idris
  | PureScript deriving (Eq,Enum, Bounded,Show)
data Programmer =
  Programmer {os   :: OperatingSystem
             ,lang :: ProgrammingLanguage}
  deriving (Eq,Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [minBound..]

allLanguages :: [ProgrammingLanguage]
allLanguages = [minBound..]

allProgrammers :: [Programmer]
allProgrammers = concatMap (\o -> map (\l -> Programmer { os = o, lang = l}) allLanguages) allOperatingSystems
