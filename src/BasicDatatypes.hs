module BasicDatatypes  where

data Mood = Blah | Woot deriving Show 
--    [1]   [2] [3] [4]
-- 1 - type constructor
-- 2 - data constructor for the value Blah
-- 3 - the pipe, indicates sum type or logical disjunction or
-- 4 - data constructor for the value Woot

instance Eq Mood where
  (==) Blah Blah = True
  (==) Woot Woot = True
  (==) _ _ = False



data Town = Town {women :: Integer,
            men :: Integer} deriving Show

data GirlDormitory = GirlDormitory {students :: Integer,
                                    address :: String}
class Inhabited a where
  inhabitants :: a -> Integer

instance Inhabited Town where
  inhabitants a = women a + men a

instance Inhabited GirlDormitory where
  inhabitants a = students a
  
instance Eq Town where
  (==) a b = women a == women b && men a == men b

addInhabitants :: (Inhabited a, Inhabited b) => a -> b -> Integer
addInhabitants a1 a2 = inhabitants a1 + inhabitants a2
