{-#language MultiParamTypeClasses #-}
{-#language FlexibleInstances #-}

module Types where 

import Foreign.Marshal (newArray)
import Data.Semigroup (Semigroup, (<>))
import Data.Monoid (Sum)

t = "LOL"
class DoubleLike a where
    toDouble :: a -> Double

instance DoubleLike Bool where
    toDouble True  = 1.0
    toDouble _     = 0.0

instance DoubleLike Double where
    toDouble = id

class Normed v n where
    norm :: v n -> n

instance Floating a => Normed Vec3 a where
    norm v = sqrt $ foldr1 (+) $ (**2) <$> v

class MeasurableSpace v m where
    distance :: v m -> v m -> m

instance Floating a => MeasurableSpace Vec3 a where
    distance v1 v0 = norm $ v0 - v1

data Vec3 a = Vec3 {x :: a, y :: a, z :: a} 
    deriving (Show, Eq, Ord)

type Vec = Vec3 Double

infix 6 *|
s *| v = (*s) <$> v

dot v0 v1 = sum $ v0 * v1
normalize v = (1/norm v) *| v

xzPrj v = v * (Vec3 1 0 1)
xyPrj v = v * (Vec3 1 1 0)

instance Functor Vec3 where
    fmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

instance Applicative Vec3 where
    pure a = Vec3 a a a
    Vec3 fx fy fz <*> Vec3 x y z = Vec3 (fx x) (fy y) (fz z)

instance Num a => Num (Vec3 a) where
    v0 + v1 = (fmap (+) v0) <*> v1
    v0 * v1 = (fmap (*) v0) <*> v1
    fromInteger x = fromInteger <$> Vec3 x x x
    negate v      =  (*(-1)) <$> v
    signum v      = signum   <$> v
    abs    v      = abs      <$> v

instance Semigroup a => Semigroup (Vec3 a) where
    v0 <> v1 = (((<>) <$>) v0) <*> v1

instance Monoid a => Monoid (Vec3 a) where
    mempty  = Vec3 mempty mempty mempty
    mappend v0 v1 = ((mappend <$>) v0) <*> v1

zero :: Num a => Vec3 a
x1 :: Num a => Vec3 a
y1 :: Num a => Vec3 a
z1 :: Num a => Vec3 a

zero = Vec3 0 0 0
x1   = Vec3 1 0 0
y1   = Vec3 0 1 0
z1   = Vec3 0 0 1

instance Foldable Vec3 where
    foldMap f (Vec3 x y z) = mempty <> f x <> f y <> f z 
        where (<>) = mappend

data Action = Action {actVelocity :: Vec3 Double, jS :: Double} deriving Show

data Collide = Collide {colDist :: Double, colNormal :: Vec3 Double}
                    deriving (Show)

instance Eq Collide where
    Collide d0 _ == Collide d1 _ = d0 == d1

instance Ord Collide where
    Collide d0 _ <= Collide d1 _ = d0 <= d1

toForeignType (Action (Vec3 x y z) js) = newArray [x, y, z, js]

data Game = Game {ball :: Ball, currentTick :: Int, score :: Score}

data Score   = Score {myScore :: Int, enemyScore :: Int}

data Player         = Player {bot0 :: Bot, bot1 :: Bot}
newtype EnemyPlayer = EnemyPlayer Player
newtype IPlayer     = IPlayer Player
getMe   (IPlayer (Player me _  )) = me
getMate (IPlayer (Player _ mate)) = mate

data Bot     = Bot { botId :: Int,      botLoc :: Vec3 Double, botVel :: Vec3 Double, botRad :: Double, botTouch :: Touch}

data Touch  = Touch {isTouch :: Bool,    touchNormal :: Vec3 Double}
data Ball    = Ball {ballLoc :: Vec3 Double, ballVel :: Vec3 Double, balRadius :: Double}

class MoveAble a where
    velocity :: a -> Vec3 Double

instance MoveAble Bot where
    velocity (Bot _ _ v _ _) = v

instance MoveAble Ball where
    velocity (Ball _ v _) = v

instance MoveAble Action where
    velocity = actVelocity

instance MoveAble IPlayer where
    velocity = velocity . getMe

class MoveAble a => Character a where
    radius   :: a -> Double
    location :: a -> Vec3 Double

instance Character Bot where
    radius   (Bot _ _ _ r _) = r
    location (Bot _ l _ _ _) = l

instance Character Ball where
    radius   (Ball _ _ r) = r
    location (Ball l _ _) = l

instance Character IPlayer where
    radius   = radius   . getMe
    location = location . getMe
