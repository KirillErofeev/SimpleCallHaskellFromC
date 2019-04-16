{-#language MultiParamTypeClasses #-}
{-#language FlexibleInstances #-}

module Types where 

import Foreign.Marshal (newArray)
import Foreign.Ptr (Ptr(..))
import Data.Semigroup (Semigroup, (<>))
import Data.Monoid (Sum, All, getAny, All(..))
import Debug.Trace (trace, traceShow)
import Data.Foldable (toList)

import Constants

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

data Vec3 a = Vec3 {x :: !a, y :: !a, z :: !a} 
    deriving (Eq, Ord)

instance Show a => Show (Vec3 a) where
    show (Vec3 x y z) = show x ++ " " ++ show y ++ " " ++ show z

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

x1 :: Num a => Vec3 a
y1 :: Num a => Vec3 a
z1 :: Num a => Vec3 a

x1   = Vec3 1 0 0
y1   = Vec3 0 1 0
z1   = Vec3 0 0 1

instance Foldable Vec3 where
    foldMap f (Vec3 x y z) = mempty <> f x <> f y <> f z 
        where (<>) = mappend

data Action a = Action {actVelocity :: !(Vec3 a), jS :: !a} 
    deriving (Show, Eq)

goTo iAm point = Action v 0 where
    v = 1e3 *| xzPrj (point - location iAm)

actFromX x = Action (Vec3 x 0 0) 0 
actSetZ (Action v j) z = Action (v {z=z}) j
actSetJ a j = a {jS=j}

zeroAct = (zeroAction, [])
zeroAction = Action (Vec3 0 0 0) 0.0
oneAction  = Action (Vec3 1 4 8) 8.0

instance Foldable Action where
    foldr f m (Action v jump) = foldr f (jump `f` m) v


data Collide = Collide {colDist :: !Double, colNormal :: !(Vec3 Double)}
                    deriving (Show)

data Move a = Move {myAction :: !(Action a), mateAction :: !(Action a)} 
    deriving (Show, Eq)

moveToIAm (Move my mate) (IPlayer (Player b0 b1)) = 
    IPlayer $ Player (b0 {possAct=my}) (b1 {possAct=mate}) 

setEnemyAct a (EnemyPlayer (Player b0 b1)) = 
    EnemyPlayer $ Player (b0 {possAct=a}) (b1 {possAct=a}) 

data Answer a = Answer {getMove :: !(Move a), getStored :: !([a])}

instance ForeignType (Answer Double) where
    toForeignType (Answer m s) = toForeignType $ toList m ++ toList s

instance Foldable Move where
    foldr f ini (Move a0 a1) = foldr f (foldr f ini a1) a0

instance ForeignType (Move Double) where
    toForeignType (Move a0 a1) = toForeignType $ toList a0 ++ toList a1

class Finite a where
    isNumber :: a -> Bool

instance Finite Double where
    isNumber x = not $ isNaN x || isInfinite x

instance Finite Ball where
    isNumber (Ball l v) = isNumber l && isNumber v

instance Finite a => Finite (Vec3 a) where
    isNumber v = getAll $ foldMap (All . isNumber) v 

instance Eq Collide where
    Collide d0 _ == Collide d1 _ = d0 == d1

instance Ord Collide where
    Collide d0 _ <= Collide d1 _ = d0 <= d1

class ForeignType a where
    toForeignType :: a -> IO (Ptr Double)

instance ForeignType (Action Double) where
    toForeignType (Action (Vec3 x y z) js) = newArray [x, y, z, js]
    
instance ForeignType [Double] where
    toForeignType = newArray

instance (Foldable a, Foldable b) => ForeignType (a Double,b Double) where
    toForeignType (a,b) = toForeignType $ foldr (:) [] a ++ foldr (:) [] b

--instance (Foldable a, Foldable b) => ForeignType (a Double,b Double) where
--    toForeignType (a,b) = toForeignType $ foldr (:) [] a ++ foldr (:) [] b


data Game = Game {ball :: !(Ball), currentTick :: !Int, score :: !Score}

setBall (Game b ct score) ball = Game ball ct score
setBall' (Game b ct score) ball | isNumber ball = Game ball ct score
                               | otherwise = trace (show ball) undefined

data Score   = Score {myScore :: !Int, enemyScore :: !Int}

data Player         = Player {bot0 :: !Bot, bot1 :: !Bot}
newtype EnemyPlayer = EnemyPlayer {getEnemyPlayer :: Player}
getEnemyBot0 = bot0 . getEnemyPlayer
getEnemyBot1 = bot1 . getEnemyPlayer
newtype IPlayer     = IPlayer Player
getMe   (IPlayer (Player me _  )) = me
getMate (IPlayer (Player _ mate)) = mate
setMyAct a' (IPlayer (Player b b0)) = IPlayer (Player (b {possAct = a'}) b0)

data Bot = Bot {botId :: !Int,      
                botLoc :: !(Vec3 Double), 
                botVel :: !(Vec3 Double), 
                botRad :: !Double, 
                botTouch :: !Touch, 
                botRadiusChangeSpeed :: !Double,
                possAct :: !(Action Double)}

data Touch  = Touch {isTouch :: !Bool, touchNormal :: !(Vec3 Double)}
data Ball    = Ball {ballLoc :: !(Vec3 Double), ballVel :: !(Vec3 Double)} 
    deriving (Eq)

instance Show Ball where
    show (Ball l v) = "B: loc:" ++ show l ++ " vel:" ++ show v

mapBall f (Ball l v) = Ball (f <$> l) (f <$> v)


class Entity a where
    arenaE :: a -> Double
    mass   :: a -> Double

instance Entity Ball where
    arenaE b = 0.7 
    mass   b = 1.0

instance Entity Bot where
    arenaE b = 0.0 
    mass   b = 2.0

class MoveAble a where
    velocity :: a -> Vec3 Double

instance MoveAble Bot where
    velocity (Bot _ _ v _ _ _ _) = v

instance MoveAble Ball where
    velocity (Ball _ v) = v

instance MoveAble (Action Double) where
    velocity = actVelocity

instance MoveAble IPlayer where
    velocity = velocity . getMe

class MoveAble a => Character a where
    radius   :: a -> Double
    location :: a -> Vec3 Double

instance Character Bot where
    radius   (Bot _ _ _ r _ _ _) = r
    location (Bot _ l _ _ _ _ _) = l

instance Character Ball where
    radius   (Ball _ _) = ballRadius
    location (Ball l _) = l

instance Character IPlayer where
    radius   = radius   . getMe
    location = location . getMe

class Character a => PredictableCharacter a where
    setVelocity :: Vec3 Double -> a -> a
    setLocation :: Vec3 Double -> a -> a
    setRadius   :: Double -> a -> a
    setRadiusChangeSpeed :: Double -> a -> a
    setTouch :: Touch -> a -> a
    radiusChangeSpeed :: a -> Double 
    isBot :: a -> Bool

instance PredictableCharacter Bot where
    setVelocity v' (Bot a b v c d rcs act) = Bot a b v' c d rcs act 
    setLocation l' (Bot a l v c d rcs act) = Bot a l' v c d rcs act
    setRadius   r' (Bot a l v r d rcs act) = Bot a l v r' d rcs act
    setTouch   t' (Bot a l v r t rcs act)  = Bot a l v r t' rcs act
    setRadiusChangeSpeed rcs' b = --traceShow rcs' $ 
        b {botRadiusChangeSpeed = rcs'}
    radiusChangeSpeed (Bot _ _ _ _ _ rcs _) = rcs
    isBot _ = True

instance PredictableCharacter Ball where
    setVelocity v' (Ball l v) = Ball l  v'
    setLocation l' (Ball l v) = Ball l' v
    setRadius   r' (Ball l v) = Ball l  v
    setRadiusChangeSpeed = flip const
    setTouch             = flip const 
    radiusChangeSpeed b  = 0.0
    isBot _ = False

traceShow'  x = uncurry traceShow $ (\a->(a,a)) x
checkNumberTrace x | not . isNumber $ x = traceShow' x 
                   | otherwise          = x

data Prediction = Prediction {predGame :: !Game, 
    predIAm :: !IPlayer, predEnemy :: !EnemyPlayer}

predBallLoc = location . ball . predGame  
predBallVel = velocity . ball . predGame  
predMyLoc   = location . predIAm
predMyVel   = velocity . predIAm

data Proposition = Proposition {proposeMe     :: !(Action Double), 
                                proposeMate   :: !(Action Double),
                                proposeEnemy0 :: !(Action Double),
                                proposeEnemy1 :: !(Action Double)}

predBall = ball . predGame  

class ShowStatic a where
    show' :: a -> String

instance ShowStatic Game where
    show' (Game ball _ _) = show' ball

instance ShowStatic Ball where
    show' (Ball l v) = "Ball: " ++ show l

instance ShowStatic IPlayer where
    show' i = "I: " ++ show (location i) ++ " M: " ++ show (location . getMate $ i)  

instance ShowStatic EnemyPlayer where
    show' (EnemyPlayer e) = "E: " ++ show (location . bot0 $ e) ++ " | " ++ show (location . bot1 $ e)  

instance (ShowStatic a, ShowStatic b, ShowStatic c) => ShowStatic (a,b,c) where
    show' (a,b,c) = show' a ++ " " ++ show' b ++ " " ++ show' c

class Zero a where
    zero :: a

instance Zero (Vec3 Double) where
    zero = Vec3 0 0 0

instance Zero Ball where
    zero = Ball zero zero

instance Zero Player where
    zero = Player zero zero

instance Zero IPlayer where
    zero = IPlayer $ Player zero zero

instance Zero EnemyPlayer where
    zero = EnemyPlayer $ Player zero zero

instance Zero Double where
    zero = 0.0

instance Zero Int where
    zero = 0

instance Zero (Action Double) where
    zero = Action zero zero

instance Zero (Move Double) where
    zero = Move zero zero

instance Zero Bot where
    zero = Bot zero zero zero zero zero zero zero

instance Zero Touch where
    zero = Touch False zero

instance Zero Score where
    zero = Score 0 0

instance Zero Game where
    zero = Game zero zero zero

instance Zero Prediction where
    zero = Prediction zero zero zero
