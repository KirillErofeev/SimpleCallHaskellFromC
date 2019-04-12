{-#language FlexibleInstances #-}
module Estimate where

import Types
import Constants
import Debug.Trace (trace)

estimate :: Game -> IPlayer -> EnemyPlayer -> Double

estimate game iAm enemy | z bl > arenaDepth/2 + ballRadius = maxPoints
                        | z bl < -arenaDepth/2 - ballRadius = minPoints
                        | z bl < 0 = z bl * totalPoints / arenaDepth + abs(x bl) * 3
                        | z bl > 0 = z bl * totalPoints / arenaDepth - abs(x bl) * 3
                        | otherwise = 0 where
                            bot0en = getEnemyBot0 enemy
                            bl = location (ball game)
                            meLoc = location (getMe iAm)
                            mateLoc = location (getMate iAm)
                            bot0Loc = location (bot0en)
                            -- bot1Loc = location (bot1 enemy)
                            -- fromdistance (Vec3 0 0 (-arenaDepth/2)) (bl)
                            totalPoints = 2000
                            minPoints = -1000
                            maxPoints = 1000


uncurry3 f (a,b,c) = f a b c
traceShowStatic f a = trace (show' a ++ " ESTIMATION: " ++ show (f a)) ()
test = traceShowStatic (uncurry3 estimate) <$> [(posToGame (Vec3 10 0 (-43)), zero, zero)
                         ,(posToGame (Vec3 (-30) 0 (-40)), zero, zero)
                         ,(posToGame (Vec3 0 0 (0)), zero, zero)
                         ,(posToGame (Vec3 0 0 (10)), zero, zero)
                         ,(posToGame (Vec3 30 0 (40)), zero, zero)
                          ]

ballToGame ball = Game ball 0 (Score 0 0)
posToBall l = Ball l zero
posToGame = ballToGame . posToBall

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

instance Zero Bot where
    zero = Bot zero zero zero zero zero zero zero

instance Zero Touch where
    zero = Touch False zero

