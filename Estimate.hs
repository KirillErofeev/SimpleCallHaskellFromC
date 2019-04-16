{-#language FlexibleInstances #-}
module Estimate where

import Types
import Constants
import Debug.Trace (trace)

maxPoints = 1000

estimate :: Game -> IPlayer -> EnemyPlayer -> Double
estimate' game iAm enemy | z bl > arenaDepth/2 + ballRadius = maxPoints
                         | z bl < -arenaDepth/2 - ballRadius = -maxPoints
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

estimate game iAm enemy = estimate' game iAm enemy + cor where
    cor = (80 - minBallDistance) + (80 - minDefDistance - minBallDistance)
    minBallDistance = 
        min (distance (location iAm) bl) (distance (location . getMate $ iAm) bl)
    bl  = location . ball $ game
    def = Vec3 0 0 (-40)
    minDefDistance =
        min (distance (location iAm) def) (distance (location . getMate $ iAm) def)

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
