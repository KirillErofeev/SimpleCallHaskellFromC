{-# language NoMonomorphismRestriction #-}
{-#language FlexibleContexts #-}
module Algebra where

import Types
import Foretold

import Debug.Trace (traceShow, trace)
--traceShow = flip const

act :: Game -> IPlayer -> EnemyPlayer -> Score -> Action Double
act game iAm enemy score = r where
    r = f (predict game iAm enemy (1/60) (1/60)) zeroAct
    f (Ball _ _) a = a

zeroAct = zeroAction
zeroAction = Action (Vec3 0 0 0) 0.0

--isIAmCloserToBall game iAm
--     | myDist < mateDist = False
--     | otherwise         = True
--        where
--           bl = location . ball $ game
--           myDist   = distance (location iAm) bl
--           mateDist = distance (location (getMate iAm)) bl

isIAmCloserToBall game iAm
    | (botId . getMe) iAm > (botId . getMate) iAm = True
    | not $ isNotAutogoal (getMate iAm) game      = True
    | otherwise                                   = False
        where
            bl = (location . ball $ game)

goTo iAm point = Action v 0 where
    v = 1e3 *| xzPrj (point - location iAm)

hitBall game iAm = condHitBall game iAm

isNotAutogoal p game = z (bl - location p) >= (-1) where
    bl = (location . ball $ game)

condHitBall game iAm = action where
    action | isIAmCloserToBall game iAm = Action vOff jumpOff
           | otherwise                  = Action vDef jumpDef
    bl = (location . ball $ game)
    distanceToBall = distance bl (location iAm)
    isNotAutogoal = z (bl - location iAm) >= (-1)
    vDef  = velocity $ goTo iAm defPs
    defPs = (0.5*|(bl - Vec3 0 0 (-30))) + Vec3 0 0 (-30)

    vOff       | distanceToBall > 6  = velocity $ goTo iAm bl
               | isNotAutogoal       = velocity $ goTo iAm bl
               | otherwise           = velocity $ goTo iAm defPs

    jumpDef | distanceToBall < 5 && z bl < 2 && isNotAutogoal = 100
            | distanceToBall < 4             && isNotAutogoal = 100
            | otherwise                                       = 0
    jumpOff | distanceToBall < 4.5 && z bl < 2 && isNotAutogoal = 100
            | distanceToBall < 4               && isNotAutogoal = 100
            | otherwise                                         = 0

