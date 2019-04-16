{-# language NoMonomorphismRestriction #-}
{-#language FlexibleContexts #-}
module Algebra where

import Types
import Foretold
import Prediction
import Constants
import Data.Foldable (toList)

import Debug.Trace (traceShow, trace)
--traceShow = flip const

act' :: Game -> IPlayer -> EnemyPlayer -> Score -> IO Double -> Answer Double
act' game iAm enemy score savedData = --trace ( "[" ++ show(location iAm)++"]")$ --trace ("PRED VEL " ++ show (myVel)) $ 
    Answer (Move myAct zero) stored where
        stored = (toList predLoc ++ toList predVel ++ toList myLoc)
        myAct = ballChaseJAct game iAm
        predLoc = predBallLoc p
        predVel = predBallVel p
        myLoc = predMyLoc p
        myVel = predMyVel p
        iAmAct = setMyAct myAct iAm
        p = predict (Prediction game iAmAct enemy) (1/60) (1/6000)
        --predVel = velocity $ trace ("BALL AFTER:" ++ show (newBall)) newBall
        game' = trace ("BALL BEFORE:" ++ show (ball game)) game
        celebrate = zeroAct

act :: Game -> IPlayer -> EnemyPlayer -> Score -> IO Double -> Answer Double
act game iAm enemy score savedData = --trace ( "[" ++ show(location iAm)++"]")$ --trace ("PRED VEL " ++ show (myVel)) $ 
    Answer (corMove game iAm move) stored where
        move = getBestMove game iAm enemy (ballChaseAct game iAm)
        stored = [0,0,0, 0,0,0, 0,0,0, 0,0,0]

corMove game iAm (Move (Action v0 j0) (Action v1 j1)) = Move (Action v0 j0') (Action v1 j1') where
    j0' | distance (bl) (location iAm) > 3.5 = 0
        | otherwise = j0
    j1' | distance (bl) (location . getMate $ iAm) > 3.5 = 0
        | otherwise = j1
    bl = location . ball $ game
jumpAction = Action zero 30
--isIAmCloserToBall game iAm
--     | myDist < mateDist = False
--     | otherwise         = True
--        where
--           bl = location . ball $ game
--           myDist   = distance (location iAm) bl
--           mateDist = distance (location (getMate iAm)) bl

ballChaseAct game iAm = Action v 0 where
    v = 1000 *| (bl - location iAm)
    bl = location . ball $ game

ballChaseJAct game iAm = Action v j where
    v = 1000 *| (bl - location iAm)
    bl = location . ball $ game
    j | distance bl (location iAm) < 3.5 = 30
      | otherwise                        = 0

isIAmCloserToBall game iAm
    | (botId . getMe) iAm > (botId . getMate) iAm = True
    | not $ isNotAutogoal (getMate iAm) game      = True
    | distance (location (getMate iAm)) bl - distance (location iAm) bl < 0 = True
    | distance (location iAm) bl < 6 = True
    | otherwise                                   = False
        where
            bl = (location . ball $ game)

hitBall game iAm = condHitBall game iAm

isNotAutogoal p game = z (bl - location p) >= (-1) where
    bl = (location . ball $ game)

condHitBall game iAm enemy = action where
    action | (botId . getMe) iAm > (botId . getMate) iAm = Action vOff 0
           | otherwise = Action vOff jumpOff
    bl = (location . ball $ game)
    predictBall :: Double -> Vec3 Double
    predictBall time = undefined
    distanceToBall = distance bl (location iAm)
    isNotAutogoal = z (bl - location iAm) >= (-1)

    vBc = velocity $ goTo iAm bl
    jc = jumpOff

    vDef  = velocity $ goTo iAm defPs
    defPs = (0.5*|(bl - Vec3 0 0 (-30))) + Vec3 0 0 (-30)

    vOff | distanceToBall > 10  = velocity $ goTo iAm (predictBall (1))
         | otherwise            = Vec3 0 0 0

    jumpDef | distanceToBall < 5 && z bl < 2 && isNotAutogoal = 0
            | distanceToBall < 4             && isNotAutogoal = 0
            | otherwise                                       = 0
    jumpOff | distanceToBall < 3.5 && z bl < 7 && isNotAutogoal = 100
            | otherwise                                         = 0

