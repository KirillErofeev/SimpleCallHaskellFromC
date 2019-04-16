{-#LANGUAGE NoMonomorphismRestriction#-}
module Prediction where

import Types
import Foretold
import Estimate
import Data.Foldable (foldl')

import Debug.Trace (traceShow, trace)

simplePredict' game iAm enemy dt
    | isBug = trace (show ballNow ++ show dt) undefined
    | otherwise = ball' where
        ball'      = Ball l' v'
        Ball l v   = move dt ballNow
        Ball l' v' = collideWithArena (Ball l v)
        --debugPrint = show (location ballNow) ++ " " ++ show l
        ballNow = ball$game
        isBug = isNumber ballNow && not (isNumber ball')

{-# NOINLINE [1] iterate' #-}
iterate' :: (a -> a) -> a -> [a]
iterate' f x =
    let x' = f x
    in x' `seq` (x : iterate' f x')

predict :: Prediction -> Double -> Double -> Prediction
predict p@(Prediction game iAm enemy) time dt = --traceShow (possAct . getMe $ iAm)
    snd . head . dropWhile ((<time) . fst) $
    iterate' predictIterate (0,p) where 
        predictIterate (t, prediction) = (t+dt, simplePredict prediction dt)

minVelGrid1d = [-1000.0,0.0,1000.0]
minJumpGrid = [0.0,30.0]
jumpGrid = [0.0,15.0,30.0]
gridZ grid act =  actSetZ act <$> grid
gridJ grid act =  actSetJ act <$> grid

minGrid :: Bot -> [Action Double]
minGrid bot | not $ (isTouch . botTouch) bot = [zero]
            | otherwise = do
    xa <- actFromX <$> minVelGrid1d
    za <- gridZ minVelGrid1d xa 
    gridJ minJumpGrid za

minGridMove gb0 gb1 = Move <$> gb0 <*> gb1

estimatePrd (Prediction game i e) = estimate game i e

deepEstimate :: Game -> IPlayer -> EnemyPlayer -> Action Double -> Move Double -> (Double, Prediction)
deepEstimate game iAm enemy eAc move = stepForesight game iAm enemy eAc move

stepForesight game iAm enemy eAc move@(Move act0 act1) = (estimatePrd p, p) where
    p = predict (Prediction game iAm' enemy') (3/60) (1/60)
    iAm'   = moveToIAm move iAm
    enemy' = setEnemyAct eAc enemy 

deepEstimate' :: Game -> IPlayer -> EnemyPlayer -> Action Double -> Move Double -> (Double, Move Double)
deepEstimate' game iAm enemy eAc move = (fst $ deepEstimate game iAm enemy eAc move, move)

getBestMove :: Game -> IPlayer -> EnemyPlayer -> Action Double -> Move Double
getBestMove game iAm enemy enemyAct = snd $ foldl' maxF (-100000, zero) $ 
    deepEstimate' game iAm enemy enemyAct <$> minGridMove (minGrid (getMe iAm)) (minGrid (getMate iAm)) where
        maxF (est, a) (est', a') | est > est' = (est,a)
                                 | otherwise  = (est',a')

goalExactness = 0
goalUL = Vec3 x y z where
    x = (goalWidth/2-ballRadius-goalExactness)
    y = (goalHeight-ballRadius-goalExactness)
    z = arenaWidth/2

goalUR = Vec3 x y z where
    x = (-goalWidth/2+ballRadius+goalExactness)
    y = (goalHeight-ballRadius-goalExactness)
    z = arenaWidth/2

goalCenter = Vec3 x y z where
    x = 0
    y = (goalHeight)
    z = arenaWidth/2

freeKick game executor goal time 
    | isReadyForKick = goToBall
