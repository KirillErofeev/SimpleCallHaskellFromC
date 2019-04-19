{-# LANGUAGE NoMonomorphismRestriction#-}
module Kickoff where

import Types
import Debug.Trace

act :: Game -> IPlayer -> EnemyPlayer -> Score -> IO Double -> Move Double
act game (IPlayer (Player me mate)) (EnemyPlayer (Player e0 e1)) score stored 
  | (currentTick game == 0) = traceShow ds $ Move meAct mateAct 
  | otherwise = Move meAct mateAct where
      (meAct, mateAct) | isBumper' me = (kickOffBumpAct me, scoredAct mate)
                       | otherwise = (scoredAct me, kickOffBumpAct mate)
      isBumper  bot | (z (velocity e0) < 0) = isE0MeSameSide
                                            -- enemyGoalkeeper = e1
                    | otherwise = not isE0MeSameSide
                                -- enemyGoalkeeper = e0
      scoredAct me | isSecondStage game = hitTheFuckingBall me
                   | isScorerUp me = goTo (Vec3 (-28) 0 (-8)) me
                   | otherwise = goTo (Vec3 28 0 (-8)) me -- ндо написать кто goTo а не только куда     здесь соит функция, поэтому он думает что выше тоже должна быть функция
      kickOffBumpAct me | isSecondStage game = goTo (location enemyGoalkeeper) me
                        | otherwise = flip goTo (ball game) me
      isSecondStage game  = distance (location (ball game)) zero > 5
      isE0MeSameSide = x (location e0) * x (location me) > 0
      isScorerUp scorer = x (location scorer) > 0
      isBumper' b = isBumper b
      ds :: [Double]
      ds = dist (ball $ game) <$> [me, mate, e0, e1]