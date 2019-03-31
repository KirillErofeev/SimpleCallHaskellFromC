module Haskell where

import Types

import Foreign.Marshal (newArray)
import Foreign.Ptr (Ptr(..))

import Types
import Algebra (act)

foreign export ccall edwardHere :: IO ()
edwardHere = putStr $ "Edward Kmett is here too!" 


foreign export ccall haskellAct ::
  --  me.Id  -> me.is_teammate -> me.x   -> me.y   -> me.z
    Int -> Bool    -> Double -> Double -> Double ->
  --me.vel_x -> me.vel_y -> me.vel_z ->
    Double   -> Double   -> Double   ->
  --me.radius -> me.touch -> me.tnX -> me.tnY -> me.tnZ
    Double    -> Bool     -> Double -> Double -> Double ->
  --  me.Id  -> me.is_teammate -> me.x   -> me.y   -> me.z
    Int -> Bool    -> Double -> Double -> Double ->
  --me.vel_x -> me.vel_y -> me.vel_z ->
    Double   -> Double   -> Double   ->
  --me.radius -> me.touch -> me.tnX -> me.tnY -> me.tnZ
    Double    -> Bool     -> Double -> Double -> Double ->
  --  me.Id  -> me.is_teammate -> me.x   -> me.y   -> me.z
    Int -> Bool    -> Double -> Double -> Double ->
  --me.vel_x -> me.vel_y -> me.vel_z ->
    Double   -> Double   -> Double   ->
  --me.radius -> me.touch -> me.tnX -> me.tnY -> me.tnZ
    Double    -> Bool     -> Double -> Double -> Double ->
  --  me.Id  -> me.is_teammate -> me.x   -> me.y   -> me.z
    Int -> Bool    -> Double -> Double -> Double ->
  --me.vel_x -> me.vel_y -> me.vel_z ->
    Double   -> Double   -> Double   ->
  --me.radius -> me.touch -> me.tnX -> me.tnY -> me.tnZ
    Double    -> Bool     -> Double -> Double -> Double ->
  --me.x   -> me.y   -> me.z
    Double -> Double -> Double ->
  --me.vel_x -> me.vel_y -> me.vel_z -> me.radius
    Double   -> Double   -> Double   -> Double ->
  --current_tick
    Int -> Int -> Int ->
    IO (Ptr Double)

haskellAct
    meId meIsMate
    meX meY meZ meVelX meVelY meVelZ
    meRadius meTouch meTnX meTnY meTnZ
    mateId mateIsMate
    mateX mateY mateZ mateVelX mateVelY mateVelZ
    mateRadius mateTouch mateTnX mateTnY mateTnZ
    eBotId eBotIsMate
    eBotX eBotY eBotZ eBotVelX eBotVelY eBotVelZ
    eBotRadius eBotTouch eBotTnX eBotTnY eBotTnZ
    eBot0Id eBot0IsMate
    eBot0X eBot0Y eBot0Z eBot0VelX eBot0VelY eBot0VelZ
    eBot0Radius eBot0Touch eBot0TnX eBot0TnY eBot0TnZ
    ballX ballY ballZ ballVelX ballVelY ballVelZ ballRadius
    currentTick myScore enemyScore
        = toForeignType $ act game iAm enemy score where
            game        = Game ball currentTick score
            ball        = Ball ballLoc ballVel ballRadius
            ballLoc     = Vec3 ballX ballY ballZ
            ballVel     = Vec3 ballVelX ballVelY ballVelZ
            score       = Score myScore enemyScore
            iAm         = IPlayer     $ Player myBot0 myBot1
            enemy       = EnemyPlayer $ Player eBot0  eBot1
            meLoc       = Vec3 meX meY meZ
            meVel       = Vec3 meVelX meVelY meVelZ
            mateLoc     = Vec3 mateX mateY mateZ
            mateVel     = Vec3 mateVelX mateVelY mateVelZ
            myBot0      = Bot meId    meLoc    meVel    meRadius    meTouchN
            myBot1      = Bot mateId  mateLoc  mateVel  mateRadius  mateTouchN
            eBot0       = Bot eBotId  eBotLoc  eBotVel  eBotRadius  eBotTouchN
            eBot1       = Bot eBot0Id eBot0Loc eBot0Vel eBot0Radius eBot0TouchN
            meTouchN    = Touch meTouch    meTN
            mateTouchN  = Touch mateTouch  mateTN
            eBotTouchN  = Touch eBotTouch  eBotTN
            eBot0TouchN = Touch eBot0Touch eBot0TN
            meTN        = Vec3  meTnX meTnY meTnZ
            mateTN      = Vec3  mateTnX mateTnY mateTnZ
            eBotTN      = Vec3  eBotTnX eBotTnY eBotTnZ
            eBot0TN     = Vec3  eBot0TnX eBot0TnY eBot0TnZ
            eBotLoc     = Vec3 eBotX eBotY eBotZ
            eBotVel     = Vec3 eBotVelX eBotVelY eBotVelZ
            eBot0Loc    = Vec3 eBot0X eBot0Y eBot0Z
            eBot0Vel    = Vec3 eBot0VelX eBot0VelY eBot0VelZ

helloFromHaskell = 1111.0
