module Foretold where

import Data.Maybe (isJust, fromJust)
import Types
import Constants
import Debug.Trace (traceShow, trace)
import Data.Foldable (foldl')
import Test

debugPredict game iAm enemy time dt = trace debugPrint ball' where
    ball'      = collideWithArena (Ball l v)
    Ball l v   = move dt ballNow
    --debugPrint = show (location ballNow) ++ " " ++ show l
    debugPrint = show (location ballNow) ++ " " ++ show l
    ballNow = ball$game

freeBallHit ball e = undefined 
botUpdateAction dt bot
    | notTouch || norm targetVelChange <= 0 = bot
    | otherwise = setVelocity v bot where
        notTouch = not . isTouch . botTouch $ bot
        v = velocity bot + clamp updateVelocity (norm targetVelChange)
        acceleration = robotAcceleration * max 0 (y tN)
        updateVelocity = (acceleration * dt) *| normalize targetVelChange
        targetVelChange = targetVel - velocity bot
        clampR = clamp (velocity . possAct $ bot) botMaxGroundSpeed
        tN = touchNormal . botTouch $ bot
        targetVel = clampR - ((tN `dot` clampR) *| tN)

botUpdateRadius bot = --traceShow (botId bot) $
    setRadiusChangeSpeed rcs . setRadius r $ bot where
    r = robotMinRadius + flRadius * jumpSpeed / robotMaxJumpSpeed
    flRadius = robotMaxRadius-robotMinRadius
    jumpSpeed = min (jS . possAct $ bot) robotMaxJumpSpeed --   ?
    rcs = jumpSpeed

collideBotToBot' f (prs, (b:bots)) = collideBotToBot' f (b':prs, bots') where
    (b', bots') = foldl' foldF (b,[]) (reverse bots)
    foldF (b'',bots'') b1 = (newB, b1':bots'') where
        (newB, b1') = f b'' b1
collideBotToBot' f (prs,[]) = (prs,[])
collideBotToBot'' f bots = reverse $ fst $ collideBotToBot' f ([],bots)
collideBotToBot = collideBotToBot'' collideEntities

collideBotToBall :: Ball -> [Bot] -> (Ball, [Bot])
collideBotToBall ball bots = collideBotToBall' collideEntities ball bots

collideBotToBall' f ball bots = --traceShow (location <$> bots)
    foldr foldF (ball,[]) bots where
        foldF b (ball',prs) = --traceShow (location <$> prs) 
            (ball'', collideWithArena b':prs) where
                (ball'',b') = --traceShow (location b) $
                    f ball' b

simplePredict (Prediction game iAm enemy) dt = --traceShow (possAct me) $ 
    Prediction
    (setBall game ball')
    (IPlayer $ Player me mate)
    (EnemyPlayer $ Player enBot0 enBot1)
      where
        ball'        = Ball l' v'
        mBall        = move dt ballNow
        Ball l' v'   = collideWithArena mcbBall
        botUpdates   = botUpdateRadius . move dt . botUpdateAction dt
        updatedBots  = botUpdates <$> oldBots
        collidedBots = seq updatedBots (collideBotToBot updatedBots)
        (mcbBall, collidedBallBots) = --traceShow (radiusChangeSpeed<$>collidedBots) $ 
            collideBotToBall mBall collidedBots
        [me, mate, enBot0, enBot1] = collidedBallBots
        oldBots = [getMe iAm, getMate iAm, getEnemyBot0 enemy, getEnemyBot1 enemy]
        ballNow = ball$game


collideEntities :: (PredictableCharacter x, PredictableCharacter y, Entity x, Entity y) => x -> y -> (x,y)
collideEntities a b = (a', b') where
    dLoc = location b - location a
    distAb = norm dLoc
    penetration = radius a + radius b - distAb
    kn = (1/mass a) + (1/mass b)
    k e = (1/mass e)/kn
    normal = normalize dLoc
    rcs :: (PredictableCharacter z) => z -> Double
    rcs = radiusChangeSpeed
    dVel   = (velocity b - velocity a) `dot` normal - rcs a - rcs b
    updateLoc sa e = setLocation (location e + ((sa*penetration * k e)*|normal)) e
    updateVel sa e = setVelocity (velocity e - ((sa * k e)*|impulse)) e
    update sa = updateLoc sa . updateVel sa
    impulse = ((1+meanHitE) * dVel) *| normal
    (a',b') | penetration <= 0 = (a, b)
            | dVel        >= 0 = (updateLoc (-1) a, updateLoc 1 b)
            | otherwise        = (update (-1) a, update 1 b)

isInField e = cz && cx where
    cz = abs z <= arenaDepth/2 + radius e
    cx = abs x <= arenaWidth/2 + radius e
    Vec3 x y z = location e

move'' e dt | isNumber (location e) && isNumber (radius e) && isNumber dt = move'' e dt
          | otherwise = trace (show (location e) ++ " " ++ show (radius e) ++ " " ++ show dt) undefined

move0 e dt = trace (show e ++ " " ++ show dt ++" "++ show (move0 e dt)) $ move0 e dt

move dt e | isInField e = move' e dt
          | otherwise   = e

move' e dt = setLocation l . setVelocity v $ e where
    l = Vec3 (x locE)   ly (z locE)
    v = Vec3 (x clampV) vy (z clampV)
    clampV = clamp (velocity e) maxEntitySpeed
    vy = (y clampV) - gravity * dt
    locE = location e + (dt *| clampV)
    ly = (y locE) - gravity*dt*dt/2

clamp v m | norm v > m = (m / norm v) *| v
          | otherwise  = v

f g (a,b) = (g a, g b)
collideWithArena e | not $ isInField e = e
                   | otherwise = debug e  $
    setVelocity eVel . setLocation ePos . setTouch touch $ e where
        Collide d n = collideArena $ location e
        touch = Touch (pCond && velCond) tN
        tN | pCond  && velCond = n
           | otherwise         = zero
        penetration = radius e - d
        pCond = penetration > 0
        ePos | pCond = location e + (penetration*|n)
             | True  = location e
        vel = --trace (show(radiusChangeSpeed e)++" "++show(velocity e `dot` n - radiusChangeSpeed e)) $ 
            velocity e `dot` n - radiusChangeSpeed e
        velCond = vel < 0
        eVel | pCond && velCond = --traceShow' $ 
            velocity e - (((1 + arenaE e) * vel) *| n)
             | otherwise        = velocity e
        debug e | isBot e = id--trace (show (location e) ++ " D " ++ show d ++ " R "++ show(radius e))
                | True    = id

collideWithArena' e radiusChangeSpeed
    | isNumber (location e) && isNumber (radius e) && isNumber radiusChangeSpeed = r
    | otherwise = trace (show (location e) ++ " " ++ show (radius e) ++ " " ++ show radiusChangeSpeed) r
    where
        r = collideWithArena' e radiusChangeSpeed

collidePlane' point planePoint planeNormal
    | isNumber d && isNumber n = r
    | otherwise  = trace (show point ++ " " ++ show planePoint ++ " " ++ show planeNormal ++ " " ++ show r) r
        where
            r@(Collide d n) = collidePlane' point planePoint planeNormal

collidePlane point planePoint planeNormal =
    Collide d n where
        d = (point - planePoint) `dot` planeNormal
        n = planeNormal

collideInnerSphere p center radius =
    Collide d n where
        d = radius - distance p center
        n = normalize $ center - p

collideOuterSphere p center radius =
    Collide d n where
        d = negate $ radius - distance p center
        n = (normalize . negate) $ center - p

collideArena' v | isNumber v = r
               | otherwise  = trace (show v) r
               where
                   r = collideArena' v

collideArena (Vec3 x y z) = collideArenaQ' where
    p' = Vec3 (abs x) y (abs z)
    collideArenaQ' = let Collide d n = collideArenaQ p' in
        Collide d (cor n)
    cor (Vec3 a b c) = Vec3 (signum x * a) b (signum z * c)
    cor' (Vec3 a b c) = Vec3 (signum a * x) b (signum c * z)

collideArenaQ p =
    fromJust . foldr1 min . filter isJust . (fmap ($ p)) $
        [ground, ceil, sideX, sideZ, corner,
        goalOuterCorner0, bottomCorner0, ceilingCorner0,
        goalOuterCorner1, bottomCorner1, ceilingCorner1,
        goalOuterCorner2, bottomCorner2, ceilingCorner2,
        bottomCorner3]
idMin l = (trace $ show $ snd $ foldr1 (\p0@(a,b) p1@(a0,b0) -> if a >= a0 then p1 else p0)  $ zip (fromJust <$> filter isJust l) [0..]) l

ground p  = Just $ collidePlane p     zero                  y1
ceil   p  = Just $ collidePlane p    (arenaHeight  *| y1) (-y1)
sideX  p  = Just $ collidePlane p    (arenaWidth/2 *| x1) (-x1)
sideZ  p | c = Just $ collidePlane p (arenaDepth/2 *| z1) (-z1)
         | True = Nothing where
             c = c0 || c1 || c2
             c0 = x p >= goalWidth/2 + goalSideRadius
             c1 = y p >= goalHeight  + goalSideRadius
             c2 = x t > 0 && y t > 0 && c3
             c3 = norm t >= goalTopRadius + goalSideRadius
             t  = xyPrj p - Vec3 tx ty 0
             tx = goalWidth/2 - goalTopRadius
             ty = goalHeight  - goalTopRadius
corner p | c = Just $ collideInnerSphere p (Vec3 x' (y p) z') cornerRadius
         | True = Nothing where
             x' = arenaWidth/2 - cornerRadius
             z' = arenaDepth/2 - cornerRadius
             c = c0 && c1
             c0 = x p > arenaWidth/2 - cornerRadius
             c1 = z p > arenaDepth/2 - cornerRadius

goalOuterCorner p = z p < arenaDepth/2 + goalSideRadius
goalOuterCorner0 p | cx && goalOuterCorner p  = 
    Just $ collideOuterSphere p centerX goalSideRadius where
        cx = x p < goalWidth/2 + goalSideRadius
        centerX = Vec3 x' (y p) z'
        x' = goalWidth/2 + goalSideRadius
        z' = goalDepth/2 + goalSideRadius
goalOuterCorner0 p | True = Nothing

goalOuterCorner1 p | cCeil && goalOuterCorner p = 
    Just $ collideOuterSphere p centerCeil goalSideRadius where
        cCeil = y p < goalHeight + goalSideRadius
        centerCeil = Vec3 (x p) y' z'
        y' = goalHeight/2 + goalSideRadius
        z' = goalDepth /2 + goalSideRadius
goalOuterCorner1 p | True = Nothing

goalOuterCorner2 p | cTopCorner && goalOuterCorner p = 
    Just $ collideOuterSphere p centerTopCorner goalSideRadius where
        centerTopCorner = Vec3 (x o') (y o') z'
        cTopCorner = x t > 0 && y t > 0
        t = xyPrj p - o
        o = Vec3 x'' y'' 0
        x'' = goalWidth/2 - goalTopRadius
        y'' = goalHeight  - goalTopRadius
        o' = o + ((goalTopRadius+goalSideRadius) *| normalize t)
        z' = goalDepth/2 + goalSideRadius
goalOuterCorner2 p | True = Nothing

bottomCorner p = y p < bottomRadius
bottomCorner0 p | cx && bottomCorner p = 
    Just $ collideInnerSphere p centerX bottomRadius where
        centerX = Vec3 x' bottomRadius (z p)
        x' = arenaWidth/2 - bottomRadius
        cx = x p > arenaWidth/2 - bottomRadius
bottomCorner0 p | True = Nothing
bottomCorner1 p | cz && bottomCorner p = 
    Just $ collideInnerSphere p centerZ bottomRadius where
        centerZ = Vec3 (x p) bottomRadius z'
        z' = arenaDepth/2 - bottomRadius
        cz = cz0 && cz1
        cz0 = z p >  arenaDepth/2 - bottomRadius
        cz1 = x p >= goalWidth /2 + goalSideRadius
bottomCorner1 p | True = Nothing
bottomCorner2 p | cGoalOuter && bottomCorner p = 
    Just $ collideInnerSphere p centerGoalOuter bottomRadius where
        centerGoalOuter = Vec3 (x o') bottomRadius (z o')
        z' = arenaDepth/2 - bottomRadius
        cGoalOuter = x t < 0 && y t < 0 && c'
        c' = norm t < goalSideRadius + bottomRadius
        t = xzPrj p - o
        o = Vec3 x'' 0 z''
        o' = o + ((goalSideRadius+bottomRadius) *| normalize t)
        x'' = goalWidth/2 + goalSideRadius
        z'' = arenaDepth  + goalSideRadius
bottomCorner2 p | True = Nothing
bottomCorner3 p | cCorner && bottomCorner p = 
    Just $ collideInnerSphere p centerGoalOuter bottomRadius where
        cCorner = c0 && c1 && c2
        c0 = x p > arenaWidth/2 - cornerRadius 
        c1 = z p > arenaDepth/2 - cornerRadius 
        o = Vec3 x'' 0 z''
        x'' = arenaWidth/2 - cornerRadius
        z'' = arenaDepth/2 - cornerRadius
        t = xzPrj p - o
        dist = norm t
        c2 = dist > cornerRadius - bottomRadius
        t' = (1/dist) *| t
        o' = o + ((cornerRadius-bottomRadius) *| t')
        centerGoalOuter = Vec3 (x o') bottomRadius (z o')
bottomCorner3 p | True = Nothing

ceilingCorner p = y p > arenaHeight - topRadius
ceilingCorner0 p | cx && ceilingCorner p =
    Just $ collideInnerSphere p centerX topRadius where
    cx = x p > arenaWidth/2 - topRadius
    centerX = Vec3 x' y' (z p)
    x' = arenaWidth/2 - topRadius
    y' = arenaHeight  - topRadius
ceilingCorner0 p | True = Nothing
ceilingCorner1 p | cz && ceilingCorner p =
    Just $ collideInnerSphere p centerZ topRadius where
    cz = z p > arenaDepth/2 - topRadius
    centerZ = Vec3 (x p) y' z'
    y' = arenaHeight   - topRadius
    z' = arenaDepth/2  - topRadius
ceilingCorner1 p | True = Nothing
ceilingCorner2 p | cCorner && ceilingCorner p =
    Just $ collideInnerSphere p centerCorner topRadius where
    cCorner = c0 && c1 && c2
    c0 = x p > arenaWidth/2 - cornerRadius
    c1 = z p > arenaDepth/2 - cornerRadius
    c2 = norm t > cornerRadius - topRadius
    o = Vec3 x'' 0 z''
    x'' = arenaWidth/2 - cornerRadius
    z'' = arenaDepth/2 - cornerRadius
    t = xzPrj p - o
    t' = normalize t
    o' = o + ((cornerRadius-topRadius) *| t')
    centerCorner = Vec3 (x o') y' (z o')
    y' = arenaHeight - topRadius
ceilingCorner2 p | True = Nothing
