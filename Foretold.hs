module Foretold where

import Data.Maybe (isJust, fromJust)
import Types
import Constants
import Debug.Trace (traceShow, trace)

debugPredict game iAm enemy time dt = trace debugPrint ball' where
    ball'      = Ball l v
    (l,v)      = move ballNow dt
    --debugPrint = show (location ballNow) ++ " " ++ show l
    debugPrint = show (location ballNow) ++ " " ++ show l
    ballNow = ball$game

predict game iAm enemy time dt = trace debugPrint ball' where
    ball'      = Ball l v
    (l,v)      = move ballNow dt
    debugPrint = show (location ballNow) ++ " " ++ show l
    ballNow = ball$game

move e dt = (Vec3 (x locE)   ly (z locE), 
             Vec3 (x clampV) vy (z clampV)) where
    clampV = clamp (velocity e) maxEntitySpeed
    vy = (y clampV) - gravity * dt
    locE = location e + (dt *| clampV)
    ly = (y locE) - gravity*dt*dt/2
    
clamp v m | norm v > m = (m / norm v) *| v
          | otherwise  = v

collideWithArena e radiusChangeSpeed = (ePos, eVel) where
    Collide d n = collideArena $ location e
    penetration = radius e - d
    ePos | penetration > 0 = location e + (penetration*|n)
         | otherwise       = location e
    vel = velocity e `dot` n - radiusChangeSpeed
    eVel | penetration > 0 && vel < 0 = velocity e - (((1 + arenaE e) *| velocity e) * n)
         | otherwise                  = velocity e

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

collideArena (Vec3 x y z) = collideArenaQ' where
    p' = Vec3 (abs x) y (abs z)
    collideArenaQ' = let Collide d n = collideArenaQ p' in
        Collide d (cor n)
    cor (Vec3 a b c) = Vec3 (signum a * x) b (signum c * z)

collideArenaQ p = 
    fromJust . foldr1 min . filter isJust . (fmap ($ p)) $ 
        [ground, ceil, sideX, sideZ, corner, 
        goalOuterCorner0, bottomCorner0, ceilingCorner0,
        goalOuterCorner1, bottomCorner1, ceilingCorner1,
        goalOuterCorner2, bottomCorner2, ceilingCorner2,
        bottomCorner3]

ground p  = Just $ collidePlane p zero                   y1
ceil   p  = Just $ collidePlane p (arenaHeight  *| y1) (-y1)
sideX  p  = Just $ collidePlane p (arenaWidth/2 *| x1) (-x1)
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


