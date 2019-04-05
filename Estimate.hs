module Estimate where

import Types
import Constants

estimate :: Game -> IPlayer -> EnemyPlayer -> Double

estimate game iAm enemy | z bl > arenaDepth/2 + ballRadius = maxPoints
                        | z bl < -arenaDepth/2 - ballRadius = minPoints
                     -- | -arenaDepth/2 < z bl < arenaDepth/2 = 
                        | otherwise = z bl * totalPoints / arenaDepth where
                            bl = location (ball game)
                            meLoc = location (getMe iAm)
                            mateLoc = location (getMate iAm)
                            -- bot1Loc = location (bot0 enemy)
                            -- bot2Loc = location (bot1 enemy)
                            -- fromdistance (Vec3 0 0 (-arenaDepth/2)) (bl)
                            totalPoints = 2000
                            minPoints = -1000
                            maxPoints = 1000

