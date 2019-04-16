module EnemyModel where

ballChaseJAct game iAm = Action v j where
    v = 1000 *| (bl - location iAm)
    bl = location . ball $ game
    j | distance bl (location iAm) < 3.5 = 30
      | otherwise                        = 0
