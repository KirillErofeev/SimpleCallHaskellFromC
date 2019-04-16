module Test where

data T a = T a [a] deriving (Show)

add (T b l) (T b' l') = (T b (b':l), T b' (b:l'))
ad (T b l) (T b' l') = T b' (b:l')
--add _ _ = undefined

l = [T 0 [], T 1 [], T 2 [], T 3 []]
t = T 4 []

collideBotToBall''' ball bots = --traceShow (location <$> bots)
    foldr foldF (ball,[]) bots where
        foldF b (ball',prs) = --traceShow (location <$> prs) 
            (ball'', ad (T (-1) []) b':prs) where
                (ball'',b') = --traceShow (location b) $
                    add ball' b

