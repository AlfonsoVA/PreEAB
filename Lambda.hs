module Lambda where

type (\x y -> x) = True
type (\x y -> y) = False
type (\z -> z) False True = (||)
type (\x y -> xy) False = (&&)
