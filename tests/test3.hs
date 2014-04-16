module Main where
import Data.List

main = print . group . sort $ [3,2,1]
