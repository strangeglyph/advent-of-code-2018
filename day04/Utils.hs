module Utils where

odds :: [a] -> [a]
odds []     = []
odds (x:xs) = x : evens xs

evens :: [a] -> [a]
evens []    = []
evens xs    = odds $ tail xs
