module Main where

import Data.List

main :: IO ()
main = interact gen

gen :: String -> String
gen s = unlines $ fork printer (deconstruct s)

printer :: [String]
printer =
  [ concat (replicate 7 "<><><> ")
  , "[]>[.>]"
  ]

fork :: [String] -> [String] -> [String]
fork a b =
     ["&+-[+-"]
  ++ map indent a
  ++ ["]["]
  ++ map indent b
  ++ ["]"]

nestFork :: [String] -> [String]
nestFork [] = []
nestFork [prog] = [prog]
nestFork progs =
  let (a, b) = splitAt (length progs `div` 2) progs
  in
  fork (nestFork a) (nestFork b)

indent = ("  " ++)

deconstruct :: String -> [String]
deconstruct s =
    nestFork
  $ map ((++ ">") . concat)
  $ transpose
  $ map deconstructChar s

deconstructChar :: Char -> [String]
deconstructChar c =
  flip map [0..127] $ \n ->
    if n < fromEnum c then ">+" else "> "
