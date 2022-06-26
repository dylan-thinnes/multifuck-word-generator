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
fork a b = ["&+-[+-"] ++ map i a ++ ["]["] ++ map i b ++ ["]"]

nestFork :: [String] -> [String]
nestFork [] = []
nestFork [prog] = [prog]
nestFork progs =
  let (a, b) = splitAt (length progs `div` 2) progs
  in
  fork (nestFork a) (nestFork b)

i = ("  " ++)

deconstruct :: String -> [String]
deconstruct s =
    nestFork
  $ map (++ ">")
  $ map concat
  $ transpose
  $ map char s

char :: Char -> [String]
char c =
  let i = fromEnum c
      f n = if n < i then ">+" else "> "
  in
  map f [0..127]
