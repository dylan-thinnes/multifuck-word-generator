module Main where

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
  $ flip map [0..127]
  $ \i -> concat (deconstructChar i <$> s) ++ ">"

deconstructChar :: Int -> Char -> String
deconstructChar target c
  | target < fromEnum c = ">+"
  | otherwise = "> "
