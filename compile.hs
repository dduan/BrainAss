module Main where

import System.IO (hPutStrLn, stderr)
import System.Environment
import System.Exit

compileHeader :: [[String]] -> [String]
compileHeader [[getP, get], [putP, put]] =
    ["(module"
    ,"(func $getc (import \"" ++ getP ++ "\" \"" ++ get ++ "\") (result i32))"
    ,"(func $putc (import \"" ++ putP ++ "\" \"" ++ put ++ "\") (param i32))"
    ,"(memory 1)"
    ,"(global $p (mut i32) (i32.const 0))"
    ,"(func $main"
    ]
compileHeader _ = error "expecting get/put and their packages"

compileFooter :: String -> [String]
compileFooter name =
    [ ")"
    , "(export \"" ++ name ++ "\" (func $main))"
    , ")"
    ]

genInstr :: Char -> [String]
genInstr '<' =
    [ "get_global $p"
    , "i32.const -1"
    , "i32.add"
    , "set_global $p"
    ]
genInstr '>' =
    [ "get_global $p"
    , "i32.const 1"
    , "i32.add"
    , "set_global $p"
    ]
genInstr '+' =
    [ "get_global $p"
    , "get_global $p"
    , "i32.load8_u"
    , "i32.const 1"
    , "i32.add"
    , "i32.store8"
    ]
genInstr '-' =
    [ "get_global $p"
    , "get_global $p"
    , "i32.load8_u"
    , "i32.const -1"
    , "i32.add"
    , "i32.store8"
    ]
genInstr '[' =
    [ "(block"
    , "(loop"
    , "get_global $p"
    , "i32.load8_u"
    , "i32.const 0"
    , "i32.eq"
    , "br_if 1"
    ]
genInstr ']' =
    [ "br 0"
    , ")"
    , ")"
    ]
genInstr '.' =
    [ "get_global $p"
    , "i32.load8_u"
    , "call $putc"
    ]
genInstr ',' =
    [ "get_global $p"
    , "call $getc"
    , "i32.store8"
    ]
genInstr _ = []

compileBody :: String -> [String]
compileBody = concatMap genInstr

compile :: [[String]] -> String -> String
compile imports source = unlines $ concat
                       [ compileHeader (take 2 imports)
                       , compileBody   source
                       , compileFooter (head (imports !! 2))
                       ]

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

parseImports :: [String] -> [[String]]
parseImports [input, output, name]
    | length splitInput == 2 && length splitOutput == 2 = [splitInput, splitOutput, [name]]
    | otherwise                                         = []
    where splitInput = wordsWhen (=='.') input
          splitOutput = wordsWhen (=='.') output
parseImports _ = []

validateArgs :: [String] -> IO [[String]]
validateArgs args
    | not $ null result = return result
    | otherwise         = usage >> exitSuccess
    where result = parseImports args


usage :: IO ()
usage = hPutStrLn stderr $ unlines
      [ "Usage: compile IMPORTED_INPUT IMPORTED_OUTPUT EXPORT                  "
      , "where                                                                 "
      , "  IMPORTED_INPUT  is imported Javascript function for Brainfuck's ,   "
      , "  IMPORTED_OUTPUT is imported Javascript function for Brainfuck's .   "
      , "  EXPORT is the exported Javascript function from the WASM module     "
      , "                                                                      "
      , "Brainfuck source is read fro STDIN and text wasm is written to STDOUT "
      , "(similar to `cat`)                                                    "
      , "                                                                      "
      , "Example:                                                              "
      , "  cat source.bf | compile bf.getchar bf.putchar bf > main.wat         "
      , "  makes wasm module import bf.getchar as \",\" and bf.putchar as \".\""
      , "  and the wasm module exports a function named bf."
      ]

main :: IO ()
main = getArgs >>= validateArgs >>= \x -> interact $ compile x
