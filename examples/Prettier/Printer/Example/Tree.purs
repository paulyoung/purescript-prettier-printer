module Prettier.Printer.Example.Tree where

import Prelude

import Data.List (List(Cons), (:))
import Data.List as List
import Data.String as String
import Effect (Effect)
import Effect.Console (log)
import Prettier.Printer (DOC, bracket, group, line, nest, nil, pretty, text)

-- Tree example

data Tree = Node String (List Tree)

showTree :: Tree -> DOC
showTree (Node s ts) = group (text s <> nest (String.length s) (showBracket ts))

showBracket :: List Tree -> DOC
showBracket List.Nil = nil
showBracket ts = text "[" <> nest 1 (showTrees ts) <> text "]"

showTrees :: List Tree -> DOC
showTrees List.Nil = nil
showTrees (Cons t List.Nil) = showTree t
showTrees (Cons t ts) = showTree t <> text "," <> line <> showTrees ts

showTree' :: Tree -> DOC
showTree' (Node s ts) = text s <> showBracket' ts

showBracket' :: List Tree -> DOC
showBracket' List.Nil = nil
showBracket' ts = bracket "[" (showTrees' ts) "]"

showTrees' :: List Tree -> DOC
showTrees' List.Nil = nil
showTrees' (Cons t List.Nil) = showTree t
showTrees' (Cons t ts) = showTree t <> text "," <> line <> showTrees ts

tree :: Tree
tree =
  Node "aaa"
    ( Node "bbbbb"
      ( Node "ccc" List.Nil
      : Node "dd" List.Nil
      : List.Nil
      )
    : Node "eee" List.Nil
    : Node "ffff"
      ( Node "gg" List.Nil
      : Node "hhh" List.Nil
      : Node "ii" List.Nil
      : List.Nil
      )
    : List.Nil
    )

testtree :: Int -> Effect Unit
testtree w = log $ pretty w (showTree tree)

testtree' :: Int -> Effect Unit
testtree' w = log $ pretty w (showTree' tree)
