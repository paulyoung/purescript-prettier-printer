module Prettier.Printer
  ( DOC
  , Doc
  -- , be
  , below
  , beside
  , besideOrBelow
  -- , best
  -- , better
  , bracket
  -- , copy
  , fill
  , fillwords
  -- , fits
  -- , flatten
  , folddoc
  , group
  , layout
  , line
  , nest
  , nil
  , pretty
  , spread
  , stack
  , text
  , words
  -- , (:<>)
  -- , (:<|>)
  , (<+>)
  , (</>)
  , (<+/>)
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (intercalate)
import Data.List (List(Cons), (:))
import Data.List as List
import Data.Monoid (class Monoid)
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))

data DOC
  = NIL
  | APPEND DOC DOC
  | NEST Int DOC
  | TEXT String
  | LINE
  | UNION DOC DOC

infixr 5 UNION as :<|>
infixr 6 APPEND as :<>

instance semigroupDOC :: Semigroup DOC where
  append = APPEND

instance monoidDOC :: Monoid DOC where
  mempty = NIL

data Doc
  = Nil
  | Text String Doc
  | Line Int Doc

nil :: DOC
nil = NIL

nest :: Int -> DOC -> DOC
nest = NEST

text :: String -> DOC
text = TEXT

line :: DOC
line = LINE

group :: DOC -> DOC
group x = flatten x :<|> x

flatten :: DOC -> DOC
flatten NIL = NIL
flatten (APPEND x y) = flatten x :<> flatten y
flatten (NEST i x) = NEST i $ flatten x
flatten t@(TEXT _) = t
flatten LINE = TEXT " "
flatten (x :<|> y) = flatten x

layout :: Doc -> String
layout Nil = ""
layout (Text s x) = s <> layout x
layout (Line i x) = "\n" <> copy i " " <> layout x

copy :: Int -> String -> String
copy i x = intercalate "" $ Array.replicate i x

best :: Int -> Int -> DOC -> Doc
best w k x = be w k $ List.singleton (Tuple 0 x)

be :: Int -> Int -> List (Tuple Int DOC) -> Doc
be w k List.Nil = Nil
be w k (Cons (Tuple i NIL) z) = be w k z
be w k (Cons (Tuple i (APPEND x y)) z) = be w k $ (Tuple i x) : (Tuple i y) : z
be w k (Cons (Tuple i (NEST j x)) z) = be w k  $ (Tuple (i + j) x) : z
be w k (Cons (Tuple i (TEXT s)) z) = Text s $ be w (k + String.length s) z
be w k (Cons (Tuple i LINE) z) = Line i $ be w i z
be w k (Cons (Tuple i (UNION x y)) z) =
  let x' = be w k $ (Tuple i x) : z
  in if fits (w - k) x' then x' else be w k $ (Tuple i y) : z

fits :: Int -> Doc -> Boolean
fits w x | w < 0 = false
fits w Nil = true
fits w (Text s x) = fits (w - String.length s) x
fits w (Line i x) = true

pretty :: Int -> DOC -> String
pretty w x = layout $ best w 0 x

-- Utility functions

beside :: DOC -> DOC -> DOC
beside x y = x <> text " " <> y

infixr 6 beside as <+>

below :: DOC -> DOC -> DOC
below x y = x <> line <> y

infixr 5 below as </>

folddoc :: (DOC -> DOC -> DOC) -> List DOC -> DOC
folddoc f List.Nil = nil
folddoc f (Cons x List.Nil) = x
folddoc f (Cons x xs) = f x $ folddoc f xs

spread :: List DOC -> DOC
spread = folddoc (<+>)

stack :: List DOC -> DOC
stack = folddoc (</>)

bracket :: String -> DOC -> String -> DOC
bracket l x r = group $ text l <> nest 2 (line <> x) <> line <> text r

besideOrBelow :: DOC -> DOC -> DOC
besideOrBelow x y = x <> (text " " :<|> line) <> y

infixr 6 besideOrBelow as <+/>

words :: String -> List String
words = List.fromFoldable <<< String.split (Pattern " ")

fillwords :: String -> DOC
fillwords = folddoc (<+/>) <<< map text <<< words

fill :: List DOC -> DOC
fill List.Nil = nil
fill (Cons x List.Nil) = x
fill (Cons x (Cons y zs)) =
  (flatten x <+> fill (flatten y : zs))
  :<|>
  (x </> fill (y : zs))
