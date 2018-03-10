module Prettier.Printer
  ( DOC
  , Doc
  -- , be
  , below
  , beside
  , besideOrBelow
  -- , best
  -- , better
  , bracket'
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

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array as Array
import Data.Foldable (intercalate)
import Data.List (List(Cons), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
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

newtype State = State
  { f :: Doc -> Doc
  , k :: Int
  , l :: List (Tuple Int DOC)
  -- Based on the generated document, should it keep the result or try a new state
  , next :: Maybe (Tuple (Doc -> Boolean) State)
  }

be :: Int -> Int -> List (Tuple Int DOC) -> Doc
be w k0 l0 = tailRec go $ State { f: id, k: k0, l: l0, next: Nothing }
  where
  go :: State -> Step State Doc
  go (State { f, k, l, next }) = case l of
    List.Nil ->
      -- obtain the result by running the function on the empty document
      let res = f Nil in
      case next of
        -- if this one fails, continue at the next state
        Just (Tuple p st) | not p res ->
          Loop st
        -- otherwise return what we got
        _ -> Done res
    (Cons (Tuple _ NIL) z) -> Loop $
      State { f, k, l: z, next }
    (Cons (Tuple i (APPEND x y)) z) -> Loop $
      State { f, k, l: (Tuple i x) : (Tuple i y) : z, next }
    (Cons (Tuple i (NEST j x)) z) -> Loop $
      State { f, k, l: (Tuple (i + j) x) : z, next }
    (Cons (Tuple _ (TEXT s)) z) -> Loop $
      State { f: f <<< Text s, k: (k + String.length s), l: z, next }
    (Cons (Tuple i LINE) z) -> Loop $
      State { f: f <<< Line i, k: i, l: z, next }
    (Cons (Tuple i (UNION x y)) z) ->
      let
        -- add a continuation that tests whether the previous doc is too long
        trial = Tuple (fits (w - k)) nextSt
        nextSt = State { f, k, l: (Tuple i y) : z, next }
      in  Loop $ State { f, k, l: (Tuple i x) : z, next: Just trial }

fits :: Int -> Doc -> Boolean
fits w _ | w < 0 = false
fits _ Nil = true
fits w (Text s x) = fits (w - String.length s) x
fits _ (Line _ _) = true

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

bracket' :: Int -> String -> DOC -> String -> DOC
bracket' i l x r = group $ text l <> nest i (line <> x) <> line <> text r

bracket :: String -> DOC -> String -> DOC
bracket = bracket' 2

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
