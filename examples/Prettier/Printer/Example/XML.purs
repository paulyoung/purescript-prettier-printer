module Prettier.Printer.Example.XML where

import Prelude

import Data.List (List, (:))
import Data.List as List
import Effect (Effect)
import Effect.Console (log)
import Prettier.Printer (DOC, bracket, fill, folddoc, nil, pretty, text, words)

-- XML example

data XML
  = Elt String (List Att) (List XML)
  | Txt String

data Att = Att String String

showXML :: XML -> DOC
showXML x = folddoc (<>) (showXMLs x)

showXMLs :: XML -> List DOC
showXMLs (Elt n a List.Nil) = List.singleton $
  text "<" <> showTag n a <> text "/>"
showXMLs (Elt n a c) = List.singleton $
  text "<" <> showTag n a <> text ">" <>
  showFill showXMLs c <>
  text "</" <> text n <> text ">"
showXMLs (Txt s) = map text (words s)

showAtts :: Att -> List DOC
showAtts (Att n v) = List.singleton $
  text n <> text "=" <> text (quoted v)

quoted :: String -> String
quoted s = "\"" <> s <> "\""

showTag :: String -> List Att -> DOC
showTag n a = text n <> showFill showAtts a

showFill :: forall a. (a -> List DOC) -> List a -> DOC
showFill f List.Nil = nil
showFill f xs = bracket "" (fill (List.concat (map f xs))) ""

xml :: XML
xml =
  Elt "p"
    ( Att "color" "red"
    : Att "font" "Times"
    : Att "size" "10"
    : List.Nil
    )
    ( Txt "Here is some"
    : Elt "em"
      List.Nil
      ( Txt "emphasized"
      : List.Nil
      )
    : Txt "text."
    : Txt "Here is a"
    : Elt "a"
      ( Att "href" "http://www.eg.com/"
      : List.Nil
      )
      ( Txt "link"
      : List.Nil
      )
    : Txt "elsewhere."
    : List.Nil
    )

testXML :: Int -> Effect Unit
testXML w = log $ pretty w (showXML xml)
