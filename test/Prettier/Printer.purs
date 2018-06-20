module Test.Prettier.Printer where

import Prelude

import Control.Monad.Gen (oneOf)
import Data.NonEmpty ((:|))
import Effect.Class (liftEffect)
import Prettier.Printer (DOC, line, nest, nil, pretty, text)
import Test.QuickCheck (class Arbitrary, arbitrary, quickCheck, (===))
import Test.Spec (Spec, describe, it)

newtype DOC' = DOC' DOC

instance arbDOC' :: Arbitrary DOC' where
  arbitrary = oneOf $
    pure (DOC' nil) :| --map pure
      [ DOC' <<< text <$> arbitrary
      , pure <<< DOC' $ line
      ]

spec :: Spec Unit
spec = describe "Prettier.Printer" do
  describe "text" do
    it "is a homomorphism from string concatenation to document concatenation" do
      liftEffect $ quickCheck \w s t ->
        pretty w (text (s <> t)) === pretty w ((text s <> text t))

      liftEffect $ quickCheck \w ->
        pretty w (text "") === pretty w nil

  describe "nest" do
    it "is a homomorphism from addition to composition" do
      liftEffect $ quickCheck \w i j (DOC' x) ->
        pretty w (nest (i + j) x) == pretty w (nest i (nest j x))

      liftEffect $ quickCheck \w (DOC' x) ->
        pretty w (nest 0 x) == pretty w x

    it "distributes through concatenation" do
      liftEffect $ quickCheck \w i (DOC' x) (DOC' y) ->
        pretty w (nest i (x <> y)) == pretty w (nest i x <> nest i y)

      liftEffect $ quickCheck \w i ->
        pretty w (nest i nil) == pretty w nil

    it "is absorbed by text" do
      liftEffect $ quickCheck \w i s ->
        pretty w (nest i (text s)) == pretty w (text s)
