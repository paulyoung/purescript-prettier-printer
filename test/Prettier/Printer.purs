module Test.Prettier.Printer where

import Prelude

import Data.NonEmpty ((:|))
import Prettier.Printer (DOC, line, nest, nil, pretty, text)
import Test.QuickCheck (class Arbitrary, arbitrary, (===))
import Test.QuickCheck.Gen (oneOf)
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck)

newtype DOC' = DOC' DOC

instance arbDOC' :: Arbitrary DOC' where
  arbitrary = oneOf $
    pure (DOC' nil) :| --map pure
      [ DOC' <<< text <$> arbitrary
      , pure <<< DOC' $ line
      ]

spec :: Spec (QCRunnerEffects ()) Unit
spec = describe "Prettier.Printer" do
  describe "text" do
    it "is a homomorphism from string concatenation to document concatenation" do
      quickCheck \w s t ->
        pretty w (text (s <> t)) === pretty w ((text s <> text t))

      quickCheck \w ->
        pretty w (text "") === pretty w nil

  describe "nest" do
    it "is a homomorphism from addition to composition" do
      quickCheck \w i j (DOC' x) ->
        pretty w (nest (i + j) x) == pretty w (nest i (nest j x))

      quickCheck \w (DOC' x) ->
        pretty w (nest 0 x) == pretty w x

    it "distributes through concatenation" do
      quickCheck \w i (DOC' x) (DOC' y) ->
        pretty w (nest i (x <> y)) == pretty w (nest i x <> nest i y)

      quickCheck \w i ->
        pretty w (nest i nil) == pretty w nil

    it "is absorbed by text" do
      quickCheck \w i s ->
        pretty w (nest i (text s)) == pretty w (text s)
