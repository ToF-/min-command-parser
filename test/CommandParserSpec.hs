{-# LANGUAGE OverloadedStrings #-}
module CommandParserSpec
    where

import Command

import Options.Applicative
import Test.Hspec

spec :: SpecWith ()
spec = do
    describe "parsing command" $ do
        it "recognizes the version command" $ do
            let args = words "version"
            let (Success cmd) = parseCommand args
            cmd `shouldBe` Version

        it "recognizes the accounts command" $ do
            let args = words "accounts"
            let (Success cmd) = parseCommand args
            cmd `shouldBe` Accounts

        it "recognizes the (abbreviated) ver command" $ do
            let args = words "vers"
            parseCommandIO args
            let (Success cmd) = parseCommand args
            cmd `shouldBe` Version

        it "recognizes the reassign command without any flag" $ do
            let args = words "reassign"
            parseCommandIO args

        it "recognizes the reassign command with a from flag" $ do
            let args = words "reassign --from foobar"
            parseCommandIO args
            let (Success cmd) = parseCommand args
            cmd `shouldBe` Reassign "foobar"

        it "recognizes the transfer  command with a from flag and a to flag" $ do
            let args = words "transfer --from foo --to bar"
            parseCommandIO args
            let (Success cmd) = parseCommand args
            cmd `shouldBe` Transfer "foo" "bar"

        it "recognizes the cash command with a eur flag or a usd flag" $ do
            let args = words "cash --EUR"
            let (Success cmd) = parseCommand args
            cmd `shouldBe` Cash EUR

        it "recognizes the summary command with -m -y and a year and month" $ do
            let args = words "summary -y 2024 -m 04"
            parseCommandIO args
            let (Success cmd) = parseCommand args
            cmd `shouldBe` Summary (Month 2024 04)

        it "recognizes the detail command with maybe a currency and maybe an account" $ do
            let args = words "detail --EUR -a MyAccount"
            let (Success cmd) = parseCommand args
            cmd `shouldBe` Detail (Just EUR) (Just "MyAccount")
