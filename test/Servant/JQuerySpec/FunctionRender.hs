{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.JQuerySpec.FunctionRender where

import Data.Either (isRight)
import Data.List
import Data.Proxy
import Language.ECMAScript3.Parser (parseFromString)
import Test.Hspec

import Servant.API
import Servant.JQuery

data Book = Book String String

type BookAPI = "books" :> Capture "isbn" String :> Get Book

bookAPIProxy :: Proxy BookAPI
bookAPIProxy = Proxy

renderFunctionJSSpec :: Spec
renderFunctionJSSpec = describe "JS function rendering" $ do
    it "should generate valid javascript with default settings" $ do
        let out = generateJS' defaultSettings $ jquery bookAPIProxy
        ("\nfunction getbooks(isbn,onSuccess,onError){" `isPrefixOf` out) `shouldBe` True
        ("}\n" `isSuffixOf` out) `shouldBe` True
        parseFromString out `shouldSatisfy` isRight

    it "should generate valid javascript hoisted function" $ do
        let out = generateJS' (Settings Hoisted "") $ jquery bookAPIProxy
        ("\nfunction getbooks(isbn,onSuccess,onError){" `isPrefixOf` out) `shouldBe` True
        ("}\n" `isSuffixOf` out) `shouldBe` True
        parseFromString out `shouldSatisfy` isRight

    it "should generate valid javascript non-hoisted function" $ do
        let out = generateJS' (Settings NonHoisted "") $ jquery bookAPIProxy
        ("\nvar getbooks = function (isbn,onSuccess,onError){" `isPrefixOf` out) `shouldBe` True
        ("};\n" `isSuffixOf` out) `shouldBe` True
        parseFromString out `shouldSatisfy` isRight

    it "should generate valid javascript module function" $ do
        let out = generateJS' (Settings (Module "Foo.Bar") "") $ jquery bookAPIProxy
        ("\nFoo.Bar.getbooks = function (isbn,onSuccess,onError){" `isPrefixOf` out) `shouldBe` True
        ("};\n" `isSuffixOf` out) `shouldBe` True
        parseFromString out `shouldSatisfy` isRight

    it "should generate valid anonymous function" $ do
        let out = generateJS' (Settings (Anonymous False) "") $ jquery bookAPIProxy
        ("\nfunction (isbn,onSuccess,onError){" `isPrefixOf` out) `shouldBe` True
        ("}\n" `isSuffixOf` out) `shouldBe` True
        parseFromString out `shouldSatisfy` isRight

    it "should generate valid anonymous function variable" $ do
        let out = generateJS' (Settings (Anonymous True) "") $ jquery bookAPIProxy
        ("\nreturn function (isbn,onSuccess,onError){" `isPrefixOf` out) `shouldBe` True
        ("};\n" `isSuffixOf` out) `shouldBe` True
        parseFromString out `shouldSatisfy` isRight

    it "should generate valid purescript-friendly function" $ do
        let out = generateJS' (Settings PurescriptFriendly "") $ jquery bookAPIProxy
        ("\nfunction getbooks(isbn,onSuccess,onError){\n\nreturn function (){\n" `isPrefixOf` out) `shouldBe` True
        ("};\n}\n" `isSuffixOf` out) `shouldBe` True
        parseFromString out `shouldSatisfy` isRight
