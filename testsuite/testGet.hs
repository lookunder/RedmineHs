{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Data.Aeson
import Data.Map
import Data.Time (Day, fromGregorian)


import Redmine.Get

test1 = TestCase (do let m   = fromList [("toto","a"),("offset","20"), ("limit","100")]
                         ref = "?toto=a&offset=20&limit=100&"
                     assertEqual "Option expansion failed" ref (expandOptions m))

test2 = TestCase (do let m   = fromList [("offset","20"), ("limit","100")]
                         ref = fromList [("offset","120"), ("limit","100")]
                     assertEqual "increaseQueryRange a échouée" ref (increaseQueryRange m))

test3 = TestCase $ assertEqual "Test de parsing d'un Day" (Just $ fromGregorian 1999 01 12) (decode "\"1999-01-12\"")

test4 = TestCase $ assertEqual "Test de parsing d'un Day" ("\"1999-01-12\"") (encode $ fromGregorian 1999 01 12)

--decodeIssue
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3, TestLabel "test4" test4]

main = runTestTT tests
