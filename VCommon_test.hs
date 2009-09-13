module Main where

import Test.HUnit
import qualified Data.Map as Map
import VCommon

tests = test 
        -- positive tests
        ["happy path" ~: read "name;param=pval1,pval2:value" 
                          @?= ContentLine "name" 
                                  (Map.singleton "param" "pval1,pval2") 
                                  "value"
        , "empty value" ~: read "name;param=pval:" 
                            @?= ContentLine "name" 
                                    (Map.singleton "param" "pval")
                                    ""
        , "empty params" ~: read "name:value" 
                             @?= ContentLine "name"
                                     (Map.empty)
                                     "value"
        , "empty param value" ~: read "name;param=:value"
                                  @?= ContentLine "name"
                                          (Map.singleton "param" "")
                                          "value"
        , "empty name" ~: read ":value" @?= ContentLine "" (Map.empty) "value"
        -- negative tests
        , "no colon" ~: do res <- catch (readIO "sometext") 
                                  (\e -> do return "")
                           assertEqual "read of wrong string had to rise" "" res]

main :: IO ()
main = do res <- runTestTT tests
          putStr ((show res) ++ "\n")