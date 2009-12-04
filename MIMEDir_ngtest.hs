module Main where

import Test.HUnit
import qualified Data.Map as Map
import MIMEDir

clTests = test 
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

vcTests = test [
           "takeBeforeCRLF" ~: takeBeforeCRLF "text\r\nothertext" @?= "text"
          , "readCL" ~: readContentLine "begin:vcard" 
                         @?= ContentLine "begin" Map.empty "vcard"
          , "reads" ~: reads "begin:vcard\r\ntherest" 
                        @?= [(ContentLine "begin" Map.empty "vcard", "therest")]
          , "readCLs1" ~: readContentLines "begin:vcard\r\n"
                           @?= [ContentLine "begin" Map.empty "vcard"] 
          , "readCLs2" ~: readContentLines "begin:vcard\r\nend:vcard\r\n"
                           @?= [ContentLine "begin" Map.empty "vcard"
                               , ContentLine "end" Map.empty "vcard"] 
          , "happy path" ~: mimeDirToString (mimeDirFromString "BEGIN:VCARD\r\nEND:VCARD")
                             @?= mimeDirToString (MIMEDir "VCARD" Map.empty)
          , "reverse happy path" ~: mimeDirToString MIMEDir 
                                     {kind = "INDEX", 
                                      contents = 
                                          (Map.fromList 
                                                  [("1234321", Left [(Map.empty, "PP")])
                                                   , ("FIELD", Left [(Map.empty, "TEL")])])}
                                     @?= "BEGIN:INDEX\r\n1234321:PP\r\nFIELD:TEL\r\nEND:INDEX\r\n"
         ]


main :: IO ()
main = do putStr "ContentLine tests:\n"       
          _ <- runTestTT clTests
          putStr "MIMEDir tests:\n"
          _ <- runTestTT vcTests
          return ()
