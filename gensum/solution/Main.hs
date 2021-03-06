{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

import Language.C.Quote
import Language.C.Quote.C
import Language.Haskell.TH        (Name, mkName)
import Text.PrettyPrint.Mainland

genSum :: Name -> Int -> BlockItem
genSum arr n = [citem| {
                 int sum = 0;
                 $stms:(additions 0)
               } |]
  where
    additions i | i == n    = []
                | otherwise = [cstms| 
                                sum += $id:(show arr)[ $int:i ];
                                $stms:(additions (i + 1))
                              |]
               
main = print $ ppr (genSum (mkName "arr") 5)
