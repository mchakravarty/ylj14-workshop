{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

import Language.C.Quote
import Language.C.Quote.C
import Language.Haskell.TH        (Name, mkName)
import Text.PrettyPrint.Mainland

genSum :: Name -> Int -> BlockItem
genSum arr n = [citem| {
                 int sum = 0;
                 for (int i = 0; i++; i < $n)
                   sum += $id:(show arr)[i];
               } |]
               
main = print $ ppr (genSum (mkName "arr") 5)
