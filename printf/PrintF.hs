{-# LANGUAGE TemplateHaskell #-}

-- This code is partially from http://www.haskell.org/haskellwiki/Template_Haskell

module PrintF where

-- NB: printf needs to be in a separate module to the one where
-- you intend to use it.

-- Import Template Haskell syntax
import Language.Haskell.TH

-- Possible string tokens: %d %s and literal strings
data Format = D | S | L String
    deriving Show

-- The printf meta-programming function
printf :: String -> ExpQ
printf fmt = gen (parse fmt)

-- Generating Haskell code for a particular format
gen :: [Format] -> ExpQ
gen [D]   = [| \n -> show n |]
gen [S]   = [| \s -> s |]
gen [L s] = [| s |]

-- A poor man's tokenizer
parse :: String -> [Format]
parse [] = []
parse ('%':c:rest) | c == 'd' = D : parse rest
                   | c == 's' = S : parse rest
parse (s:str) = L (s:p) : parse rest -- so we don't get stuck on weird '%'
    where (p, rest) = span (/= '%') str


{-
-- generate argument list for the function
args :: [Format] -> [PatQ]
args fmt = concatMap (\(f,n) -> case f of
                                  L _ -> []
                                  _   -> [varP n]) $ zip fmt names
    where names = [ mkName $ 'x' : show i | i <- [0..] ]

-- generate body of the function
body :: [Format] -> ExpQ
body fmt = foldr (\ e e' -> infixApp e [| (++) |] e') (last exps) (init exps)
    where exps = [ case f of
                    L s -> stringE s
                    D   -> appE [| show |] (varE n)
                    S   -> varE n
                 | (f,n) <- zip fmt names ]
          names = [ mkName $ 'x' : show i | i <- [0..] ]

-- glue the argument list and body together into a lambda
-- this is what gets spliced into the haskell code at the call
-- site of "printf"
printf :: String -> Q Exp
printf format = lamE (args fmt) (body fmt)
    where fmt = tokenize format    
    -}
    
