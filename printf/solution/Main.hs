{-# LANGUAGE TemplateHaskell #-}

import PrintF (printf)

main 
  = putStrLn $ 
      $(printf "Hello %s! The answer is %d.") "World" 42
