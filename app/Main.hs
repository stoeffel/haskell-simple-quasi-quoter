{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Lib
import Protolude

main :: IO ()
main = do
  let expr = [Lib.qq|1|]
  let expr' =
        [Lib.qq|
                (+
                  (* 4 (* 2 11))
                  (- 8 3)
                )
                |]
  print (interpret expr)
  print (interpret expr')
