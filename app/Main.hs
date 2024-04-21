module Main (main) where

import Command
import Options.Applicative as O

main :: IO ()
main = do
    result <- customExecParser (prefs disambiguate) (info commandOptions idm)
    print result
