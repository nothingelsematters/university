module Main where
import Control.Applicative ((<$>))

main :: IO ()
main = (*) <$> readIntg <*> readIntg >>= print
    where readIntg = readLn::IO Integer
