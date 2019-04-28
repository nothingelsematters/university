import System.IO

main :: IO ()
main = do
    contents <- readFile "rps2.in"
    let args = map (\x -> read x::Int) $ words contents
    let subt a b c = args !! a - args !! b - args !! c
    writeFile "rps2.out" (show . maximum $ [0, subt 1 4 3, subt 2 5 4, subt 0 3 5])
