module Main where

main :: IO ()
main = do
    putStr "Enter depth of triangle (must be greater than or equal to 0): "
    input <- getLine
    let n = (read input :: Int)
    printTriangle n

printTriangle :: Int -> IO ()
printTriangle n = putStrLn $ unlines (triangle n)

triangle :: Int -> [String]
triangle n
    | n < 0     = error "can't have negative n"
    | n == 0    = smallest
    | otherwise = let t = triangle (n-1)
                      padding = replicate (halfWidthOfTriangle t) ' '
                      upper = map (\line -> padding ++ line ++ padding) t
                      lower = zipWith (++) t t
                  in upper ++ lower

halfWidthOfTriangle :: [String] -> Int
halfWidthOfTriangle = (`div` 2) . length . head

smallest :: [String]
smallest = lines " /\\ \n\
                 \/\\/\\"
