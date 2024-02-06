import System.Environment


main = do
    print "Hello, world"

    contents <- getArgs >>= readFile . head
    print $ take 20 contents