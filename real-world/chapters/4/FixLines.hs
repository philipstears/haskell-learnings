import SplitLines
import System.Environment (getArgs)

interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

main = mainWith myFunction
    where
        mainWith function = do
            args <- getArgs
            case args of
                [input, output] -> interactWith function input output
                _ -> putStrLn "usage: interact inputFileName outputFileName"

        -- the function that will be executed, change id
        fixLines input = unlines (splitLines input)
        myFunction = fixLines
