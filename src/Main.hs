module Main where

import Data.Bifunctor (first)
import Data.Map qualified as M
import System.Environment (getArgs)
import Text.Parsec (ParseError)

import AbstractMachine qualified as AM
import Auxiliary
import Parser

newlines :: Int -> IO ()
newlines = putStr . concat . flip replicate "\n"

printParseError :: (FilePath, ParseError) -> IO ()
printParseError (path, err) = do
    putStrLn path
    print err
    newlines 1

prettyPrintTupleWithMap :: (Show k, Show v) => ((k, k), v) -> IO ()
prettyPrintTupleWithMap (k, m) = do
    putStrLn $ show k ++ " -> " ++ show m

prettyPrintLocals :: M.Map (String, String) AM.RuntimeValue -> IO ()
prettyPrintLocals locals = do
    putStrLn "locals:"
    mapM_ prettyPrintTupleWithMap (M.toList locals)

main :: IO ()
main = do
    [rootDir, entryPoint] <- getArgs
    paths <- getAllFilePaths rootDir
    fileContents <- mapM readFile paths
    let tuples = zip paths fileContents
    let parseResults = map (fmap parseInput) tuples
    let (errs, srcFiles) = partition parseResults
    if null errs
        then putStrLn "No parse errors"
        else do
            putStrLn "Parse Errors:"
            mapM_ printParseError errs
            newlines 3
    let fileGetter = M.fromList (map (first $ drop (length rootDir)) srcFiles)
    case M.lookup entryPoint fileGetter of
        Just src -> do
            let amResult = AM.runFileWithState entryPoint src fileGetter
            case amResult of
                Left err -> do
                    putStrLn "Abstract Machine Error:"
                    putStrLn err
                Right (locals, (cost, trace)) -> do
                    prettyPrintLocals locals
                    putStrLn $ "cost: " ++ show cost
                    putStrLn "trace:"
                    putStrLn $ AM.traceToTypst trace
        Nothing -> do
            putStrLn "Entry point not found"
