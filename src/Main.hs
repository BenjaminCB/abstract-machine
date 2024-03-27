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

prettyPrintMap :: (Show k, Show v) => M.Map k v -> IO ()
prettyPrintMap m = do
    mapM_ (\(k, v) -> putStrLn $ "    " ++ show k ++ ": " ++ show v) (M.toList m)

prettyPrintTupleWithMap :: (Show k, Show v) => (k, M.Map k v) -> IO ()
prettyPrintTupleWithMap (k, m) = do
    putStrLn $ "  " ++ show k ++ ":"
    prettyPrintMap m

prettyPrintLocals :: M.Map String (M.Map String AM.RuntimeValue) -> IO ()
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
            let amResult = AM.runFile entryPoint src fileGetter
            case amResult of
                Left err -> do
                    putStrLn "Abstract Machine Error:"
                    putStrLn err
                Right locals -> do
                    prettyPrintLocals locals
        Nothing -> do
            putStrLn "Entry point not found"
