module Test where

import Data.Bifunctor (first)
import Data.Map qualified as M
import Test.HUnit

import AST qualified
import AbstractMachine qualified as AM
import Auxiliary
import Parser

program2Res :: (FilePath, M.Map (String, String) AM.RuntimeValue)
program2Res = ("/program2.jsx", M.fromList xs)
    where
        xs = [ (("/program2.jsx", "a"), AM.RVInt 1)
             , (("/program2.jsx", "args"), AM.RVComp ["a", "b"] [AST.Let "compvar" (AST.BO AST.Add (AST.Var "a") (AST.Var "b"))] "/program2.jsx")
             , (("/program2.jsx", "b"), AM.RVInt 2)
             , (("/program2.jsx", "compvar"), AM.RVInt 3)
             , (("/program2.jsx", "noargs"), AM.RVComp [] [] "/program2.jsx")
             , (("/program2.jsx", "x"), AM.RVObj (M.fromList [("a", AM.RVInt 1), ("b", AM.RVInt 2), ("c", AM.RVInt 3)]))
             , (("/program2.jsx", "y"), AM.RVInt 0)
             , (("/program2.jsx", "z"), AM.RVInt 9)
             , (("/program4.jsx", "a"), AM.RVInt 1)
             , (("/program4.jsx", "b"), AM.RVInt 2)
             , (("/program4.jsx", "c"), AM.RVInt 3)
             ]


programResults :: [(FilePath, M.Map (String, String) AM.RuntimeValue)]
programResults = [program2Res]

testProgram ::
    M.Map String AST.SrcFile ->
    String ->
    AST.SrcFile ->
    M.Map (String, String) AM.RuntimeValue ->
    Test
testProgram fileGetter entryPoint file expected =
    entryPoint ~: Right expected ~=? AM.runFile entryPoint file fileGetter

findMatchingFst :: (Eq a) => [(a, b)] -> [(a, c)] -> [(a, b, c)]
findMatchingFst [] _ = []
findMatchingFst ((a, b) : xs) ys = case lookup a ys of
    Just c -> (a, b, c) : findMatchingFst xs ys
    Nothing -> findMatchingFst xs ys

main :: IO Counts
main = do
    let rootDir = "./programs"
    paths <- getAllFilePaths rootDir
    fileContents <- mapM readFile paths
    let tuples = zip paths fileContents
    let parseResults = map (fmap parseInput) tuples
    let (_, srcFiles) = partition parseResults
    let amSrcFiles = map (first $ drop (length rootDir)) srcFiles
    let fileGetter = M.fromList amSrcFiles
    let programsTests = findMatchingFst amSrcFiles programResults
    putStrLn "Testing programs"
    mapM_ (print . (\(a,_,_) -> a)) programsTests
    runTestTT $
        TestList $
            map
                (\(entryPoint, src, expected) -> testProgram fileGetter entryPoint src expected)
                programsTests
