module Test where

import Data.Bifunctor (first)
import Data.Map qualified as M
import Test.HUnit

import AST qualified
import AbstractMachine qualified as AM
import Auxiliary
import Parser

program2Res :: (FilePath, M.Map String (M.Map String AM.RuntimeValue))
program2Res =
    ( "/program2.jsx"
    , M.fromList
        [
            ( "/program2.jsx"
            , M.fromList
                [ ("a", AM.RVInt 1)
                , ("args", AM.RVComp ["a", "b"] [AST.Let "compvar" (AST.BO AST.Add (AST.Var "a") (AST.Var "b"))] "/program2.jsx")
                , ("b", AM.RVInt 2)
                , ("compvar", AM.RVInt 3)
                , ("noargs", AM.RVComp [] [] "/program2.jsx")
                , ("x", AM.RVObj (M.fromList [("a", AM.RVInt 1), ("b", AM.RVInt 2), ("c", AM.RVInt 3)]))
                , ("y", AM.RVInt 0)
                , ("z", AM.RVInt 9)
                ]
            )
        ,
            ( "/program4.jsx"
            , M.fromList
                [ ("a", AM.RVInt 1)
                , ("b", AM.RVInt 2)
                , ("c", AM.RVInt 3)
                ]
            )
        ]
    )
-- Should succede (Accesse nested projection)
program8Res :: (FilePath, M.Map String (M.Map String AM.RuntimeValue))
program8Res =
    ( "/nested_imports/program8.jsx"
    , M.fromList [
        ("/nested_imports/program6.jsx"
            , M.fromList
                [
                    ("a", AM.RVInt 2),
                    ("b", AM.RVInt 3)
                ]
            )
        ,   ("/nested_imports/program7.jsx"
            , M.fromList
                [
                    ("c", AM.RVObj (M.fromList [("a", AM.RVInt 2), ("b", AM.RVInt 3)])),
                    ("d", AM.RVInt 2)
                ]
            )
        ,   ("/nested_imports/program8.jsx"
            , M.fromList
                [
                    ("a", AM.RVInt 2),
                    ("c", AM.RVObj (M.fromList [("a", AM.RVInt 2), ("b", AM.RVInt 3)])),
                    ("x", AM.RVObj (M.fromList [(
                        "c",
                        AM.RVObj (M.fromList [("a", AM.RVInt 2), ("b", AM.RVInt 3)])
                    ), ("d", AM.RVInt 2)]))
                ]
            )
    ])
-- Expected to fail until alpha conversion is added (z is modified after component declaration)
program9Res :: (FilePath, M.Map String (M.Map String AM.RuntimeValue))
program9Res = ( "/alpha/program9.jsx"
    , M.fromList [
        ("/alpha/program9.jsx", M.fromList
            [
                ("z", AM.RVInt 42),
                ("button", AM.RVComp [] [AST.Let "y" (AST.Var "z")] "/alpha/program9.jsx"),
                ("y", AM.RVInt 0)
            ]
        )
    ])

programResults :: [(FilePath, M.Map String (M.Map String AM.RuntimeValue))]
programResults = [program2Res, program8Res,program9Res]

testProgram ::
    M.Map String AST.SrcFile ->
    String ->
    AST.SrcFile ->
    M.Map String (M.Map String AM.RuntimeValue) ->
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
    let (errs, srcFiles) = partition parseResults
    if not (null errs)
    then do
        putStrLn "Errors in parsing"
        mapM_ print errs
        putStrLn ""
    else
        putStrLn "All files parsed successfully"
    let amSrcFiles = map (first $ drop (length rootDir)) srcFiles
    let fileGetter = M.fromList amSrcFiles
    let programsTests = findMatchingFst amSrcFiles programResults
    putStrLn "Testing programs"
    mapM_ (print . (\(a,_,_) -> a)) programsTests
    putStrLn ""
    runTestTT $
        TestList $
            map
                (\(entryPoint, src, expected) -> testProgram fileGetter entryPoint src expected)
                programsTests
