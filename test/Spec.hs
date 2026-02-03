module Main (main) where

import Control.Monad (forM)
import Data.List (sort)
import System.Directory (doesFileExist, getCurrentDirectory, listDirectory)
import System.Exit (ExitCode, exitFailure)
import System.FilePath (replaceExtension, takeExtension, (</>))
import System.Process (CreateProcess(..), readCreateProcessWithExitCode, proc)

main :: IO ()
main = do
    root <- getCurrentDirectory
    let testDir = root </> "test"
    entries <- listDirectory testDir
    let programFiles = sort [testDir </> entry | entry <- entries, takeExtension entry == ".nu"]
    results <- forM programFiles (runTest root)
    let failures = [message | Left message <- results]
    mapM_ putStrLn failures
    if null failures
        then putStrLn "All tests passed."
        else exitFailure

runTest :: FilePath -> FilePath -> IO (Either String ())
runTest root filePath = do
    let expectedPath = replaceExtension filePath ".out"
    exists <- doesFileExist expectedPath
    if not exists
        then pure (Left ("Missing expected output file: " <> expectedPath))
        else do
            expected <- readFile expectedPath
            let process = (proc "stack" ["exec", "nura-exe", "--", filePath]) { cwd = Just root }
            (exitCode, out, err) <- readCreateProcessWithExitCode process ""
            let actual = out <> err
            if actual == expected
                then pure (Right ())
                else pure (Left (formatFailure filePath expected actual exitCode))

formatFailure :: FilePath -> String -> String -> ExitCode -> String
formatFailure filePath expected actual exitCode =
    "Test failed: " <> filePath <> "\n"
    <> "Exit: " <> show exitCode <> "\n"
    <> "Expected:\n" <> expected <> "\n"
    <> "Actual:\n" <> actual <> "\n"
