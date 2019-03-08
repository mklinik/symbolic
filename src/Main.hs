module Main where

import Foundation

import qualified Data.Map.Strict as M
import qualified Data.Tree as T

import Concrete
import Util
import Programs
import SMT
import Symbolic

main :: IO ()
main = do
  args <- getArgs
  let trace = elem "-t" args
  let prog = listToProgram assertTest
  putStrLn $ show prog
  -- stack <- run trace prog (0, M.empty, [])
  stack <- return []
  putStrLn $ show $ wordToSignedInt <$> stack
  traces <- symRun 50 prog defaultSymState
  putStrLn $ fromString $ T.drawTree $ fmap (toList . show . \(pc,_,_,st,cs) -> (pc, renderSym <$> st, renderSym <$> cs)) traces
  -- solvedTraces <- solveSym traces
  -- putStrLn $ fromString $ T.drawTree $ fmap (toList . renderSolvedState) solvedTraces
