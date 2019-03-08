module Symbolic where

import Foundation
import Foundation.Collection

import qualified Data.Map.Strict as M
import qualified Data.Tree as T
import qualified Data.SBV.Dynamic as S

import Types
import Util
import SMT

symRun :: Int -> Prog -> SymState -> IO Trace
symRun maxDepth prog state@(pc, _, _, _, _) =
  case prog ! (Offset pc) of
    Just Done -> return $ T.Node state []
    Just instr ->
      if maxDepth > 0 then do
        newStates <- symStep state instr
        children <- sequence (symRun (maxDepth - 1) prog <$> newStates)
        return $ T.Node state children
      else
        return $ T.Node state []
    Nothing ->
      error $ "No instruction at " <> show pc

symStep :: SymState -> Instr -> IO [SymState]
symStep (pc, i, mem, l:r:stack, cs) Add = return $ pure (pc+1, i, mem, SAdd l r : stack, cs)
symStep _ Add = error "Add expects two arguments."
symStep (pc, i, mem, stack, cs) Read = return $ pure (pc+1, i+1, mem, SAny i : stack, cs)
symStep (pc, i, mem, stack, cs) (Push w) = return $ pure (pc+1, i, mem, SCon w : stack, cs)
symStep (pc, i, mem, _:stack, cs) Pop = return $ pure (pc+1, i, mem, stack, cs)
symStep _ Pop = error "Pop expects one argument."
symStep (pc, i, mem, w:stack, cs) Dup = return $ pure (pc+1, i, mem, w:w:stack, cs)
symStep _ Dup = error "Dup expects one argument."
symStep (pc, i, mem, _:stack, cs) Print = return $ pure (pc+1, i, mem, stack, cs)
symStep _ Print = error "Print expects one argument."
symStep (pc, i, mem, x:y:stack, cs) Swap = return $ pure (pc+1, i, mem, y:x:stack, cs)
symStep _ Swap = error "Swap expects two arguments."
symStep (pc, i, mem, cond:SCon addr:stack, cs) JmpIf = return
  [ (pc+1, i, mem, stack, SNot cond : cs)
  , (wordToInt addr, i, mem, stack, cond : cs)
  ]
symStep (pc, i, mem, _:_:stack, cs) JmpIf =
  -- If the jump address is not concrete, don't explore that branch
  -- The jump could be to anywhere in the program.
  return $ pure (pc+1, i, mem, stack, cs)
symStep _ JmpIf = error "JmpIf expects two arguments."
symStep (pc, i, mem, w:stack, cs) Over = return $ pure (pc+1, i, mem, w:stack <> [w], cs)
symStep _ Over = error "Over expects one argument."
symStep (pc, i, mem, w:stack, cs) RotL = return $ pure (pc+1, i, mem, stack <> [w], cs)
symStep _ RotL = error "RotL expects one argument."
symStep (pc, i, mem, w:stack, cs) Not = return $ pure (pc+1, i, mem, SNot w:stack, cs)
symStep _ Not = error "Not expects one argument."
symStep (pc, i, mem, l:r:stack, cs) And = return $ pure (pc+1, i, mem, SAnd l r:stack, cs)
symStep _ And = error "And expects two arguments."
symStep (pc, i, mem, l:r:stack, cs) Or = return $ pure (pc+1, i, mem, SOr l r:stack, cs)
symStep _ Or = error "Or expects two arguments."
symStep (pc, i, mem, l:r:stack, cs) Lt = return $ pure (pc+1, i, mem, SLt l r: stack, cs)
symStep _ Lt = error "Lt expects two arguments."
symStep (pc, i, mem, l:r:stack, cs) Eq = return $ pure (pc+1, i, mem, SEq l r: stack, cs)
symStep _ Eq = error "Eq expects two arguments."
symStep (pc, i, mem, SCon addr:w:stack, cs) Store = return $ pure (pc+1, i, M.insert addr w mem, stack, cs)
symStep (pc, i, mem, _:_:stack, cs) Store =
  -- Only handle concrete addresses for now.
  return $ pure (pc+1, i, mem, stack, cs)
symStep _ Store = error "Store expects two arguments."
symStep (pc, i, mem, SCon addr:stack, cs) Load =
  case M.lookup addr mem of
    Just w -> return $ pure (pc+1, i, mem, w:stack, cs)
    Nothing -> error "Nothing to Load at address."
symStep (pc, i, mem, _:stack, cs) Load =
  -- Only handle concrete addresses for now.
  return $ pure (pc+1, i+1, mem, SAny i: stack, cs)
symStep _ Load = error "Store expects two arguments."
symStep (pc, i, mem, stack@(v:_), cs) (Assert predicate) = do
  let assertion = makeAssertion predicate v
  putStrLn $ "checking assertion " <> renderSym assertion
  let smtExpr = toSMT $ assertion : cs
  S.SatResult smtRes <- S.satWith S.z3 smtExpr
  putStrLn $ renderSMTResult smtRes
  return $ pure (pc+1, i+1, mem, stack, cs)
symStep _ (Assert _) =
  error "Assert expects one argument."

symStep _ Done = error "No step for Done"

defaultSymState :: SymState
defaultSymState = (0, 0, M.empty, [], [])

-- replace (SAny (-1)) with the given sym
makeAssertion :: Sym -> Sym -> Sym
makeAssertion (SAdd l r)  x = SAdd (makeAssertion l x) (makeAssertion r x)
makeAssertion s@(SCon _)  _ = s
makeAssertion (SAny (-1)) x = x
makeAssertion s@(SAny _)  _ = s
makeAssertion (SEq l r)   x = SEq (makeAssertion l x) (makeAssertion r x)
makeAssertion (SNot c)    x = SNot (makeAssertion c x)
makeAssertion (SAnd l r)  x = SAnd (makeAssertion l x) (makeAssertion r x)
makeAssertion (SOr l r)   x = SOr (makeAssertion l x) (makeAssertion r x)
makeAssertion (SLt l r)   x = SLt (makeAssertion l x) (makeAssertion r x)
