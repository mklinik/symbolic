module Programs where

import Foundation

import Types
import Util

countDown :: [Instr]
countDown = [ Read
            , Push (twosComplement 1)
            , Add
            , Dup
            , Print
            , Dup
            , Push 1
            , Swap
            , JmpIf
            , Pop
            , Done
            ]

countUp :: [Instr]
countUp = [ Push 0
          , Read
          , Dup
          , RotL
          , RotL
          , Push 1
          , Add
          , Dup
          , Print
          , Dup
          , RotL
          , Eq
          , Not
          , Push 2
          , Swap
          , JmpIf
          , Done]

multiply :: [Instr]
multiply = [ Read
           , Dup
           , Read
           , Push (twosComplement 1) -- start
           , Add
           , Dup
           , Not
           , Push 18 -- end address
           , Swap
           , JmpIf
           , RotL
           , Dup
           , RotL
           , Add
           , RotL
           , Push 3 -- start address
           , Push 1
           , JmpIf
           , Pop --end
           , Pop
           , Print
           , Done ]

addInputs :: [Instr]
addInputs = [ Read
            , Read
            , Add
            , Push 0
            , Store
            , Push 9
            , Read
            , JmpIf
            , Done
            , Push 0
            , Load
            , Done ]

addInputsPrintOver15 :: [Instr]
addInputsPrintOver15 =
  [ Read
  , Read
  , Add
  , Dup
  , Push 15
  , Lt
  , Push 10 -- Address of Print instruction
  , Swap
  , JmpIf
  , Done
  , Print
  , Done ]

loop :: [Instr]
loop = [ Push 0
       , Push 1
       , JmpIf
       , Done
       ]

-- make the input at least 42
atLeast42 :: [Instr]
atLeast42 =
  [ Read
  , Dup
  , Push 42
  , Swap
  , Lt
  , Push 9 -- Address of then branch
  , Swap
  , JmpIf
  , Done -- else: do nothing
  , Push (42) -- then: number is less than 42
  , Add
  , Assert (SLt (SCon 41) (SAny (-1))) -- assert( x > 41 )
  , Done
  ]

assertTest :: [Instr]
assertTest =
  [ Read
  , Assert (SLt (SCon 10) (SAny (-1)))
  , Done
  ]

assertTest2 :: [Instr]
assertTest2 =
  [ Read
  , Dup
  , Push 10
  , Swap
  , Lt
  , Push 9 -- address of then
  , Swap
  , JmpIf
  , Done
  , Assert (SLt (SAny (-1)) (SCon 9)) -- assert( x < 9 ), which is false
  , Done
  ]

-- Path says v_0 < 12 and v_0 > 10
-- Assertion says v0 == 11
assertTest3 :: [Instr]
assertTest3 =
  [ Read
  , Dup
  , Dup
  , Push 10
  , Lt
  , Swap
  , Push 12
  , Swap
  , Lt
  , And
  , Push 14 -- address of then
  , Swap
  , JmpIf
  , Done
  , Assert (SEq (SAny (-1)) (SCon 11)) -- assert( x == 11 )
  , Done
  ]

listToProgram :: [Instr] -> Prog
listToProgram = fromList
