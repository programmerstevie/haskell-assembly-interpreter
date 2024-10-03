-- This file is part of Assembly Language Parser and Evaluator in Haskell.
--
-- Assembly Language Parser and Evaluator in Haskell is free software: you can
-- redistribute it and/or modify it under the terms of the GNU General Public
-- License as published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.
--
-- Assembly Language Parser and Evaluator in Haskell is distributed in the hope
-- that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
-- warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License along with
-- Assembly Language Parser and Evaluator in Haskell. If not, see <https://www.gnu.org/licenses/>.

module Interface 
( runStrIO
, runFileIO
, runStr
, runFile
) where

import Grammar (Program)
import Evaluator (CPUState(..), evaluate, startingState)
import Data.Maybe (fromJust)
import qualified Data.Bits as B
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Parser as P
import Control.Monad.State

type CPU a = State CPUState a

-- newtype CPU a = CPU {nextLine :: CPUState -> ((a, Bool), CPUState)} // Bool is whether it terminated

cpuFlush :: CPU (IO ())
cpuFlush= state $ \cpu -> let cpu' = evaluate cpu in (printAll cpu', cpu' { printLns = S.empty })

cpuFlushString :: CPU String
cpuFlushString = state $ \cpu -> let cpu' = evaluate cpu in (getMessage cpu', cpu' { printLns = S.empty })

---------------------------------------------------------


runProgramIO :: Program -> IO ()
runProgramIO = runProgramFromIO startingState

runProgramFromIO :: CPUState -> Program -> IO ()
runProgramFromIO cpu prg = 
    mapM_ putStr $ map fst prgStates ++ ["The program terminated with exit code: " ++ code]
  where
    code = fst . head . snd $ msgStates
    prgStates = fst msgStates
    msgStates = span (\(str, cpu') -> not $ terminated cpu')
              $ iterate (flushMsg . evaluate . snd) ("", cpu {program = prg})

takeWhileThen :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
takeWhileThen f g [] = []
takeWhileThen f g xs@(x:xtail)
  | f x       = x : takeWhileThen f g xtail
  | otherwise = takeWhile g xs

takeWhileN :: Int -> (a -> Bool) -> [a] -> [a]
takeWhileN n f [] = []
takeWhileN n f xs@(x:xtail)
  | f x       = x : takeWhileN n f xtail
  | otherwise = take n xs

runStr :: String -> CPUState
runStr contents = run $ P.mainParser contents

runStrIO :: String -> IO ()
runStrIO contents = runProgramIO $ P.mainParser contents

runFile :: String -> IO CPUState
runFile fileName = 
  do
    prg <- P.mainFileParser fileName
    return $ run prg

runFileIO :: String -> IO ()
runFileIO fileName = 
  do
    prg <- P.mainFileParser fileName
    runProgramIO prg

run :: Program -> CPUState
run = runFrom startingState

runFrom :: CPUState -> Program -> CPUState
runFrom cpu prg = fromJust 
                $ F.find terminated
                $ iterate evaluate $ cpu {program = prg}

flush :: CPUState -> (IO (), CPUState)
flush cpu = (printAll cpu, cpu { printLns = S.empty })

flushMsg :: CPUState -> (String, CPUState)
flushMsg cpu = (getMessage cpu, cpu { printLns = S.empty })

printAll :: CPUState -> IO ()
printAll cpu = mapM_ putStrLn $ printLns cpu

getPrintLines :: CPUState -> [String]
getPrintLines cpu = F.toList $ printLns cpu

getMessage :: CPUState -> String
getMessage = unlines . getPrintLines