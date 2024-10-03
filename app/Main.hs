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

module Main where

import System.IO
import qualified Parser as P
import qualified Data.Bits as B
import qualified Interface as I

main :: IO ()
main = do
  putStrLn "What file would you like to read from?"
  filename <- getLine
  fin  <- openFile filename ReadMode
  contents <- hGetContents fin
  I.runStrIO contents
  --putStr contents
  hClose fin
