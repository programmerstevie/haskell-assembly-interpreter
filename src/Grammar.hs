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

module Grammar where

import qualified Data.Vector as V
import Data.Word
import Data.Int
import qualified Data.Array as A

defaultVal :: Val
defaultVal = 0

type Reg = String

type Val = Word32
type SVal = Int32

type Lbl = String

data Msg = UArgMsg Arg
         | ArgMsg  Arg
         | StrMsg  String
         deriving (Eq, Show)

data Arg = Val Val
         | Reg Reg
         deriving (Eq, Show)

type Program = V.Vector ProgramLine

data ProgramLine = InstrLine Instruction
                 | LabelLine Lbl
                 | Empty
                 deriving (Eq, Show)

data Instruction = Inc Reg
                 | Dec Reg
                 | Mov Reg Arg -- two arg instrs
                 | Add Reg Arg
                 | Sub Reg Arg
                 | Mul Arg
                 | Div Reg Arg -- Int division
                 | Xor Reg Arg  --
                 | And Reg Arg
                 | Or  Reg Arg
                 | Not Reg
                 | Neg Reg
                 | Shr Reg Arg
                 | Shl Reg Arg
                 | Rol Reg Arg
                 | Ror Reg Arg
                 | Cmp Arg Arg 
                 | Jmp Lbl -- jumps
                 | Jne Lbl
                 | Je  Lbl
                 | Jge Lbl
                 | Jl  Lbl
                 | Jg  Lbl
                 | Jle Lbl
                 | Jo  Lbl
                 | Jc  Lbl
                 | Jp  Lbl
                 | Js  Lbl
                 | Jno Lbl
                 | Jnc Lbl
                 | Jnp Lbl
                 | Jns Lbl
                 | Call Lbl
                 | Push Arg
                 | Pop Reg
                 | PushF
                 | PopF
                 | Msg [Msg]
                 | Ret
                 | End
                 | UnsafeEnd
                 deriving (Eq, Show)
