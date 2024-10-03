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

module Evaluator
( CPUState(..)
, evaluate 
, startingState
) where

import Grammar

import Debug.Trace
import Data.List (uncons)
import qualified Data.Bits as B
import qualified Data.Sequence as S
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

type Stack = [Val]
type CallStack = [Int]

data CPUState = CPUState { registers  :: HM.HashMap Reg Val
                         , callStack  :: CallStack
                         , stack      :: Stack
                         , flags      :: Val
                         , currLine   :: Int
                         , printLns   :: S.Seq String
                         , program    :: Program
                         , terminated :: Bool
                         } deriving (Eq, Show)
-- 0 CF, 2 PF, 6 ZF, 7 SF, 9 IF, 11 OF 
-- Carry Flag, Parity Flag, Zero Flag, Sign Flag, Interrupt Flag, OverFlow Flag

startingState :: CPUState
startingState = CPUState
 { registers  = HM.empty
 , callStack  = []
 , stack      = []
 , flags      = 0
 , currLine   = 0
 , printLns   = S.empty
 , program    = V.empty
 , terminated = False
}

evaluate :: CPUState -> CPUState
evaluate cpu = advance $ case prg V.!? currLine cpu of
    Just (InstrLine (Inc reg)) -> add reg (Val 1) cpu
    Just (InstrLine (Dec reg)) -> subSafe reg (Val 1) cpu
    Just (InstrLine (Mov reg arg)) -> mov reg arg cpu
    Just (InstrLine (Add reg arg)) -> add reg arg cpu
    Just (InstrLine (Sub reg arg)) -> subSafe reg arg cpu
    Just (InstrLine (Mul arg)) -> mul arg cpu 
    Just (InstrLine (Div reg arg)) -> adjRegWithArg (flip div) reg arg cpu
    Just (InstrLine (Xor reg arg)) -> adjRegWithArg B.xor reg arg cpu
    Just (InstrLine (And reg arg)) -> adjRegWithArg (B..&.) reg arg cpu
    Just (InstrLine (Or  reg arg)) -> adjRegWithArg (B..|.) reg arg cpu
    Just (InstrLine (Not reg)) -> adjReg B.complement reg cpu
    Just (InstrLine (Neg reg)) -> adjReg negate reg cpu
    Just (InstrLine (Shr reg arg)) -> shift32 RIGHT reg arg cpu
    Just (InstrLine (Shl reg arg)) -> shift32 LEFT  reg arg cpu
    Just (InstrLine (Rol reg arg)) -> rotate32 LEFT  reg arg cpu
    Just (InstrLine (Ror reg arg)) -> rotate32 RIGHT reg arg cpu
    Just (InstrLine (Cmp arg0 arg1)) -> comp arg0 arg1 cpu
    Just (InstrLine (Jmp lbl))  -> jump lbl cpu
    Just (InstrLine (Jne lbl))  -> jmpNone 0x0040 lbl cpu
    Just (InstrLine (Je  lbl))  -> jmpAll  0x0040 lbl cpu
    Just (InstrLine (Jge lbl))  -> jmpXnor jmpAll 0x0800 jmpAll 0x0080 lbl cpu
    Just (InstrLine (Jl  lbl))  -> jmpXor  jmpAll 0x0800 jmpAll 0x0080 lbl cpu
    Just (InstrLine (Jg  lbl))  -> jmpAnd  jmpNone 0x0040 (jmpXnor jmpAll 0x0800 jmpAll) 0x0080 lbl cpu
    Just (InstrLine (Jle lbl))  -> jmpOr   jmpAll  0x0040 (jmpXor  jmpAll 0x0800 jmpAll) 0x0080 lbl cpu
    Just (InstrLine (Jo  lbl))  -> jmpAll  0x0400 lbl cpu
    Just (InstrLine (Jc  lbl))  -> jmpAll  0x0001 lbl cpu
    Just (InstrLine (Jp  lbl))  -> jmpAll  0x0004 lbl cpu
    Just (InstrLine (Js  lbl))  -> jmpAll  0x0080 lbl cpu
    Just (InstrLine (Jno lbl))  -> jmpNone 0x0400 lbl cpu
    Just (InstrLine (Jnc lbl))  -> jmpNone 0x0001 lbl cpu
    Just (InstrLine (Jnp lbl))  -> jmpNone 0x0004 lbl cpu
    Just (InstrLine (Jns lbl))  -> jmpNone 0x0080 lbl cpu
    Just (InstrLine (Call lbl)) -> call lbl cpu
    Just (InstrLine (Push arg)) -> push arg cpu
    Just (InstrLine (Pop  reg)) -> pop reg cpu
    Just (InstrLine PushF) -> pushf cpu
    Just (InstrLine PopF)  -> popf cpu
    Just (InstrLine Ret) -> ret cpu
    Just (InstrLine (Msg msgs)) -> msg cpu msgs
    Just (InstrLine End) -> msg (cpu { terminated = True }) [StrMsg "0"]
    Just (LabelLine lbl) -> cpu
    Nothing              -> msg (cpu { terminated = True }) [StrMsg "-1"]
    where
      prg      = program cpu
      cpuFlags = flags cpu

data ShiftDir = LEFT | RIGHT

-- add memory addressing [address] in place of vars, and make regs have standard names
-- add sections.

rotate32 :: ShiftDir -> Reg -> Arg -> CPUState -> CPUState
rotate32 dir reg arg cpu = (mov reg (Val $ safe 32 shift + cout) cpu) {flags = flg}
  where
    argval = getArgVal arg cpu
    regval = toInteger $ getRegVal reg cpu
    flg = getFlags shift
    cout = flg B..&. 1
    dirnum = case dir of
              LEFT  ->  B.shiftL
              RIGHT ->  B.shiftR
    shift = dirnum regval $ fromIntegral argval

shift32 :: ShiftDir -> Reg -> Arg -> CPUState -> CPUState
shift32 dir reg arg cpu = (mov reg (Val $ safe 32 shift) cpu) {flags = flg}
  where
    argval = getArgVal arg cpu
    regval = toInteger $ getRegVal reg cpu
    flg = getFlags shift
    dirnum = case dir of
              LEFT  ->  B.shiftL
              RIGHT ->  B.shiftR
    shift = dirnum regval $ fromIntegral argval

safe :: (Integral a, B.Bits a) => Int -> a -> Val
safe n = fromIntegral . (B..&.) (B.shiftL 1 n - 1)

setFlags :: Reg -> CPUState -> CPUState
setFlags reg cpu = cpu {flags = getFlags $ getArgVal (Reg reg) cpu}

getFlags :: (Integral a, B.Bits a) => a -> Val
getFlags val = sum $ zipWith (*)
  (map (fromIntegral . fromEnum . ($ val)) flagFunc)
  (map B.bit [0x0.. 0xF])
  where
    flagFunc :: (Integral a, B.Bits a) => [a -> Bool]
    flagFunc = [ (0 /=) . (0x100000000 B..&.)
               , const False
               , (0 ==) . (1 B..&.) . B.popCount
               , const False
               , const False
               , const False
               , (0 ==)
               , (> 0) . (0x80000000 B..&.)
               , const False
               , const False
               , const False
               , const False
               ]

mul :: Arg -> CPUState -> CPUState
mul arg cpu = cpu'' {flags = setOV $ flags cpu''}
  where
    setOV = if prod > 0xFFFFFFFF
                then (0x800 B..|.)
                else id
    prod = regval cpu * argval cpu
    regval = toInteger . getRegVal "ax"
    argval = toInteger . getArgVal arg
    cpu'' = setFlags "ax" cpu'
    cpu' = mov "dx" (Val $ safe 32 (prod `B.shiftR` 32)) $ mov "ax" (Val $ safe 32 prod) cpu

add :: Reg -> Arg -> CPUState -> CPUState
add reg arg cpu = mov reg (Val $ getRegVal reg cpu'') $ cpu'' {flags = setOV $ flags cpu''}
  where
    setOV f = if cin /= cout then 0x400 B..|. f else f
    cin = ((regval cpu B..&. 0x7FFFFFFF) + (argval cpu B..&. 0x7FFFFFFF)) >= 0x80000000
    regval = toInteger . getRegVal reg
    argval = toInteger . getArgVal arg
    cout  = flags cpu'' B..&. 0x1 == 1
    cpu'' = setFlags reg cpu'
    cpu'  = adjRegWithArgUnsafe (+) reg arg cpu

sub :: Reg -> Arg -> CPUState -> CPUState
sub reg arg cpu = mov reg (Val $ regval cpu'') $ cpu'' {flags = setOV $ flags cpu''}
  where
    setOV f = if cin /= cout then 0x400 B..|. f else f
    cin = ((regval cpu B..&. 0x7FFFFFFF) + ((twoC . argval $ cpu) B..&. 0x7FFFFFFF)) >= 0x80000000
    regval = getRegVal reg
    argval = getArgVal arg
    cout = flags cpu'' B..&. 0x1 == 1
    cpu'' = setFlags reg cpu'
    cpu' = adjRegWithArgUnsafe subtract reg arg cpu

subSafe :: Reg -> Arg -> CPUState -> CPUState
subSafe reg arg cpu = add reg (Val newArg) cpu
  where
    newArg = twoC . (safe 32 . getArgVal arg) $ cpu

twoC :: Val -> Val
twoC v = B.complement v + 1

type CondJmp = Val -> Lbl -> CPUState -> CPUState

jmpNot :: CondJmp -> CondJmp
jmpNot jmp val lbl cpu = 
  if cpu' == cpu
    then jump lbl cpu
    else cpu
  where
    cpu' = jmp val lbl cpu

jmpNor :: CondJmp -> Val -> CondJmp -> CondJmp
jmpNor jmp0 val0 jmp1 = jmpNot $ jmpOr jmp0 val0 jmp1

jmpOr :: CondJmp -> Val -> CondJmp -> CondJmp
jmpOr jmp0 val0 jmp1 val1 lbl cpu =
  if (cpu0 /= cpu) || (cpu1 /= cpu)
    then jump lbl cpu
    else cpu
  where
    cpu0 = jmp0 val0 lbl cpu
    cpu1 = jmp1 val1 lbl cpu

jmpAnd :: CondJmp -> Val -> CondJmp -> CondJmp
jmpAnd jmp0 val0 jmp1 val1 lbl cpu =
  if (cpu0 /= cpu) && (cpu1 /= cpu)
    then jump lbl cpu
    else cpu
  where
    cpu0 = jmp0 val0 lbl cpu
    cpu1 = jmp1 val1 lbl cpu

jmpXor :: CondJmp -> Val -> CondJmp -> CondJmp
jmpXor jmp0 val0 jmp1 val1 lbl cpu =
  if (cpu0 /= cpu) /= (cpu1 /= cpu)
    then jump lbl cpu
    else cpu
  where
    cpu0 = jmp0 val0 lbl cpu
    cpu1 = jmp1 val1 lbl cpu

jmpXnor :: CondJmp -> Val -> CondJmp -> CondJmp
jmpXnor jmp0 val0 jmp1 = jmpNot $ jmpXor jmp0 val0 jmp1

jmpAll :: Val -> Lbl -> CPUState -> CPUState
jmpAll v lbl cpu
  | cpuFlags B..&. v == v = jump lbl cpu
  | otherwise             = cpu
  where
    cpuFlags = flags cpu

jmpNone :: Val -> Lbl -> CPUState -> CPUState
jmpNone v lbl cpu
  | cpuFlags B..&. v == 0 = jump lbl cpu
  | otherwise             = cpu
  where
    cpuFlags = flags cpu

jmpOne :: Val -> Lbl -> CPUState -> CPUState
jmpOne v lbl cpu
  | cpuFlags B..&. v /= 0 = jump lbl cpu
  | otherwise             = cpu
  where
    cpuFlags = flags cpu

mov :: Reg -> Arg -> CPUState -> CPUState
mov reg arg cpu =
  let cpu' = prepState cpu reg
  in  cpu' { registers = HM.adjust (const (getArgValSafe arg cpu)) reg $ registers cpu' }

pushf :: CPUState -> CPUState
pushf = push =<< Val . flags

popf :: CPUState -> CPUState
popf cpu = cpu {flags = flags'}
  where
    (flags', stack') = popOffStack $ stack cpu

push :: Arg -> CPUState -> CPUState
push arg cpu = cpu { stack = pushToStack (stack cpu) $ getArgVal arg cpu }

pop :: Reg -> CPUState -> CPUState
pop reg cpu = (mov reg (Val val) cpu) { stack = stack' }
  where
    (val, stack') = popOffStack $ stack cpu

msg :: CPUState -> [Msg] -> CPUState
msg cpu msgs = cpu { printLns = printLns cpu S.|> message }
  where
    message = concatMap (showMsg cpu) msgs
    showMsg cpu m =
      case m of
        UArgMsg ua -> show $ getArgVal ua cpu
        ArgMsg a -> show $ getSignedArgVal a cpu
        StrMsg s -> s

advance :: CPUState -> CPUState
advance cpu = cpu { currLine = currLine cpu + 1 }

ret :: CPUState -> CPUState
ret cpu = let (lastPlace, stackTail) = getCall $ callStack cpu
          in cpu { currLine  = lastPlace
                 , callStack = stackTail
                 }

getCall :: CallStack -> (Int, CallStack)
getCall st = 
  case uncons st of
    Just (hd, tl) -> (hd, tl)
    Nothing       -> error "Attempt to pop empty call stack."

pushToStack :: Stack -> Val -> Stack
pushToStack st v = v : st

popOffStack :: Stack -> (Val, Stack)
popOffStack st = 
  case uncons st of
    Just (hd, tl) -> (hd, tl)
    Nothing       -> error "Attempt to pop empty stack."

call :: Lbl -> CPUState -> CPUState
call lbl cpu = 
  case V.elemIndex (LabelLine lbl) prg of
    Just newLn -> cpu { currLine = newLn
                      , callStack = currLine cpu : callStack cpu
                      }
    Nothing    -> error "Call to nonexistant label"
  where
    prg = program cpu

jump :: Lbl -> CPUState -> CPUState
jump lbl cpu = 
  case V.elemIndex (LabelLine lbl) prg of
    Just newLn -> cpu { currLine = newLn }
    Nothing    -> error "Jump to nonexistant label"
  where
    prg = program cpu

comp :: Arg -> Arg -> CPUState -> CPUState
comp arg0 arg1 cpu =
  let
    v0 = getSignedArgVal arg0 cpu
    v1 = getSignedArgVal arg1 cpu
  in case compare v0 v1 of
    EQ -> cpu { flags = setClear (flags cpu) [6] [7]  }
    LT -> cpu { flags = setClear (flags cpu) [7] [6]  }
    GT -> cpu { flags = setClear (flags cpu) [] [6,7] }

setClear :: Val -> [Int] -> [Int] -> Val
setClear v [] []        = v
setClear v [] (cb:cbs)  = setClear (v `B.clearBit` cb) [] cbs
setClear v (sb:sbs) cbs = setClear (v `B.setBit` sb) sbs cbs

getRegVal :: Reg -> CPUState -> Val
getRegVal = getArgVal . Reg

getArgVal :: Arg -> CPUState -> Val
getArgVal arg cpu =
  case arg of
    Val v -> v
    Reg r ->
      let cpu' = prepState cpu r
      in registers cpu' HM.! r

getSignedArgVal :: Arg -> CPUState -> SVal
getSignedArgVal arg cpu = if isNeg 
                            then negate . fromIntegral $ twoC argval
                            else fromIntegral argval
  where
    argval = getArgValSafe arg cpu
    isNeg  = getFlags argval B..&. 0x0080 /= 0

getArgValSafe :: Arg -> CPUState -> Val
getArgValSafe arg cpu = (0xFFFFFFFF B..&.) $ getArgVal arg cpu

adjRegUnsafe :: (Val -> Val) -> Reg -> CPUState -> CPUState
adjRegUnsafe f reg cpu =
  let cpu' = prepState cpu reg
  in setFlags reg $ cpu' { registers = HM.adjust f reg $ registers cpu' }

adjReg :: (Val -> Val) -> Reg -> CPUState -> CPUState
adjReg f reg cpu =
  let cpu' = prepState cpu reg
  in setFlags reg $ cpu' { registers = HM.adjust ((B..&. 0xFFFFFFFF) . f) reg $ registers cpu' }

adjRegWithArg :: (Val -> Val -> Val) -> Reg -> Arg -> CPUState -> CPUState
adjRegWithArg f reg arg cpu = adjReg (f $ getArgVal arg cpu) reg cpu

adjRegWithArgUnsafe :: (Val -> Val -> Val) -> Reg -> Arg -> CPUState -> CPUState
adjRegWithArgUnsafe f reg arg cpu = adjRegUnsafe (f $ getArgVal arg cpu) reg cpu

prepState :: CPUState -> Reg -> CPUState
prepState cpu r =
  case HM.lookup r (registers cpu) of
    Just v  -> cpu
    Nothing -> cpu { registers = HM.insert r 0 (registers cpu) }