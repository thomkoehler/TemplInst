-----------------------------------------------------------------------------------------------------------------------

module TemplInst where


import Utils
import Language

-----------------------------------------------------------------------------------------------------------------------

type TiStack = [Addr]
type TiHeap = Heap Node
type TiGlobals = ASSOC Name Addr
type TiDump = [TiStack]


data TiStats = TiStats deriving Show --TODO TiStats


data TiState = TiState
   {
      tiStack :: TiStack,
      tiHeap :: TiHeap,
      tiGlobals :: TiGlobals,
      tiDump :: TiDump,
      tiStats :: TiStats
   }
   deriving Show


data Node
   = NAp Addr Addr
   | NSupercomb Name [Name] CoreExpr
   | NNum Int
   | NInd Addr
   | NPrim Name Primitive
   | NData Int [Addr]
   | NMarked MarkState Node
   deriving Show

   
data MarkState 
   = Done
   | Visit Int
   deriving Show


data Primitive
   = Neg
   | Add
   | Sub
   | Mul
   | Div
   | PrimConstr Int Int
   deriving Show


isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode  _ = False

isIndNode :: Node -> Bool
isIndNode (NInd _ ) = True
isIndNode _ = False

-----------------------------------------------------------------------------------------------------------------------



