
-----------------------------------------------------------------------------------------------------------------------

module Evaluator where

import Data.List(foldl')

import Utils
import Language

-----------------------------------------------------------------------------------------------------------------------

type TiStack = [Addr]
type TiHeap = Heap Node
type TiGlobals = ASSOC Name Addr


data TiStats = TiStats --TODO TiStats 


data TiState = TiState
   {
      tiStack :: TiStack,
      tiHeap :: TiHeap,
      tiGlobals :: TiGlobals,
      tiStats :: TiStats  
   } 


data Node 
   = NAp Addr Addr
   | NSupercomb Name [Name] CoreExpr 
   | NNum Int
   

preludeDefs :: [CoreScDefn]
preludeDefs = []

extraPreludeDefs :: [CoreScDefn]
extraPreludeDefs = []


compile :: [CoreScDefn] -> TiState
compile program = TiState initStack initHeap globals initStat
   where
      scDefs = program ++ preludeDefs ++ extraPreludeDefs
      (initHeap, globals) = buildInitialHeap scDefs
      mainAddress = aLookup globals "main" (error "main is not defined")
      initStack = [mainAddress]
      initStat = TiStats


buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap = foldl' step (hInitial, aEmpty)
   where
      step (heap, env) (ScDefn name args body) = 
         let
            (heap', addr) = hAlloc heap (NSupercomb name args body)
            env' = aInsert name addr env
         in
            (heap', env')


eval :: TiState -> [TiState]
eval state = state : restStates
   where
      nextState = doAdmin $ step state
      
      restStates 
         | tiFinal state = []
         | otherwise     = eval nextState


doAdmin :: TiState -> TiState
doAdmin = undefined

tiFinal :: TiState -> Bool
tiFinal = undefined

step :: TiState -> TiState
step = undefined 


instantiate :: CoreExpr -> TiHeap -> ASSOC Name Addr -> (TiHeap, Addr)
instantiate (ENum n) heap _ = hAlloc heap (NNum n)

instantiate (EAp expr1 expr2) heap env = hAlloc heap2 (NAp addr1 addr2)
   where
      (heap1, addr1) = instantiate expr1 heap env
      (heap2, addr2) = instantiate expr2 heap1 env
  
instantiate (EVar v) heap env = (heap, aLookup env v (error ("Undefined name " ++ show v)))  

-----------------------------------------------------------------------------------------------------------------------
