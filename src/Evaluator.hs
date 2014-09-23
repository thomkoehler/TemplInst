
-----------------------------------------------------------------------------------------------------------------------

module Evaluator where

import Data.List(foldl')

import Utils
import Language

-----------------------------------------------------------------------------------------------------------------------

type TiStack = [Addr]
type TiHeap = Heap Node
type TiGlobals = ASSOC Name Addr


data TiStats = TiStats deriving Show --TODO TiStats 


data TiState = TiState
   {
      tiStack :: TiStack,
      tiHeap :: TiHeap,
      tiGlobals :: TiGlobals,
      tiStats :: TiStats  
   } 
   deriving Show


data Node 
   = NAp Addr Addr
   | NSupercomb Name [Name] CoreExpr 
   | NNum Int
   deriving Show
   

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode  _ = False


{--

Ix = x;
K x y = x;
K1 x y = y ;
S f g x = f x (g x) ;
compose f g x = f (g x) ;
twice f = compose f f ;

--}

preludeDefs :: [CoreScDefn]
preludeDefs = 
   [
      ScDefn "I" ["x"] (EVar "x"),
      ScDefn "K" ["x", "y"] (EVar "x"),
      ScDefn "K1" ["x", "y"] (EVar "y"),
      ScDefn "S" ["f", "g", "x"] (EAp (EAp (EVar "f") (EVar "x"))(EAp (EVar "g") (EVar "x"))),
      ScDefn "compose" ["f", "g", "x"] (EAp (EVar "f") (EAp (EVar "g") (EVar "x"))),
      ScDefn "twice" ["f"] (EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
   ]


extraPreludeDefs :: [CoreScDefn]
extraPreludeDefs = []


runProg = eval . compile . parse


parse = id --TODO parse


compile :: [CoreScDefn] -> TiState
compile program = TiState initStack initHeap globals initStat
   where
      scDefs = program ++ preludeDefs ++ extraPreludeDefs
      (initHeap, globals) = buildInitialHeap scDefs
      mainAddress = aLookup globals "main" (error "main is not defined")
      initStack = [mainAddress]
      initStat = TiStats


buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap = foldl' stepFun (hInitial, aEmpty)
   where
      stepFun (heap, env) (ScDefn name args body) = 
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
doAdmin = id --TODO diAdmin


tiFinal :: TiState -> Bool
tiFinal state = isDataNode $ hLookup (tiHeap state) addr
   where
      [addr] = tiStack state


step :: TiState -> TiState
step state = dispatch $ hLookup (tiHeap state) $ head $ tiStack state
   where
      dispatch (NNum _) = error "Number applied as a function!"
      dispatch (NAp a1 _) = apStep state a1
      dispatch (NSupercomb _ argNames body) = scStep state argNames body 
      
      
apStep :: TiState -> Addr -> TiState
apStep state addr1 = state { tiStack = addr1 : tiStack state }


scStep :: TiState -> [Name] -> CoreExpr -> TiState
scStep state argNames body = state
   {
      tiStack = resultAddr : drop (length argNames + 1) (tiStack state),
      tiHeap = newHeap
   }
   where
      argBinding = zip argNames $ getArgs (tiHeap state) $ tiStack state
      env = aInsertList argBinding $ tiGlobals state
      (newHeap, resultAddr) = instantiate body (tiHeap state) env


getArgs :: TiHeap -> TiStack -> [Addr]
getArgs  heap (_ : stack) = map getArg stack
   where
      getArg addr = arg
         where
            (NAp _ arg) = hLookup heap addr
         


instantiate :: CoreExpr -> TiHeap -> ASSOC Name Addr -> (TiHeap, Addr)
instantiate (ENum n) heap _ = hAlloc heap (NNum n)

instantiate (EAp expr1 expr2) heap env = hAlloc heap2 (NAp addr1 addr2)
   where
      (heap1, addr1) = instantiate expr1 heap env
      (heap2, addr2) = instantiate expr2 heap1 env
  
instantiate (EVar v) heap env = (heap, aLookup env v (error ("Undefined name " ++ show v)))  

-----------------------------------------------------------------------------------------------------------------------
