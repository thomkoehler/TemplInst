
-----------------------------------------------------------------------------------------------------------------------

module Evaluator(runProg, getResult) where

import Data.List(foldl')

import Utils
import Language
import Grammar(parse)

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
   | NInd Addr
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


runProg :: String -> [TiState]
runProg = eval . compile . parse


getResult :: [TiState] -> Int
getResult states = 
   let
      state = last states
      (addr : _) = tiStack state
      (NNum res) = hLookup (tiHeap state) addr
   in
      res
    
    


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
tiFinal (TiState [addr] heap _ _) = isDataNode $ hLookup heap addr
tiFinal (TiState [] _ _ _) = error "Empty stack!"
tiFinal _ = False


step :: TiState -> TiState
step state = dispatch $ hLookup (tiHeap state) $ head $ tiStack state
   where
      dispatch (NNum _) = error "Number applied as a function!"
      dispatch (NAp a1 _) = apStep state a1
      dispatch (NSupercomb _ argNames body) = scStep state argNames body
      dispatch (NInd addr) = indStep state addr 
      
      
indStep :: TiState -> Addr -> TiState
indStep state addr = state { tiStack = addr : tail (tiStack state) }
      
apStep :: TiState -> Addr -> TiState
apStep state addr1 = state { tiStack = addr1 : tiStack state }


scStep :: TiState -> [Name] -> CoreExpr -> TiState
scStep state argNames body = state
   {
      tiStack = indAddr : saveDrop (length argNames + 1) (tiStack state),
      tiHeap = newHeap'
   }
   where
      argBinding = zip argNames $ getArgs (tiHeap state) $ tiStack state
      env = aInsertList argBinding $ tiGlobals state
      (newHeap, resultAddr) = instantiate body (tiHeap state) env
      (newHeap', indAddr) = hAlloc newHeap $ NInd resultAddr   
      saveDrop size stack = if size > length stack
         then error "Applied to too few arguments"
         else drop size stack


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
  
instantiate (EVar name) heap env = (heap, aLookup env name (error ("Undefined name " ++ show name)))  
          
instantiate (ELet defs body) heap env = instantiate body heap' env'
   where
      (heap', env') = foldl' stepFun (heap, env) defs
      stepFun (h, e) (name, expr) =
         let
            (h', addr) = instantiate expr h env'
         in
            (h', aInsert name addr e)
      
instantiateAndUpdate
   :: CoreExpr -- Body of supercombinator
   -> Addr -- Address of node to update
   -> TiHeap -- Heap before instantiation
   -> ASSOC Name Addr -- Associate parameters to addresses
   -> TiHeap -- Heap after instantiation
   
instantiateAndUpdate (EAp e1 e2) updAddr heap env = hUpdate heap2 updAddr (NAp addr1 addr2)
   where
      (heap1, addr1) = instantiate e1 heap env
      (heap2, addr2) = instantiate e2 heap1 env

instantiateAndUpdate (EVar name) updAddr heap env = hUpdate heap updAddr $ NInd varAddr  
   where
      varAddr = aLookup env name (error ("Undefined name " ++ show name))

instantiateAndUpdate (ENum n) updAddr heap _ = hUpdate heap updAddr $ NNum n

   
     
   
-----------------------------------------------------------------------------------------------------------------------
