
-----------------------------------------------------------------------------------------------------------------------

module Evaluator(runProg, getResult) where

import Data.List(foldl', zip4, mapAccumL)
import qualified Data.ByteString.Char8 as C

import Utils
import Language
import Parser(parse)

-----------------------------------------------------------------------------------------------------------------------

gcHeapSize :: Int
gcHeapSize = 0

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
   | NMarked Node
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


runProg :: C.ByteString -> [TiState]
runProg = eval . compile . parse "internal"

  

getResult :: [TiState] -> Int
getResult states =
   let
      state = last states
      (addr : _) = tiStack state
      (NNum res) = hLookup (tiHeap state) addr
   in
      res


compile :: [CoreScDefn] -> TiState
compile program = TiState initStack initHeap globals [] initStat
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
            (heap1, addr) = hAlloc heap (NSupercomb name args body)
            env1 = aInsert name addr env
            (heap2, env2) = allocatePrim heap1 env1 primitives
         in
            (heap2, env2)

allocatePrim :: TiHeap -> TiGlobals -> [(Name, Primitive)] -> (TiHeap, TiGlobals)
allocatePrim heap env = foldl' stepFun (heap, env)
   where
      stepFun (h, e) (name, p) = (h', e')
         where
            (h', addr) = hAlloc h (NPrim name p)
            e' = aInsert name addr e


primitives :: [(Name, Primitive)]
primitives =
   [
      ("neg", Neg),
      ("+", Add),
      ("-", Sub),
      ("*", Mul),
      ("/", Div)
   ]

eval :: TiState -> [TiState]
eval state = state : restStates
   where
      nextState = doAdmin $ step state

      restStates
         | tiFinal state = []
         | otherwise     = eval nextState


doAdmin :: TiState -> TiState
doAdmin = gc


tiFinal :: TiState -> Bool
tiFinal (TiState [addr] heap _ [] _) = isDataNode $ hLookup heap addr
tiFinal (TiState [] _ _ _ _) = error "Empty stack!"
tiFinal _ = False


step :: TiState -> TiState
step state = dispatch $ hLookup (tiHeap state) nodeAddr
   where
      nodeAddr = head $ tiStack state
      dispatch (NNum _) = numStep state
      dispatch (NAp a1 a2) = apStep state nodeAddr a1 a2
      dispatch (NSupercomb _ argNames body) = scStep state argNames body
      dispatch (NInd addr) = indStep state addr
      dispatch (NPrim name prim) = primStep state name prim


numStep :: TiState -> TiState
numStep state =
   case tiDump state of
      []               -> error "Number applied as a function!" 
      (stack' : dump') -> state { tiStack = stack', tiDump = dump' }

      
getArgData :: Int -> TiState -> [(Addr, Addr, Addr, Node)]
getArgData size state = 
   if length stack <= size
      then error "Applied to too few arguments"
      else zip4 apAddrs leftAddrs rightAddrs rightNodes
   where
      stack = tiStack state
      heap = tiHeap state
      apAddrs = take size $ tail stack
      leftAddrs = map ((\ (NAp addr _) -> addr) . hLookup heap) apAddrs
      rightAddrs = map ((\ (NAp _ addr) -> addr) . hLookup heap) apAddrs
      rightNodes = map (hLookup heap) rightAddrs

      
pushNonDataNodeToStack :: [(Addr, Addr, Addr, Node)] -> TiState -> (TiState, Bool)
pushNonDataNodeToStack argNodes state = 
   case filter (\(_, _, _, node) -> not (isDataNode node)) argNodes of
      [] -> (state, False)
      (_, _, argAddr, _):_ -> (state { tiDump = tiStack state : tiDump state, tiStack = [argAddr] }, True) 

      
resolveIndirections :: Int -> TiState -> TiState
resolveIndirections size state = foldl' step state args
   where
      step oldState (apAddr, leftAddr, rightAddr, NInd addr) = oldState { tiHeap = hUpdate (tiHeap oldState) apAddr (NAp leftAddr addr) }
      step oldState _ = oldState
      
      args = getArgData size state


primUnary :: TiState -> (Node -> Node) -> TiState
primUnary state unaryFun = if nonDataNodeFound
   then state2
   else state1
      {
            tiStack = drop 1 stack,
            tiHeap = hUpdate heap apAddr $ unaryFun argNode
      }
   where
      state1 = resolveIndirections 1 state
      heap = tiHeap state1
      stack = tiStack state1
      argNodes@[(apAddr, _, _, argNode)] = getArgData 1 state1
      (state2, nonDataNodeFound) = pushNonDataNodeToStack argNodes state1


primBinary :: TiState -> (Node -> Node -> Node) -> TiState
primBinary state binaryFun = if nonDataNodesFound
   then state2
   else state1
      {
            tiStack = drop 2 stack,
            tiHeap = hUpdate heap apAddr1 $ binaryFun argNode0 argNode1
      }
   where
      state1 = resolveIndirections 2 state
      heap = tiHeap state1
      stack = tiStack state1
      argNodes@[(_, _, _, argNode0), (apAddr1, _, _, argNode1)] = getArgData 2 state1
      (state2, nonDataNodesFound) = pushNonDataNodeToStack argNodes state1


primStep :: TiState -> Name -> Primitive -> TiState

primStep state _ Neg = primUnary state $ \(NNum i) -> (NNum (- i))
primStep state _ Add = primBinary state $ \(NNum x) (NNum y) -> (NNum (x + y))
primStep state _ Sub = primBinary state $ \(NNum x) (NNum y) -> (NNum (x - y))
primStep state _ Mul = primBinary state $ \(NNum x) (NNum y) -> (NNum (x * y))
primStep state _ Div = primBinary state $ \(NNum x) (NNum y) -> (NNum (x `div` y))

primStep state _ (PrimConstr tag size) = state
   {
      tiStack = drop size stack,
      tiHeap = hUpdate (tiHeap state) (head stack) $ NData tag args
   }
   where
      stack = tiStack state
      args = map (\(_, _, argAddr, _) -> argAddr) $ getArgData size state


indStep :: TiState -> Addr -> TiState
indStep state addr = state { tiStack = addr : tail (tiStack state) }

apStep :: TiState -> Addr -> Addr -> Addr -> TiState
apStep state addr addr1 addr2 =
   case hLookup (tiHeap state) addr2 of
      (NInd addr3) -> state { tiHeap = hUpdate (tiHeap state) addr (NAp addr1 addr3) }
      _            -> state { tiStack = addr1 : tiStack state }


scStep :: TiState -> [Name] -> CoreExpr -> TiState
scStep state argNames body = state
   {
      tiStack = addrSc : saveDrop (length argNames + 1) stack,
      tiHeap = newHeap
   }
   where
      argBinding = zip argNames $ getArgs (tiHeap state) $ tiStack state
      env = aInsertList argBinding $ tiGlobals state
      stack =  tiStack state
      addrSc = stack !! length argNames
      newHeap = instantiateAndUpdate body addrSc (tiHeap state) env


saveDrop :: Int -> [a] -> [a]
saveDrop size lst = if size > length lst
   then error "Applied to too few arguments"
   else drop size lst


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

instantiateAndUpdate :: CoreExpr -> Addr -> TiHeap -> ASSOC Name Addr -> TiHeap
instantiateAndUpdate (EAp e1 e2) updAddr heap env = hUpdate heap2 updAddr (NAp addr1 addr2)
   where
      (heap1, addr1) = instantiate e1 heap env
      (heap2, addr2) = instantiate e2 heap1 env

instantiateAndUpdate (EVar name) updAddr heap env = hUpdate heap updAddr $ NInd varAddr
   where
      varAddr = aLookup env name (error ("Undefined name " ++ show name))

instantiateAndUpdate (ENum n) updAddr heap _ = hUpdate heap updAddr $ NNum n

instantiateAndUpdate (ELet defs body) updAddr heap env = instantiateAndUpdate body updAddr heap1 env1
   where
      (heap1, env1) = foldl' stepFun (heap, env) defs
      stepFun (h, e) (name, expr) =
         let
            (h', addr) = instantiate expr h env1
         in
            (h', aInsert name addr e)

            
findStackRoots :: TiStack -> [Addr]
findStackRoots = id

findDumpRoots :: TiDump -> [Addr]
findDumpRoots = concat . map findStackRoots

findGlobalRoots :: TiGlobals -> [Addr]
findGlobalRoots = aRange

findRoots :: TiState -> [Addr]
findRoots state = findDumpRoots (tiDump state) ++ findGlobalRoots (tiGlobals state)

markFrom :: TiHeap -> Addr -> (TiHeap, Addr)
markFrom heap addr = 
   let
      node = hLookup heap addr
   in
      case hLookup heap addr of
         NMarked _ -> (heap, addr)
         
         NAp addrLeft addrRight -> 
            let
               (heap1, addrLeft1) = markFrom heap addrLeft
               (heap2, addrRight1) = markFrom heap1 addrRight
            in
               (hUpdate heap2 addr (NMarked (NAp addrLeft1 addrRight1)), addr)
               
         NInd addr0 -> markFrom heap addr0
               
         NData tag addrs ->
            let
               (heapAfter, addrsAfter) = foldl' step (heap, []) addrs
               
               step :: (TiHeap, [Addr]) -> Addr -> (TiHeap, [Addr])
               step (heap, addrs) addr = (heap', addrs ++ [addr'])
                  where
                     (heap', addr') = markFrom heap addr
               
            in
               (hUpdate heapAfter addr (NMarked (NData tag addrsAfter)), addr)
               
         _ -> (hUpdate heap addr $ NMarked node, addr)
            
            
markFromStack :: TiHeap -> TiStack -> (TiHeap, TiStack)
markFromStack heap stack = foldl' step (heap, []) stack
   where
      step (heapBefore, newStack) addr = (heapAfter, newStack ++ [addrAfter])
         where
            (heapAfter, addrAfter) = markFrom heap addr

            
markFromDump :: TiHeap -> TiDump -> (TiHeap, TiDump)
markFromDump heap dump = foldl' step (heap, []) dump
   where
      step (heapBefore, newDump) stack = (heapAfter, newDump ++ [stackAfter])
         where
            (heapAfter, stackAfter) = markFromStack heapBefore stack
   
   
   
--TODO foldl' to => mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
   
   
markFromGlobals :: TiHeap -> TiGlobals -> (TiHeap,TiGlobals)
markFromGlobals heap globals = 
   let
      (names, addrs) = unzip $ aToList globals
      (heap', addrs') = mapAccumL markFrom heap addrs
   in
      (heap', aFromList (zip names addrs'))
      
            
            
scanHeap :: TiHeap -> TiHeap
scanHeap heap = foldl' step heap (hAdresses heap)
   where
      step heap addr = 
         case hLookup heap addr of
            NMarked node -> hUpdate heap addr node
            
            _ -> hFree heap addr
      
      
gc :: TiState -> TiState
gc state = if hSize heap > gcHeapSize
   then state { tiHeap = scanHeap heap1 }
   else state
   where
      heap = tiHeap state
      heap1 = foldl' markFrom heap $ findRoots state
            
-----------------------------------------------------------------------------------------------------------------------
