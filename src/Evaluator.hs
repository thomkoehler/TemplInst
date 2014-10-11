
-----------------------------------------------------------------------------------------------------------------------

module Evaluator(runProg, getResult) where

import Data.List(foldl')
import qualified Data.ByteString.Char8 as C

import Utils
import Language
import Parser(parse)

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
   deriving Show


data Primitive
   = Neg
   | Add
   | Sub
   | Mul
   | Div
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
doAdmin = id --TODO diAdmin


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


getArgNodes :: Int -> TiState -> [(Node, Addr, Addr)]
getArgNodes size state =
   if length stack <= size
      then error "Applied to too few arguments"
      else zip3 argNodes argAddrs apAddrs
   where
         stack = tiStack state
         heap = tiHeap state
         apAddrs = (take size) $ tail stack
         argAddrs = map (\(NAp _ addr) -> addr) $ map (hLookup heap) apAddrs
         argNodes = map (hLookup heap) argAddrs

{--

   let
      stack = tiStack state
      heap = tiHeap state
   in
      if length stack <= size
         then error "Applied to too few arguments"
         else  map (hLookup heap) $
               map (\(NAp _ addr) -> addr) $
               map (hLookup heap) $
               (take size) $
               tail stack

--}

primStep :: TiState -> Name -> Primitive -> TiState


primStep state _ Neg =
   let
      heap = tiHeap state
      stack = tiStack state
      [(argNode, argAddr, apAddr)] = getArgNodes 1 state
   in
      case argNode of
         (NNum n) -> state
            {
               tiStack = drop 1 stack,
               tiHeap = hUpdate heap apAddr $ NNum (- n)
            }
         _        -> state
            {
               tiStack = [argAddr],
               tiDump =  [apAddr] : tiDump state
            }


{--
primStep state _ Neg =
   let
      heap = tiHeap state
      stack = tiStack state
      (_ : apAddr : _) = stack
      (NAp _ argAddr) = hLookup heap apAddr
      argNode = hLookup heap argAddr
   in
      case argNode of
         (NNum n) -> state
            {
               tiStack = drop 1 stack,
               tiHeap = hUpdate heap apAddr $ NNum (- n)
            }
         _        -> state
            {
               tiStack = [argAddr],
               tiDump =  [apAddr] : tiDump state
            }
--}

primStep _ _ _ = undefined --TODO primStep :: TiState -> Name -> Primitive -> TiState

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

-----------------------------------------------------------------------------------------------------------------------
