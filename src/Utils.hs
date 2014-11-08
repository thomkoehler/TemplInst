
-----------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}

module Utils where

import Data.IntMap.Strict(IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map as Map
import Data.Maybe(fromMaybe)
import Data.List(foldl')

-----------------------------------------------------------------------------------------------------------------------

type Addr = Int

data Heap a = Heap (IntMap a) Addr deriving Show

hInitial :: Heap a
hInitial = Heap IntMap.empty 1

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (Heap heapMap newAddr) x =
   (Heap (IntMap.insert newAddr x heapMap) (newAddr + 1), newAddr)


hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (Heap heapMap newAddr) addr x = Heap (IntMap.insert addr x heapMap) newAddr


hFree :: Heap a -> Addr -> Heap a
hFree (Heap heapMap newAddr) addr = Heap (IntMap.delete addr heapMap) newAddr


hLookup :: Heap a -> Addr -> a
hLookup (Heap heapMap _) addr =
   fromMaybe (error $ "Heap address " ++ show addr ++ " is unknown.") $ IntMap.lookup addr heapMap

hAdresses :: Heap a -> [Addr]
hAdresses (Heap heapMap _) = IntMap.keys heapMap


hSize :: Heap a -> Int
hSize (Heap heapMap _) = IntMap.size heapMap


hNull :: Addr
hNull = 0


hIsNull :: Addr -> Bool
hIsNull = (== hNull)


showaddr :: Addr -> String
showaddr = show


type ASSOC a b = Map.Map a b

aLookup :: Ord k => ASSOC k x -> k -> x -> x
aLookup assoc key def =
   case Map.lookup key assoc of
      Just x -> x
      _      -> def

aDomain :: ASSOC k x -> [k]
aDomain = Map.keys

aRange :: ASSOC k x -> [x]
aRange = Map.elems

aEmpty :: ASSOC k x
aEmpty = Map.empty

aInsert :: Ord k => k -> x -> ASSOC k x -> ASSOC k x
aInsert = Map.insert

aInsertList :: Ord k => [(k, x)] -> ASSOC k x -> ASSOC k x
aInsertList kxs m = foldl' step m kxs
   where
      step m0 (k, x) = aInsert k x m0

aToList :: ASSOC k x -> [(k, x)]
aToList = Map.toList

aFromList :: Ord k => [(k, x)] -> ASSOC k x
aFromList = Map.fromList

-----------------------------------------------------------------------------------------------------------------------
