
-----------------------------------------------------------------------------------------------------------------------

module GC(gc) where

import Data.List(mapAccumL, foldl')

import TemplInst
import Utils

-----------------------------------------------------------------------------------------------------------------------

gcHeapSize :: Int
gcHeapSize = 0

markFrom :: TiHeap -> Addr -> (TiHeap, Addr)
markFrom heap addr =
   let
      node = hLookup heap addr
   in
      case node of
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
               (heapAfter, addrsAfter) = mapAccumL markFrom heap addrs
            in
               (hUpdate heapAfter addr (NMarked (NData tag addrsAfter)), addr)

         _ -> (hUpdate heap addr $ NMarked node, addr)


markFromStack :: TiHeap -> TiStack -> (TiHeap, TiStack)
markFromStack = mapAccumL markFrom


markFromDump :: TiHeap -> TiDump -> (TiHeap, TiDump)
markFromDump = mapAccumL markFromStack


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
gc state =
   let
      (heap1, stack1) = markFromStack (tiHeap state) $ tiStack state
      (heap2, dump1) = markFromDump heap1 $ tiDump state
      (heap3, globals1) = markFromGlobals heap2 $ tiGlobals state
      heap4 = scanHeap heap3
   in
      state { tiHeap = heap4, tiGlobals = globals1, tiDump = dump1, tiStack = stack1 }

-----------------------------------------------------------------------------------------------------------------------


