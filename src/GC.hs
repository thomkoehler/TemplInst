
-----------------------------------------------------------------------------------------------------------------------

module GC(gc) where

import Data.List(mapAccumL, foldl')

import TemplInst
import Utils

-----------------------------------------------------------------------------------------------------------------------

gcHeapSize :: Int
gcHeapSize = 0

markFrom :: TiHeap -> Addr -> (TiHeap, Addr)
markFrom h a = go a 0 h
   where
      go fAddr bAddr heap =
         let
            fNode = hLookup heap fAddr
            bNode = hLookup heap bAddr
         in
            case (fNode, bAddr, bNode) of
               (NMarked Done _, 0, _) -> (heap, fAddr)

               (NAp addr1 addr2, _, _) -> go addr1 fAddr $ hUpdate heap fAddr $ NMarked (Visit 1) (NAp bAddr addr2)

               (NPrim _ _, _, _) -> go fAddr bAddr $ hUpdate heap fAddr $ NMarked Done fNode

               (NNum _, _, _) -> go fAddr bAddr $ hUpdate heap fAddr $ NMarked Done fNode

               (NSupercomb{}, _, _) -> go fAddr bAddr $ hUpdate heap fAddr $ NMarked Done fNode

               (NMarked Done _, _, NMarked (Visit 1) (NAp b' addr2)) ->
                  go addr2 bAddr $ hUpdate heap bAddr $ NMarked (Visit 2) (NAp fAddr b')

               (NMarked Done _, _, NMarked (Visit 2) (NAp addr1 b')) ->
                  go bAddr b' $ hUpdate heap bAddr $ NMarked Done (NAp addr1 fAddr)

               (NInd addr, _, _) -> go addr bAddr heap



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
scanHeap tih = foldl' step tih (hAdresses tih)
   where
      step heap addr =
         case hLookup heap addr of
            NMarked _ node -> hUpdate heap addr node

            _ -> hFree heap addr


gc :: TiState -> TiState
gc state =
   let
      heap = tiHeap state
      (heap1, stack1) = markFromStack heap $ tiStack state
      (heap2, dump1) = markFromDump heap1 $ tiDump state
      (heap3, globals1) = markFromGlobals heap2 $ tiGlobals state
      heap4 = scanHeap heap3
   in
      if hSize heap > gcHeapSize
         then state { tiHeap = heap4, tiGlobals = globals1, tiDump = dump1, tiStack = stack1 }
         else state

-----------------------------------------------------------------------------------------------------------------------


