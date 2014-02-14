
{-# LANGUAGE NoMonomorphismRestriction #-}

module Utils where

import qualified Data.Map as Map
import Data.Maybe(fromMaybe)


type Addr = Int

data Heap a = Heap (Map.Map Addr a) Addr deriving Show

hInitial :: Heap a
hInitial = Heap Map.empty 1

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (Heap heapMap newAddr) x =
   (Heap (Map.insert newAddr x heapMap) (newAddr + 1), newAddr)


hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (Heap heapMap newAddr) addr x = Heap (Map.insert addr x heapMap) newAddr


hFree :: Heap a -> Addr -> Heap a
hFree (Heap heapMap newAddr) addr = Heap (Map.delete addr heapMap) newAddr


hLookup :: Heap a -> Addr -> a
hLookup (Heap heapMap _) addr = 
   fromMaybe (error $ "Heap address " ++ show addr ++ " is unknown.") $ Map.lookup addr heapMap

hAdresses :: Heap a -> [Addr]
hAdresses (Heap heapMap _) = Map.keys heapMap


hSize :: Heap a -> Int
hSize (Heap heapMap _) = Map.size heapMap


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

aMap :: (a -> b) -> ASSOC k a -> ASSOC k b
aMap = Map.map

aLenght :: ASSOC k a -> Int
aLenght = length . Map.toList

aFromList :: Ord k => [(k, x)] -> ASSOC k x
aFromList = Map.fromList


------------------------------------------------------------------------------------------------------------------------
