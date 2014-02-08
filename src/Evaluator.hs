
------------------------------------------------------------------------------------------------------------------------

module Evaluator(runProg) where


import Utils
import Language

import Data.List(mapAccumL)

------------------------------------------------------------------------------------------------------------------------

data GmState = GmState
   { 
      gmCode :: GmCode, 
      gmStack :: GmStack, 
      gmHeap :: GmHeap, 
      gmGlobals :: GmGlobals, 
      gmStats :: GmStats
   }

type GmCode = [Instruction]


data Instruction 
   = Unwind
   | Pushglobal Name
   | Pushint Int
   | Push Int
   | Mkap 
   | Slide Int
   
   
instance Eq Instruction where
   Unwind == Unwind = True
   Pushglobal a == Pushglobal b = a == b
   Pushint a == Pushint b = a == b
   Push a == Push b = a == b
   Mkap == Mkap = True
   Slide a == Slide b = a == b
   _ == _ = False


type GmStack = [Addr]


type GmHeap = Heap Node


data Node 
   = NNum Int
   | NAp Addr Addr
   | NGlobal Int GmCode

   
type GmGlobals = ASSOC Name Addr



type GmStats = ()
  
runProg :: String -> String
runProg = showResults . eval . compile . parse


eval :: GmState -> [GmState]
eval state = state : restStates
   where
      restStates
         | gmFinal state = []
         | otherwise     = eval nextState
         
      nextState = doAdmin (step state)
      
doAdmin = id


gmFinal :: GmState -> Bool
gmFinal = null . gmCode


step :: GmState -> GmState
step state = dispatch i $ state { gmCode = is }
   where
      (i:is) = gmCode state
      
dispatch :: Instruction -> GmState -> GmState
dispatch (Pushglobal f) = pushglobal f
dispatch (Pushint n) = pushint n
dispatch Mkap = mkap
dispatch (Push n) = push n
dispatch (Slide n) = slide n
dispatch Unwind = unwind


pushglobal :: Name -> GmState -> GmState
pushglobal name state =
   let
      a = aLookup (gmGlobals state) name (error ("Undeclared global " ++ name))
   in
      state {gmStack = a : gmStack state}


pushint :: Int -> GmState -> GmState
pushint n state =
   let
      (heap', a) = hAlloc (gmHeap state) (NNum n)
   in
      state {gmHeap = heap', gmStack = a : gmStack state}
    
      
mkap :: GmState -> GmState
mkap state = 
   let
      (a1:a2:as) = gmStack state
      (heap', a) = hAlloc (gmHeap state) (NAp a1 a2)
   in
      state {gmHeap = heap', gmStack = a:as}
      
  
push :: Int -> GmState -> GmState
push n state =
   let
      stack = gmStack state
      a = getArg $ hLookup (gmHeap state) $ stack !! (n + 1)
   in
      state { gmStack = a:stack }

      
getArg :: Node -> Addr
getArg (NAp _ a2) = a2


slide :: Int -> GmState -> GmState
slide n state = 
   let
      (a:as) = gmStack state
   in
      state { gmStack = a : drop n as }
      
      
unwind :: GmState -> GmState
unwind state = 
   let
       (a:as) = gmStack state
       heap = gmHeap state
       newState (NNum _) = state
       newState (NAp a1 _) = state { gmCode = [Unwind], gmStack = a1:a:as }
       newState (NGlobal n c)
         | length as < n = error "Unwinding with too few arguments"
         | otherwise = state { gmCode = c }
   in
      newState $ hLookup heap a


compile :: CoreProgram -> GmState
compile program =
   let
      (heap, globals) = buildInitialHeap program
   in
      GmState initialCode [] heap globals ()  


buildInitialHeap :: CoreProgram -> (GmHeap, GmGlobals)
buildInitialHeap program = 
   let
      compiled = map compileSc program
   in
      mapAccumL allocateSc hInitial compiled


type GmCompiledSC = (Name, Int, GmCode)

allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
allocateSc heap (name, nargs, code) =
   let
      (heap', addr) = hAlloc heap $ NGlobal nargs code
   in
      (heap', (name, addr))


initialCode :: GmCode
initialCode = [Pushglobal "main", Unwind]


compileSc :: (Name, [Name], CoreExpr) -> GmCompiledSC
compileSc (name, env, body) = (name, length env, compileR body (zip env [0..]))


type GmEnvironment = ASSOC Name Int

compileR :: CoreExpr -> GmEnvironment -> GmCode
compileR expr env = compileC expr env ++ [Slide (length env + 1), Unwind]


compileC :: CoreExpr -> GmEnvironment -> GmCode
compileC (EVar v) env
   | elem v (aDomain env) = [Push n]
   | otherwise            = [Pushglobal v]
   where
      n = aLookup env v (error "Can't happen")
      
compileC (ENum num) _ = [Pushint num]

compileC (EAp e1 e2) env = compileC e2 env ++ compileC e1 (argOffset 1 env) ++ [Mkap]


argOffset :: Int -> GmEnvironment -> GmEnvironment  
argOffset n env = [(v, n + m) | (v,m) <- env]



      

--TODO parse 
parse = undefined

--TODO showResults
showResults = undefined



------------------------------------------------------------------------------------------------------------------------
