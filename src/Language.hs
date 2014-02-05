-----------------------------------------------------------------------------------------------------------------------

module Language where

-----------------------------------------------------------------------------------------------------------------------

data Expr a
   = EVar !Name
   | ENum !Int
   | EConstr !Int !Int
   | EConst !Int !Int
   | EAp (Expr a) (Expr a)
   | ELet Bool [(a, Expr a)] (Expr a)
   | ECase (Expr a) [Alter a]
   | ELam [a] (Expr a)
   deriving (Show, Eq)

   
type Name = String

type Alter a = (Int, [a], Expr a)

type Program a  =  [ScDefn  a]

type CoreProgram  = Program Name

data ScDefn a = ScDefn
   {
      scName :: Name,
      scArgs :: [a],
      scExpr :: Expr a
   }
   deriving (Show, Eq)

type CoreScDefn = ScDefn  Name

type CoreAlter = Alter Name

type CoreExpr = Expr Name

-----------------------------------------------------------------------------------------------------------------------
