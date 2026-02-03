module AST
    ( Expr(..)
    , Pattern(..)
    , Type(..)
    , Decl(..)
    , Constructor(..)
    ) where

data Expr
    = IntLit Integer
    | BoolLit Bool
    | Var String
    | Lambda String Expr
    | Apply Expr Expr
    | ListLit [Expr]
    | Nil
    | Cons Expr Expr
    | ConstructorExpr String [Expr]
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | GreaterThan Expr Expr
    | If Expr Expr Expr
    | Match Expr Pattern Expr Pattern Expr
    | Let String Expr Expr
    | LetRec String Expr Expr
    | Print Expr
    deriving (Show, Eq)

data Pattern
    = PNil
    | PCons String String
    | PCtor String [String]
    deriving (Show, Eq)

data Type
    = TInt
    | TBool
    | TVar String
    | TFunc Type Type
    | TList Type
    | TCon String [Type]
    | TUnit
    deriving (Show, Eq)

data Decl
    = TypeDecl String [String] [Constructor]
    deriving (Show, Eq)

data Constructor = Constructor
    { ctorName :: String
    , ctorArgs :: [Type]
    } deriving (Show, Eq)
