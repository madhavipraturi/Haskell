import Data.Maybe (fromJust)
import Control.Monad


data Type = IntTy
          | TBool
		  | TNat
		  | ArrowTy Type Type
	deriving (Show, Eq)
	

data Term = Const Int
		  | Var String
		  | Abs String Type Term
		  | App Term Term
		  | Tr
		  | Fl
		  | IsZero Term
		  | Succ Term
		  | Pred Term
		  | If Term Term Term
		  | Zero
	deriving (Show, Eq)
	
	
substitute :: String -> Term -> Term -> Term
substitute v x (Var v') | v' == v = x
substitute v x (Abs arg ty t) = Abs arg ty (substitute v x t)
substitute v x (App t arg) = App (substitute v x t) arg
substitute _ _ t = t


eval :: [(String, Term)] -> Term ->Maybe Term
eval _ (Const x) = Const x
eval _ x@Abs{} = x
eval e (Var x) = Const (let Const v = fromJust (lookup x e) in v)
eval e (App (Abs arg _ t) x) = substitute arg x t
eval e (App t x) = eval e (App (eval e t) x)

