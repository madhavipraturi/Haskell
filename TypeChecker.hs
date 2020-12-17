data Expr
  = Tr
  | Fl
  | IsZero Expr
  | Succ Expr
  | Pred Expr
  | If Expr Expr Expr
  | Zero


  Type Env = [(Name, Type)]

extend :: (Name, Type) -> Env -> Env
extend xt env = xt : env

inEnv :: (Name, Type) -> Check a -> Check a
inEnv (x,t) = local (extend (x,t))

lookupVar :: Name -> Check Type
lookupVar x = do
  env <- ask
  case lookup x env of
    Just e  -> return e
    Nothing -> throwError $ NotInScope x

data TypeError
  = Mismatch Type Type
  | NotFunction Type
  | NotInScope Name

type Check = ExceptT TypeError (Reader Env)


check :: Expr -> Check Type
check expr = case expr of

  Lit (LInt{}) -> return TInt

  Lit (LBool{}) -> return TBool

  Lam x t e -> do
    rhs <- inEnv (x,t) (check e)
    return (TArr t rhs)

  App e1 e2 -> do
    t1 <- check e1
    t2 <- check e2
    case t1 of
       (TArr a b) | a == t2 -> return b
       (TArr a _) -> throwError $ Mismatch t2 a
       ty -> throwError $ NotFunction ty

  Var x -> lookupVar x

  nf :: Expr -> Expr
nf t = fromMaybe t (nf <$> eval1 t)

eval :: Expr -> Maybe Expr
eval t = case isVal (nf t) of
  True  -> Just (nf t)
  False -> Nothing -- term is "stuck"


  isVal :: Expr -> Bool
isVal Tr = True
isVal Fl = True
isVal t | isNum t = True
isVal _ = False

-- Evaluate a single step.
eval1 :: Expr -> Maybe Expr
eval1 expr = case expr of
  Succ t                    -> Succ <$> (eval1 t)
  Pred Zero                 -> Just Zero
  Pred (Succ t) | isNum t   -> Just t
  Pred t                    -> Pred <$> (eval1 t)
  IsZero Zero               -> Just Tr
  IsZero (Succ t) | isNum t -> Just Fl
  IsZero t                  -> IsZero <$> (eval1 t)
  If Tr  c _                -> Just c
  If Fl _ a                 -> Just a
  If t c a                  -> (\t' -> If t' c a) <$> eval1 t
  _                         -> Nothing

  data Type 
  = TBool
  | TNat

  type Check a = Except TypeError a

data TypeError
  = TypeMismatch Type Type

check :: Expr -> Either TypeError Type
check = runExcept . typeof
typeof :: Expr -> Check Type
typeof expr = case expr of
  Succ a -> do
    ta <- typeof a
    case ta of
      TNat -> return TNat
      _    -> throwError $ TypeMismatch ta TNat

  Pred a -> do
    ta <- typeof a
    case ta of
      TNat -> return TNat
      _    -> throwError $ TypeMismatch ta TNat

  IsZero a -> do
    ta <- typeof a
    case ta of
      TNat -> return TBool
      _    -> throwError $ TypeMismatch ta TNat

  If a b c -> do
    ta <- typeof a
    tb <- typeof b
    tc <- typeof c
    if ta /= TBool
    then throwError $ TypeMismatch ta TBool
    else
      if tb /= tc
      then throwError $ TypeMismatch ta tb
      else return tc

  Tr   -> return TBool
  Fl   -> return TBool
  Zero -> return TNat

  Type Env = [(Name, Type)]

extend :: (Name, Type) -> Env -> Env
extend xt env = xt : env

inEnv :: (Name, Type) -> Check a -> Check a
inEnv (x,t) = local (extend (x,t))

lookupVar :: Name -> Check Type
lookupVar x = do
  env <- ask
  case lookup x env of
    Just e  -> return e
    Nothing -> throwError $ NotInScope x


data TypeError
  = Mismatch Type Type
  | NotFunction Type
  | NotInScope Name

type Check = ExceptT TypeError (Reader Env)

check :: Expr -> Check Type
check expr = case expr of

  Lit (LInt{}) -> return TInt

  Lit (LBool{}) -> return TBool

  Lam x t e -> do
    rhs <- inEnv (x,t) (check e)
    return (TArr t rhs)

  App e1 e2 -> do
    t1 <- check e1
    t2 <- check e2
    case t1 of
       (TArr a b) | a == t2 -> return b
       (TArr a _) -> throwError $ Mismatch t2 a
       ty -> throwError $ NotFunction ty

  Var x -> lookupVar x

