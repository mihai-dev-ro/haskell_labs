module Typing.Inference where

import Syntax.Expression
import Typing.Type
import Typing.Unification
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M

{-|
    The type of inference state.
    
    Should comprise:
    
    * The global typing context
    
    * The type variable counter.
-}
data TypingState = TypingState
    { context :: TypingContext
    , counter :: Counter
    } deriving Show
    
{-|
    The type of the inference mechanism.
    
    Should expose the following:
    
    * Access to the inference state (State)
    
    * Acces to the local typing context (Reader)

    * A means for storing unification constraints (Writer)
-}
type Infer = ReaderT TypingContext (WriterT [(Type, Type)] (State TypingState))

runInfer :: Infer a        -- ^ Expression to type
         -> TypingContext  -- ^ Local context
         -> TypingContext  -- ^ Global context
         -> Counter        -- ^ Current type variable counter
         -> (a, [(Type, Type)])
                           -- ^ The result, along with possible unification
                           --   constraints; otherwise, an error message
runInfer inf loc glob cnt = evalState (runWriterT $ runReaderT inf loc) $ 
                                      TypingState glob cnt

{-|
    Generates a copy of the given type.
    
    Should rely on 'copyM' below.
-}
copy :: Type -> Type
copy t = fst $ runInfer (copyM t) M.empty M.empty 0

{-|
    The type inference function, wich synthesizes the type of the given
    expression.
    
    Should rely on 'inferM' below.
-}
infer :: Expression          -- ^ Expression to type
      -> TypingContext       -- ^ Local context
      -> TypingContext       -- ^ Global context
      -> Substitution        -- ^ Substitution
      -> Counter             -- ^ Current type variable counter
      -> Either String Type  -- ^ If the typing succeeds,
                             --   the inferred type; otherwise, an error 
                             --   message.
infer expr loc glob subst cnt = 
    let 
        -- 1. Obtain the synthesized type together with the unifications required
        (t, unfs) = runInfer (inferM expr) loc glob cnt
        -- 2. Define Reducer to unify a pair of types and 
        --    obtain a new Unified Type by performing the 
        --    substitions required by the new bindings discovered during the unification
        reducer unifT (t1, t2) = do
            unify t1 t2
            t <- unifT
            applySubst t
        -- 3. Obtain the End reduced Unified Type
        endType = foldl reducer (return t) unfs
    in  
        fst <$> runUnif endType subst



{-|
    Generates a new type variable using the counter hidden within the state,
    and updates the latter.
-}
newTypeVar :: Infer Type
newTypeVar = do
    globState <- get
    let c = (counter globState) + 1
    let glob = context globState
    put $ TypingState glob c 
    return $ TypeVar ("t" ++ (show c))

{-|
    See 'copy'.
-}
copyM :: Type -> Infer Type
copyM tv@(TypeVar v) = do 
    loc <- ask
    case M.lookup v loc of 
        Nothing -> newTypeVar
        Just tv' -> return tv'
copyM ta@(Arrow t1 t2) = do
    tv1' <- copyM t1
    tv2' <- local (M.insert (show t1) tv1') (copyM t2)
    return $ Arrow tv1' tv2'

{-|
    See 'infer'.
-}
inferM :: Expression -> Infer Type

inferM v@(Var x) = do
    loc <- ask
    case M.lookup x loc of
        Just t -> return t
        Nothing -> do
            globState <- get
            let glob = context globState
            newT <- case M.lookup x glob of
                        Just tPolymorfic -> copyM tPolymorfic
                        Nothing -> newTypeVar
                        
            return newT 
        
inferM abs@(Lambda x e) = do 
    loc <- ask
    tX <- case M.lookup x loc of 
            Just t -> return t
            Nothing -> newTypeVar
    tE <- local (M.insert x tX) (inferM e)
    return $ Arrow tX tE


inferM app@(Application e a) = do
    t1 <- newTypeVar
    t2 <- newTypeVar
    tLambda <- return $ Arrow t1 t2
    tE <- inferM e
    tA <- inferM a
    tell [(tE, tLambda), (tA, t1)]
    return t2

inferM def@(Definition name e) = do
    tName <- inferM $ Var name
    tE <- local (M.insert name tName) (inferM e)
    tell [(tName, tE)]
    globState <- get
    let glob = context globState
    let ct = counter globState
    put $ TypingState (M.insert name tE glob) ct
    return tE

