{-# Language
KindSignatures
,ConstraintKinds
,DefaultSignatures
,TypeFamilies
,ScopedTypeVariables
,TypeApplications
#-}

module StackM where

import Control.Applicative (liftA2)
import StateM

----
-- StackM

class GetM (f :: (* -> *) -> * -> *) where 
 getM :: Monad m => StateM m (f m a) a

class SetM (f :: (* -> *) -> * -> *) where 
 setM  :: Monad m => m (Maybe (a, f m a)) -> f m a -- delete?
 setM' :: Monad m =>    Maybe (a, f m a)  -> f m a

consM :: (SetM f, Monad m) => a -> f m a -> f m a
consM x = setM' . ((Just . ((,) x))) 

emptyM :: (SetM f,Monad m) => f m a
emptyM = setM (pure Nothing)

class (GetM f,SetM f) => StackM f

----
-- FunctorM default

fmapMDefault :: (StackM t, Monad m) => (a -> m b) -> t m a -> m (t m b)
fmapMDefault f s = getM s >>= maybe (return emptyM) g
  where
   g (x,xs) = do
    x' <- f x
    return $ setM (fmap (\ys -> (Just (x' ,ys))) (fmapMDefault f xs))

----
-- FoldableM default

foldrMDefault :: (GetM t,Monad m) => (a -> m (b -> m b)) -> b -> t m a -> m b 
foldrMDefault f b xs = getM xs >>= maybe (return b) (\(x,xs)->f x >>= \g -> foldrMDefault f b xs >>= g) 

foldlMDefault :: (GetM t,Monad m) => (b -> a -> m b) -> m b -> t m a -> m b 
foldlMDefault f b xs = getM xs >>= maybe b (\(x,xs)-> foldlMDefault f (b >>= \y -> (f y x)) xs) 

----
-- ScannerM default

scannerMDefault
  :: forall s (t :: (* -> *) -> * -> *) (m :: * -> *) a b. (Monad m, StackM t) =>
     (s -> a -> m (b,s)) -> s -> t m a -> m (t m b,s)
scannerMDefault f s xs = getM xs >>= maybe (return (emptyM,s)) g -- (\(x,xs) ->  h xs <$> f s x)
  where
   g :: (a, t m a) -> m (t m b, s)
   g (x,xs) = h xs <$> f s x
   h :: t m a -> (b, s) -> (t m b, s)
   h xs' (x',s') = (setM $ (Just . (,) x') <$> fst <$> scannerMDefault f s' xs',s')

----
-- TraverseM default

traverseMDefault :: forall a b t m f. (StackM t, Monad m, Applicative f) => (a -> f b) -> t m a -> m (f (t m b))
traverseMDefault = convertM

----
-- convertM

convertM :: forall a b t' t m f. (SetM t', GetM t, Monad m, Applicative f) => (a -> f b) -> t m a -> m (f (t' m b))
convertM f s = getM s >>= maybe (return (pure emptyM)) g
 where
  g (x,xs) = (\y->((<*>)( fmap (curry (setM' . Just)) y)) <$> convertM f xs) (f x)

{-
stackMDifference :: forall t t' m a b f. (Monad m,GetM t, SetM t', Applicative f) => (a -> f b) -> StateM m (t m a) (f (t' m b -> t' m b))
stackMDifference f xs = fmap (\(x,xs') -> (fmap (curry (setM.pure.Just)) (f x),xs')) <$> getM xs

convertM' :: forall f m t t' a b. (f~m,Monad m,GetM t, SetM t', Applicative f) => (a -> f b) -> t m a -> f (t' m b)
convertM' f = hyloStateM (stackMDifference f) g
 where
  g :: (SetM t',Applicative f,Applicative m) => CoStateM m (f (t' m b)) (f (t' m b -> t' m b))
  g m = m >>= maybe (return emptyM) (uncurry (liftA2 ($)))
-}
