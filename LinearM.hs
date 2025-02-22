{-# Language 
 KindSignatures
,RankNTypes
,GADTs
,ScopedTypeVariables
,TypeApplications
,DefaultSignatures #-}

module LinearM where
import StackM
import Control.Applicative

class FunctorM (t :: (* -> *) -> * -> *) where
 fmapM ::                    Monad m  => (a -> m b) -> t m a -> m (t m b)
 default fmapM :: (StackM t, Monad m) => (a -> m b) -> t m a -> m (t m b)
 fmapM = fmapMDefault 

class FoldableM (t :: (* -> *) -> * -> *) where
 foldrM ::                 Monad m  => (a -> m (b -> m b)) -> b -> t m a -> m b 
 default foldrM :: (GetM t,Monad m) => (a -> m (b -> m b)) -> b -> t m a -> m b 
 foldrM = foldrMDefault
 foldlM ::                 Monad m  => (b -> a -> m b) -> m b -> t m a -> m b 
 default foldlM :: (GetM t,Monad m) => (b -> a -> m b) -> m b -> t m a -> m b 
 foldlM = foldlMDefault

class ScannerM (t :: (* -> *) -> * -> *) where
 scannerM         ::            Monad m  => (s -> a -> m (b,s)) -> s -> t m a -> m (t m b,s)
 default scannerM :: (StackM t, Monad m) => (s -> a -> m (b,s)) -> s -> t m a -> m (t m b,s)
 scannerM = scannerMDefault 

class TraversableM (t :: (* -> *) -> * -> *) where
 traverseM         ::           (Monad m, Applicative f) => (a -> f b) -> t m a -> m (f (t m b))
 default traverseM :: (StackM t, Monad m, Applicative f) => (a -> f b) -> t m a -> m (f (t m b))
 traverseM = traverseMDefault 

class (StackM t,ScannerM t,TraversableM t,FoldableM t,FunctorM t)=> LinearM t 
