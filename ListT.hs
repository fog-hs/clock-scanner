{-# Language 
TypeSynonymInstances
,RankNTypes
,TypeApplications,ScopedTypeVariables
,InstanceSigs
#-}

module ListT (ListT(..),IOList,liftList,unfoldrM,display) where

import GHC.Base
import Control.Monad.Identity
import StackM
import StateM
import LinearM


----
-- ListT

data ML m a = MEmpty | a `MCons` MList m a
type MList m a  = m (ML m a)

newtype ListT m a = ListT { runListT :: MList m a }

type IOList = ListT IO

liftList :: Monad m => [a] -> ListT m a
liftList [] = ListT $ return MEmpty
liftList (x:xs) = ListT . return $ x `MCons` (runListT $ liftList xs)

unfoldrM :: Monad m => (b -> m (Maybe (a, b))) -> b -> m (ListT m a)
unfoldrM f b = maybe emptyM (\(a,b) -> setM (fmap (\xs -> Just (a,xs)) (unfoldrM f b))) <$> f b

----
-- LinearM instance

instance GetM ListT where
 getM (ListT m) = fmap g m where
  g MEmpty = Nothing
  g (x `MCons` xs) = Just (x, ListT xs)

instance SetM ListT where
 setM  z = ListT $ maybe (return MEmpty) (\(x,ListT xs) -> return (x `MCons` xs)) =<< z
 setM' z = ListT $ maybe (return MEmpty) (\(x,ListT xs) -> return (x `MCons` xs))   $ z

instance StackM ListT
instance FunctorM ListT 
instance FoldableM ListT 
instance ScannerM ListT 
instance TraversableM ListT 
instance LinearM ListT 

----
-- testing

egListT :: IOList Int
egListT = liftList [1..]

egListT1 :: IO (IOList Int)
egListT1 = unfoldrM (\b -> return (Just (b,b))) (0 :: Int)

egListT2 :: IO (IOList Int)
egListT2 = fmapM (\x -> return (x*2)) egListT 

egListT3 :: IO (IOList Int)
egListT3 = fst <$> scannerM (\s a -> return (a*s,s+1)) (0::Int) egListT 

display :: Show a => IOList a -> IO ()
display = foldrM (\a -> print a >> (return (\ () -> return ()))) () 

test  =              display egListT
test1 = egListT1 >>= display
test2 = egListT2 >>= display
test3 = egListT3 >>= display



