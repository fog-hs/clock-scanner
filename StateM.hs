module StateM where

----
-- StateM

type StateM   m s a = s -> m (Maybe (a, s))
type CoStateM m s a = m (Maybe (a, s)) -> s

hyloStateM :: Functor m => StateM m s a -> (CoStateM m b a) -> s -> b
hyloStateM f c = go
 where
  go = c . fmap (fmap (fmap go)) . f

