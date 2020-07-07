module Clock where
import ListT
import LinearM
import Control.Concurrent

scannerIO
  :: (s -> IO (b, s)) -> s -> IOList a -> IO (IOList b, s)
scannerIO f s = scannerM (\ s' _ -> f s') s 

----
-- Clock

type Clock = IOList ()

clock :: Clock
clock = liftList $ repeat ()

test1 :: IO ()
test1 = display clock

----
-- timer

timer :: Int -> IO Clock
timer n = fmapM (\ _ -> threadDelay n >> return ()) clock 

test2 :: IO ()
test2 = display =<< timer 100000 

----
-- untimed

untimed :: (s -> IO (b, s)) -> s -> IO (IOList b, s)
untimed f s = scannerIO f s clock

test3 :: IO ()
test3 = display . fst =<< untimed (\s -> return (s*2,s+1)) 0

----
-- timed

timed :: Int -> (s -> IO (b, s)) -> s -> IO (IOList b, s)
timed n f s = scannerIO f s =<< timer n

test4 :: IO ()
test4 = display . fst =<< timed 100000 (\s -> return (s*2,s+1)) 0

