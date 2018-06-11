import           Control.Monad (replicateM_)
--import           Control.Monad.Extra (sequenceUntil)
import           LFSR
import           System.TimeIt (timeItT)

main :: IO ()
main = do
    let nIter = 100000000    -- number of iterations

    lfsr <- newLFSR

    putStrLn "=== RunRepeat ======="
    putStrLn $ "#iter:    " ++ show nIter

    (tBaseLine, _) <- timeItT $ do
        setLFSR lfsr 42
        repeatLFSR lfsr nIter
        getLFSR lfsr
    putStrLn $ "Baseline: " ++ show tBaseLine

    (t, _) <- timeItT $ do
        setLFSR lfsr 42
        replicateM_ nIter (stepLFSR lfsr)
        getLFSR lfsr
    putStrLn $ "IO:       " ++ show t

    putStrLn $ "factor:   " ++ show (t/tBaseLine)
