import           Control.Monad (replicateM_)
--import           Control.Monad.Extra (sequenceUntil)
import           LFSR
import           System.TimeIt (timeItT)

main :: IO ()
main = do
    let n = 10000000

    lfsr <- newLFSR

    putStrLn "=== RunRepeat ======="

    (tBaseLine, _) <- timeItT $ do
        setLFSR lfsr 42
        repeatLFSR lfsr n
        getLFSR lfsr
    putStrLn $ "Baseline: " ++ show tBaseLine

    (t, _) <- timeItT $ do
        setLFSR lfsr 42
        replicateM_ n (stepLFSR lfsr)
        getLFSR lfsr
    putStrLn $ "IO:       " ++ show t
