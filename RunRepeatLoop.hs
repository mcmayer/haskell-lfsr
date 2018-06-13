import           Control.Loop  (numLoop)
import           LFSR
import           System.TimeIt (timeItT)

main :: IO ()
main = do
    let nIter = 100000000    -- number of iterations

    lfsr <- newLFSR

    putStrLn "=== RunRepeatLoop ====="
    putStrLn $ "#iter:    " ++ show nIter

    (tBaseLine, _) <- timeItT $ do
        setLFSR lfsr 42
        repeatLFSR lfsr nIter
        getLFSR lfsr
    putStrLn $ "Baseline: " ++ show tBaseLine

    (t, _) <- timeItT $ do
        setLFSR lfsr 42
        numLoop (1::Int) nIter $ \_-> stepLFSR lfsr
        getLFSR lfsr
    putStrLn $ "IO:       " ++ show t

    putStrLn $ "factor:   " ++ show (t/tBaseLine)
