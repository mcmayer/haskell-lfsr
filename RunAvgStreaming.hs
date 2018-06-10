import           LFSR
import           Streaming.Prelude (Of (..))
import qualified Streaming.Prelude as S
import           System.TimeIt     (timeItT)


step1 :: LFSR -> IO Word32
step1 lfsr = stepLFSR lfsr >> getLFSR lfsr

avg :: LFSR -> Int -> IO Double
avg lfsr n = do
    let stream = S.replicateM n (fromIntegral <$> step1 lfsr :: IO Double)
    (mySum :> _) <- S.sum stream
    return (mySum / fromIntegral n)

main :: IO ()
main = do
    let nIter = 10000000    -- number of iterations

    lfsr <- newLFSR

    putStrLn "=== RunAvgStreaming ==="

    (tBaseLine, _) <- timeItT $ do
        setLFSR lfsr 42
        avgLFSR lfsr nIter
    putStrLn $ "Baseline: " ++ show tBaseLine

    (t, _) <- timeItT $ do
        setLFSR lfsr 42
        avg lfsr nIter
    putStrLn $ "IO:       " ++ show t

    putStrLn $ "factor:   " ++ show (t/tBaseLine)
