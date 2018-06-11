import qualified Data.Vector.Fusion.Stream.Monadic as V
import           LFSR
import           System.TimeIt                     (timeItT)

step1 :: LFSR -> IO Word32
step1 lfsr = stepLFSR lfsr >> getLFSR lfsr

step1' :: LFSR -> IO Double
step1' lfsr = fromIntegral <$> step1 lfsr

avg :: LFSR -> Int -> IO Double
avg lfsr n = do
    let stream = V.replicateM n (step1' lfsr)
    (/fromIntegral n) <$> V.foldl (+) 0.0 stream

main :: IO ()
main = do
    let nIter = 10000000    -- number of iterations

    lfsr <- newLFSR

    putStrLn "=== RunVector ========="

    (tBaseLine, _) <- timeItT $ do
        setLFSR lfsr 42
        avgLFSR lfsr nIter
    putStrLn $ "Baseline: " ++ show tBaseLine

    (t, _) <- timeItT $ do
        setLFSR lfsr 42
        avg lfsr nIter
    putStrLn $ "IO:       " ++ show t

    putStrLn $ "factor:   " ++ show (t/tBaseLine)
