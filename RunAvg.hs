import           Control.Monad (replicateM)
import           LFSR
import           System.TimeIt (timeItT)

step1 :: LFSR -> IO Word32
step1 lfsr = stepLFSR lfsr >> getLFSR lfsr

avg :: LFSR -> Int -> IO Double
avg lfsr n = mean <$> replicateM n (step1 lfsr) where
    mean :: [Word32] -> Double
    mean vs = (sum $ fromIntegral <$> vs) / (fromIntegral n)

main :: IO ()
main = do
    let n = 10000000

    lfsr <- newLFSR

    putStrLn "=== RunAvg ========="

    (tBaseLine, _) <- timeItT $ do
        setLFSR lfsr 42
        avgLFSR lfsr n
    putStrLn $ "Baseline: " ++ show tBaseLine

    (t, _) <- timeItT $ do
        setLFSR lfsr 42
        avg lfsr n
    putStrLn $ "IO:       " ++ show t
