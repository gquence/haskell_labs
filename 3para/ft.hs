import System.Random

main = do
    randGen <- newStdGen
    print $ take 20 $ randomRs (0 :: Double, 1) randGen 
