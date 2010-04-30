-- Implementation of file operations and a trivial calculation functions for
-- a very simple(!) artificial neuron.
--
-- TODO
--   * connect multiple neurons. either extend Neuron type or create an
--     additional type NeuronNetwork.
--   * add trivial learning algorithm, e.g. for perceptrons.
--   * add more boilerplate code for (automated) testing
--   * if time and motivation, add backpropagation
module Main where

data Neuron = Neuron {
      nThreshold :: Double
    , nWeights   :: [Double]
} deriving Show


-- Example neurons for testing in ghci
n1 = Neuron 0.5 [0.2, 0.3]
n2 = Neuron 0.9 [0.2, 0.3, 0.9]


-- Fires, if the weighted values are bigger than the threshold.
calc :: Neuron -> [Double] -> Double
calc (Neuron ts ws) inputs =
    let value = sum $ zipWith (*) ws inputs
    in if value >= ts then 1.0 else 0.0


-- Read one neuron from a file. File format is
--   comment line -- ignored!
--   <threshold>
--   <weight 1>
--      ...
--   <weight 2>
readNeuron :: FilePath -> IO Neuron
readNeuron fname = do
    inp <- (map read . tail . lines) `fmap` readFile fname
    let ts = head inp
        ws = tail inp
    return (Neuron ts ws)

