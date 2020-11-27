module HMM where

import Utils

-- TODO:
-- Log and log-sum-exp trick (refer to HMM ipad notability last but one)
  -- http://hackage.haskell.org/package/logfloat-0.13.3.3/docs/Data-Number-LogFloat.html
-- memoize
-- hmm betas?
-- tests
-- Main.hs
-- viterbi
-- ctc loss
-- ctc backward?
-- ctc greedy
-- ctc beam search https://distill.pub/2017/ctc/, https://gist.github.com/awni/56369a90d03953e370f3964c826ed4b0

-- Hidden Markov Model
-- Ref: https://stanford.edu/~jurafsky/slp3/A.pdf

data HMM a = HMM {
  initProbs :: [a],
  transitionProbs :: Matrix a,
  emissionProbs :: Matrix a
} deriving (Show)

-- TODO
alpha :: (RealFloat a) => HMM a -> [Int] -> Int -> Int -> a
alpha hmm obs t i
  | t == 0 = initProb * emitProb
  | otherwise = sum $ map prevAlpha [0..numStates-1]
  where
    o = obs!!t
    initProb = (initProbs hmm)!!i
    emitProb = (emissionProbs hmm)!(i, o)
    a = transitionProbs hmm
    numStates = length (initProbs hmm)
    prevAlpha j = (alpha hmm obs (t-1) j) * (a!(j, i)) * emitProb

-- TODO
likelihood :: (RealFloat a) => HMM a -> [Int] -> a
likelihood hmm obs = sum $ map (alpha hmm obs (t-1)) [0..numStates-1]
  where
    t = length obs
    numStates = length (initProbs hmm)

-- alpha :: (RealFloat a) => [Int] -> [a] -> Matrix a -> Matrix a
-- alpha (i, j) obs initProbs transProbs emissionProbs
-- | i == 0 = (initProbs ! 0)

-- TODO
-- forward :: (RealFloat a) => [Int] -> [[a]] -> [[a]]
-- forward obs transitionProbs emissionProbs =
