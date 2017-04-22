module Qy.Random where

import           Data.Char     (isAlphaNum)
import qualified Data.Text     as T
import           System.Random

randomText :: IO T.Text
randomText = do
    gen <- newStdGen
    let s = T.pack . take 128 $ filter isAlphaNum $ randomRs ('A', 'z') gen
    return s
