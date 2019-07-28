{-  Copyright 2019 Google LLC

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    https://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License. -}

{-# LANGUAGE OverloadedStrings #-}

import           Data.Maybe    (fromMaybe)
import           System.Random
import           Turtle        hiding (Format, format, s, x)

import           Lib

-- | 'Options' holds the command line options for this program
data Options = Options
  { format  :: Format
  , amount  :: Int
  , ipclass :: Class
  , private :: Bool
  } deriving (Eq, Show)

-- | 'intoOptions" converts a bunch of individual values into an 'Options' value
intoOptions :: Int -> Bool -> Bool -> Maybe Class -> Bool ->  Options
intoOptions n c b s p = Options
  (if c && b then Both else if c then Cidr else if b then Binary else Both)
  n
  (fromMaybe All s)
  p

-- | 'parser' is a command line parser of 'Options'
parser :: Parser Options
parser = intoOptions
  <$> optInt "number" 'n' "output this many values"
  <*> switch "cidr" 'c' "use cider notation"
  <*> switch "binary" 'b' "use binary notation"
  <*> optional (optRead "class" 's' "only generate class A, B, C (or All) ips")
  <*> switch "private" 'p' "only generate private ips"

-- | 'genWith' calls the generation functions in 'Lib' with the information in 'Options'
genWith :: Options -> IO (Ipv4, Mask)
genWith ops = case ipclass ops of
  A -> genF mkA
  B -> genF mkB
  C -> genF mkC
  All -> do
    x <- genClass
    genWith ops { ipclass = x }

-- | 'showWith' shows a 'Mask' according to a 'Format', choosing one randomly if that format is 'Both'
showWith :: Format -> Mask -> IO String
showWith Cidr m = return $ show (Slash m)
showWith Binary m = return $ show (Bits m)
showWith Both m = do
  b <- randomIO :: IO Bool
  return $ if b then show (Slash m) else show (Bits m)

-- | 'one' generates and prints one IP and mask.
one :: Options -> IO ()
one ops = do
  (rawip, mask) <- genWith ops
  let ip = if private ops then mkPrivate rawip else rawip
  maskStr <- showWith (format ops) mask
  putStrLn (show ip ++ " " ++ maskStr)

-- | 'main' parses the command line options, and then executes 'one' 'amount' times
main :: IO ()
main = sh $ do
  ops <- options "Generates IP addresses and Subnet Masks" parser
  liftIO $ replicateM_ (amount ops) (one ops)
