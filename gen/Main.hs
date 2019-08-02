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

import           Control.Monad (replicateM_)
import           Data.Maybe    (fromMaybe, isJust, fromJust, fromMaybe)
import           System.Random
import           System.Environment
import           System.Exit
import           Text.Read

import           Lib
import Debug.Trace

-- | 'Options' holds the command line options for this program
data Options = Options
  { format  :: Format
  , amount  :: Int
  , ipclass :: Class
  , private :: Bool
  } deriving (Eq, Show)

-- | 'PartialOptions' represents an Options in the middle of being parsed
data PartialOptions = PartialOptions
  { mformat  :: Maybe Format
  , mamount  :: Maybe Int
  , mipclass :: Maybe Class
  , mprivate :: Maybe Bool
  } deriving (Eq, Show)

-- | This is a 'PartialOptions' with no values initialized.
defaultPartial :: PartialOptions
defaultPartial = PartialOptions Nothing Nothing Nothing Nothing

-- | The help message printed out when this program exits due to a failed parse
-- of command line arguments.
helpMessage :: String
helpMessage = "ip-generate is a command line tool for generating ip addresses according\n\
              \to a variety of command line flags. It is a companion tool to ip-answer\n\
              \and in general a line of output from ip-generate is a valid set of\n\
              \arguments for ip-answer.\n\
              \\n\
              \usage:        ip-generate [--private] [--cidr|--binary] [--class {A|B|C}] --number NUMBER\n\
              \\n\
              \--private -p  Indicates that the IPs generated should be private\n\
              \\n\
              \--cidr    -c  Indicates that the IPs should be displayed in cidr notation\n\
              \--binary  -b  Indicates that the IPs should be displayed in mask notation\n\
              \              If neither of the above are specified, a mix of the two will\n\
              \              be generated\n\
              \\n\
              \--class   -s  Indicates which class of IP should be generated. If not specified,\n\
              \              both classes will be generated.\n\
              \\n\
              \--number  -n  Followed by the number of IPs to generate. Required.\n\
              \\n\
              \If two contradictory arguemnts are passed in, the one occuring later in the\n\
              \list overrides the former."

-- | prints 'helpMessage' and exits with a failed exit code.
printHelpAndExit :: IO a
printHelpAndExit = do
  putStrLn helpMessage
  exitFailure

-- | 'intoOptions' converts a PartialOptions into an Options value
-- filling either defaults or throwing errors.
intoOptions :: PartialOptions -> IO Options
intoOptions opts = Options
  <$> return (fromMaybe Both (mformat opts))
  <*> (if isJust (mamount opts) then return (fromJust (mamount opts)) else printHelpAndExit)
  <*> return (fromMaybe All (mipclass opts))
  <*> return (fromMaybe False (mprivate opts))

-- | Sets the format option to 'Cidr' in the 'PartialOptions'
addCidr :: PartialOptions -> PartialOptions
addCidr opts = opts {mformat = Just Cidr}

-- | Sets the format option to 'Binary' in the 'PartialOptions'
addBinary :: PartialOptions -> PartialOptions
addBinary opts = opts {mformat = Just Binary}

-- | Sets the private option in the 'PartialOptions'
addPrivate :: PartialOptions -> PartialOptions
addPrivate opts = opts {mprivate = Just True}

-- | Sets the class in the 'PartialOptions'. Can fail and consumes the next
-- item in the args string if successful.
addClass :: PartialOptions -> [String] -> IO (PartialOptions, [String])
addClass opts ("A":xs) = return (opts {mipclass = Just A}, xs)
addClass opts ("B":xs) = return (opts {mipclass = Just B}, xs)
addClass opts ("C":xs) = return (opts {mipclass = Just C}, xs)
addClass _ _ = printHelpAndExit

-- | Sets the amount in the 'PartialOptions'. Can fail and consumes the next
-- item in the args string if successful.
addNumber :: PartialOptions -> [String] -> IO (PartialOptions, [String])
addNumber opts (x:xs) | Just n <- readMaybe x = return (opts {mamount = Just n}, xs)
addNumber _ _ = printHelpAndExit

-- | expandJumble takes arguments like "-cpn" and makes them into "-c -p -n"
expandJumble :: String -> [String]
-- The filter below is needed, otherwise `--` expands to `--`, an infinite loop
expandJumble = map (\c -> ['-', c]) . filter (/='-')

-- | 'parseOptions' is a tail recursive function that parses a list of flags into an 'Options'
parseOptions :: PartialOptions -> [String] -> IO Options
parseOptions acc [] = intoOptions acc
parseOptions acc (x:xs)
  | x == "-c" || x == "--cidr" = parseOptions (addCidr acc) xs
  | x == "-b" || x == "--binary" = parseOptions (addBinary acc) xs
  | x == "-p" || x == "--private" = parseOptions (addPrivate acc) xs
  | x == "-s" || x == "--class" = do
    (acc', xs') <- addClass acc xs
    parseOptions acc' xs'
  | x == "-n" || x == "--number" = do
    (acc', xs') <- addNumber acc xs
    parseOptions acc' xs'
  | head x == '-' && all (\c -> elem c "cbpsn") (tail x) =
    traceShow x $ parseOptions acc (expandJumble (tail x) ++ xs)
  | otherwise = printHelpAndExit

-- | 'parse' parses the command line input.
parse :: IO Options
parse = getArgs >>= parseOptions defaultPartial

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
main = do
  ops <- parse
  replicateM_ (amount ops) (one ops)
