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

import           Turtle.Options

import           Lib

-- | 'parser' parses the command line input.
parser :: Parser (Ipv4, Mask, Format)
parser = (\a  (b, c) -> (a, b, c))
  <$> (parseIP <$> argText "ip" "The IP v4 address")
  <*> (parseMask <$> argText "mask" "The IP mask in CIDR and Bits")

-- | 'main' uses 'parser' to parse the input, and then prints all of the information about that input
main :: IO ()
main = do
  (ip, mask, fmt) <- options "Gets the information about a given Subnet" parser

  putStr "Other format of mask: "

  case fmt of
    Cidr   -> print $ Bits mask
    Binary -> print $ Slash mask
    _      -> error "parseMask failed in an unexpected way"

  putStr "Number of hosts: "
  print (hostNum mask)

  putStr "Number of subnets: "
  print $ case subnetNum ip mask of
    Just i -> show i
    Nothing -> "Could not determine the number of subnets, as the ip was not in classes A, B, or C"

  putStr "Subnet Address: "
  print (subnetAddr ip mask)

  putStr "Broadcast Address: "
  print (broadcastAddr ip mask)

  putStr "Range of IP Addresses: "
  putStr (show $ start ip mask)
  putStr "-"
  print (end ip mask)

  putStr "Wildcard mask: "
  print (Wildcard mask)
