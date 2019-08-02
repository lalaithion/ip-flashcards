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

import           Lib
import           System.Environment
import           System.Exit

-- | The help message printed out when this program exits due to a failed parse
-- of command line arguments.
helpMessage :: String
helpMessage = "ip-answer is a command line tool for answering information about\n\
              \ip addresses with subnet masks. It is a companion tool to ip-generate\n\
              \and in general a line of output from ip-generate is a valid set of\n\
              \arguments for ip-answer.\n\
              \\n\
              \usage:     ip-answer IP-ADDRESS MASK\n\
              \\n\
              \IP-ADDRESS This is an IP address written as four numbers between 0 and 255\n\
              \           separated by periods, such as 127.0.0.1 or 8.8.8.8\n\
              \\n\
              \MASK       This is a mask either written in Cidr notation or as a\n\
              \           bitmask. Cidr notation would be /8 or /21 and a bitmask\n\
              \           would be 255.0.0.0 or 255.255.248.0\n\
              \\n\
              \Common errors:\n\
              \  * The IP-ADDRESS and the MASK must be separatec by a space.\n\
              \    10.6.0.1/13 is not a valid argument."

-- | prints 'helpMessage' and exits with a failed exit code.
printHelpAndExit :: IO a
printHelpAndExit = do
  putStrLn helpMessage
  exitFailure

-- | 'parse' parses the command line input.
parse :: IO (Ipv4, Mask, Format)
parse = do
  args <- getArgs
  case args of
    [] -> printHelpAndExit
    [_] -> printHelpAndExit
    [ipString, maskString] -> case (parseIP ipString, parseMask maskString) of
      (Just ip, Just (mask, fmt)) -> return (ip, mask, fmt)
      _ -> printHelpAndExit
    _ -> printHelpAndExit

-- | 'main' uses 'parser' to parse the input, and then prints all of the information about that input
main :: IO ()
main = do
  (ip, mask, fmt) <- parse

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
