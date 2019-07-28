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
module Lib where

import           Data.Bits     ((.&.), (.|.))
import           Data.List
import           System.Random
import qualified Data.Text as T

import Test.QuickCheck (Arbitrary, arbitrary)

----------------------
-- * Data Structures 
----------------------

-- Command Line Options

-- | 'Format' represents the command line option for which format to print masks in.
-- 'Cidr' corresponds to 'Slash', and 'Binary' corresponds to 'Bits'. 'Both' means a
-- mix will be chosen.
data Format = Cidr | Binary | Both deriving (Eq, Show)

-- | 'Class' represents the class of IPs for this tool to generate, or which class
-- an IP is in. 'All' signifies that all classes should be generated, and occasionally
-- that the IP is not in 'A', 'B', or 'C' class.
data Class = A | B | C | All deriving (Eq, Show, Read)

-- | 'Ipv4' represents an IP address in the v4 format.
-- Constraint: All of these 'Int's are in between 0 and 255 inclusive
data Ipv4 = Octets Int Int Int Int deriving Eq

-- | 'validIp' checks to see if the IP satisfies its constraint.
validIp :: Ipv4 -> Bool
validIp (Octets a b c d) = 
  0 <= a && a <= 255
  && 0 <= b && b <= 255
  && 0 <= c && c <= 255
  && 0 <= d && d <= 255


-- | This 'Show' instance isn't exactly lawful, but 'Show' doesn't have laws anyway.
instance Show Ipv4 where
  show (Octets a b c d) = intercalate "." $ map show [a,b,c,d]

-- | 'Mask' represents an IP range via a mask length. 
-- Constraint: The mask is between 8 and 31
newtype Mask = Mask Int deriving (Eq, Show)

-- | 'unMask' unwraps a 'Mask'
unMask :: Mask -> Int
unMask (Mask i) = i

-- | 'Slash' is a 'Mask' formatted in the /24 notation
-- Using newtypes as a formatting mechanism isn't exactly kosher, but it's what I did.
newtype Slash = Slash Mask

-- | 'Bits' is a 'Mask' formatted in a 255.255.255.0 notation,
newtype Bits = Bits Mask

-- | 'Wildcard' is a 'Mask' formatted in a 0.0.0.255 notation.
newtype Wildcard = Wildcard Mask

instance Show Slash where
  show (Slash (Mask i)) = "/" ++ show i

instance Show Bits where
  show (Bits (Mask i)) = intercalate "." $ map (show . residual) [i, i - 8, i - 16, i - 24]

instance Show Wildcard where
  show (Wildcard (Mask i)) = intercalate "." $ map (show . (255 -) . residual) [i, i - 8, i - 16, i - 24]

-- | 'residual' outputs the binary value needed to mask the input's number of higher order bits.
residual :: Int -> Int
residual 1 = 128
residual 2 = 192
residual 3 = 224
residual 4 = 240
residual 5 = 248
residual 6 = 252
residual 7 = 254
residual x = if x >= 8 then 255 else 0

-- | 'invResidual' is a partial function. It can only be used on values output by 'residual'.
invResidual :: Int -> Int
invResidual 0   = 0
invResidual 128 = 1
invResidual 192 = 2
invResidual 224 = 3
invResidual 240 = 4
invResidual 248 = 5
invResidual 252 = 6
invResidual 254 = 7
invResidual 255 = 8
invResidual x   = error (show x ++ " is not a valid output of residual")

--------------------
-- * Network Logic
--------------------

-- | 'toIp' converts a 'Mask' into an IP just like the 'Bits' formatter does.
toIp :: Mask -> Ipv4
toIp (Mask i) = Octets (residual i) (residual $ i - 8) (residual $ i - 16) (residual $ i - 24)

-- | 'hostNum' outputs the number of usable hosts in the masked subnet.
hostNum :: Mask -> Int
hostNum (Mask i) = 2^(32 - i) - 2

-- | 'subnetNum' outputs the number of subnets with the specified mask exist in the class indicated by the IP.
subnetNum :: Ipv4 -> Mask -> Maybe Int
subnetNum ip (Mask i) = case whichClass ip of
  A -> Just $ 2^(i - 8)
  B -> Just $ 2^(i - 16)
  C -> Just $ 2^(i - 24)
  _ -> Nothing

-- | 'subnetAddr' outputs the address of the subnet containing the given IP and the size of the mask.
subnetAddr :: Ipv4 -> Mask -> Ipv4
subnetAddr (Octets a b c d) m = let
  Octets w x y z = toIp m
  in Octets
    (a .&. w)
    (b .&. x)
    (c .&. y)
    (d .&. z)

-- | 'start' outputs the first usable address of the subnet containing the given IP and the size of the mask.
start :: Ipv4 -> Mask -> Ipv4
start ip m = let
  sn@(Octets a b c d) = subnetAddr ip m
  in case m of
     Mask 32 -> sn
     Mask 31 -> sn
     _       -> Octets a b c (d+1)

-- | 'broadcastAddr' outputs the broadcast address of the subnet containing the given IP and the size of the mask.
broadcastAddr :: Ipv4 -> Mask -> Ipv4
broadcastAddr (Octets a b c d) m = let
  Octets w x y z = toIp m
  in Octets
    (a .|. (255 - w))
    (b .|. (255 - x))
    (c .|. (255 - y))
    (d .|. (255 - z))

-- | 'end' outputs the last usable address of the subnet containing the given IP and the size of the mask.
end :: Ipv4 -> Mask -> Ipv4
end ip m = let
  br@(Octets a b c d) = broadcastAddr ip m
  in case m of
     Mask 32 -> br
     Mask 31 -> br
     _       -> Octets a b c (d-1)

-- | 'whichClass' gives the class of the IP, and if it is not in one of the classes, returns 'All'.
whichClass :: Ipv4 -> Class
whichClass (Octets a b c d)
  | a < 128 = A
  | 128 <= a && a < 192 = B
  | 192 <= a && a < 224 = C
  | otherwise = All

-- | 'isPrivate' is a predicate for whether the IP is in a private range or not.
isPrivate :: Ipv4 -> Bool
isPrivate (Octets a b c d)
  | a == 10 = True
  | a == 172 && 16 <= b && b < 32 = True
  | a == 192 && b == 168 = True
  | otherwise = False

--------------------------
-- * Generator Functions
--------------------------

-- | 'genClass' uses IO to generate a random 'Class'. 
genClass :: IO Class -- TODO(lalaition): make this generic over any random source for ease of testing
genClass = do
  x <- randomIO :: IO Int
  return $ case x `mod` 3 of
    0 -> A
    1 -> B
    2 -> C

-- | 'genMask' uses IO to generate a random 'Mask'. 
genMask :: IO Mask
genMask = do
  i <- randomIO :: IO Int
  return $ Mask ((i `mod` 24) + 8)

-- | 'genF' provides 5 random 'Int's to a function which generates an 'Ipv4' and a 'Mask'.
genF :: (Int -> Int -> Int -> Int -> Int -> (Ipv4, Mask)) -> IO (Ipv4, Mask)
genF f = do
  a <- randomIO :: IO Int
  b <- randomIO :: IO Int
  c <- randomIO :: IO Int
  d <- randomIO :: IO Int
  m <- randomIO :: IO Int
  return (f a b c d m)

-- | 'mkA' is a function for use with 'genF' which generates a class A IP.
mkA :: Int -> Int -> Int -> Int -> Int -> (Ipv4, Mask)
mkA a b c d m =
  ( Octets (mod a 128) (mod b 256) (mod c 256) (mod d 256)
  , Mask (mod m 24 + 8) )

-- | 'mkB' is a function for use with 'genF' which generates a class B IP.
mkB :: Int -> Int -> Int -> Int -> Int -> (Ipv4, Mask)
mkB a b c d m =
  ( Octets (mod a 64 + 128) (mod b 256) (mod c 256) (mod d 256)
  , Mask (mod m 16 + 16) )

-- | 'mkC' is a function for use with 'genF' which generates a class C IP.
mkC :: Int -> Int -> Int -> Int -> Int -> (Ipv4, Mask)
mkC a b c d m =
  ( Octets (mod a 32 + 192) (mod b 256) (mod c 256) (mod d 256)
  , Mask (mod m 8 + 24) )

-- | 'mkPrivate' takes another IP address and changes the first (and possibly second) octet so that it is private.
mkPrivate :: Ipv4 -> Ipv4
mkPrivate (Octets a b c d)
  | 0 <= a && a <= 127 = Octets 10 b c d
  | 128 <= a && a <= 191 = Octets 172 (mod b 16 + 16) c d
  | 192 <= a && a <= 223 = Octets 192 168 c d
  | otherwise = Octets 192 168 c d

-------------
-- * Parsing
-------------

-- | 'readText' is 'read' but for 'T.Text'.
readText :: (Read a) => T.Text -> a
readText = read . T.unpack

-- | 'parseIP' parses an IP from a 'T.Text' value.
parseIP :: T.Text -> Ipv4
parseIP t = let
  [a,b,c,d] = T.splitOn "." t
  in Octets (readText a) (readText b) (readText c) (readText d)

-- | 'parseMask' parses a Mask and its format from a 'T.Text' value, in either 'Slash' or 'Bits' format.
parseMask :: T.Text -> (Mask, Format)
parseMask t = case T.head t of
  '/' -> (Mask $ readText $ T.tail t, Cidr)
  _ -> (Mask $ sum $ map (invResidual . readText) $ T.splitOn "." t, Binary)

--------------
-- * Testing 
--------------

instance Arbitrary Mask where
  arbitrary = Mask . (+8) . (flip mod 24) <$> arbitrary

instance Arbitrary Ipv4 where
  arbitrary = Octets <$> ab <*> ab <*> ab <*> ab
    where ab = flip mod 256 <$> arbitrary

newtype ArbA = ArbA (Ipv4, Mask) deriving (Eq, Show)
newtype ArbB = ArbB (Ipv4, Mask) deriving (Eq, Show)
newtype ArbC = ArbC (Ipv4, Mask) deriving (Eq, Show)

instance Arbitrary ArbA where
  arbitrary = ArbA <$> (mkA <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)

instance Arbitrary ArbB where
  arbitrary = ArbB <$> (mkB <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)

instance Arbitrary ArbC where
  arbitrary = ArbC <$> (mkC <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)

