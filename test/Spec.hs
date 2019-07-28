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

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.Char
import Data.List.Split
import Data.Maybe
import qualified Data.Text as T

import Lib 

main :: IO ()
main = hspec $ do

  -- Data Structures

  describe "Ipv4" $ do
    it "shows four numbers separated by dots" $ do
      property (\i@(Octets a b c d) -> show i == show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d)
  
  describe "Slash" $ do
    it "shows with a slash" $ do
      property $ (\m -> let 
       s = show $ Slash m
       in head s == '/' && all isDigit (tail s))   

    it "shows examples correctly" $ do
      (show $ Slash $ Mask 9) `shouldBe` "/9"
      (show $ Slash $ Mask 19) `shouldBe` "/19"
      (show $ Slash $ Mask 26) `shouldBe` "/26"

  describe "Bits" $ do
    it "shows four numbers separated by dots" $ do
      property (\m -> let 
        qs = splitOn "." $ show $ Bits m
        in length qs == 4 && all (all isDigit) qs)

    it "shows examples correctly" $ do
      (show $ Bits $ Mask 9) `shouldBe` "255.128.0.0"
      (show $ Bits $ Mask 19) `shouldBe` "255.255.224.0"
      (show $ Bits $ Mask 26) `shouldBe` "255.255.255.192"

  describe "Wildcard" $ do
    it "shows four numbers separated by dots" $ do
      property (\m -> let 
        qs = splitOn "." $ show $ Wildcard m
        in length qs == 4 && all (all isDigit) qs)

    it "shows examples correctly" $ do
      (show $ Wildcard $ Mask 9) `shouldBe` "0.127.255.255"
      (show $ Wildcard $ Mask 19) `shouldBe` "0.0.31.255"
      (show $ Wildcard $ Mask 26) `shouldBe` "0.0.0.63"

  describe "residual and invResidual" $ do
    it "are inverses, over the range of residual" $ do
      property (\x -> residual (invResidual (residual x)) == residual x)

  -- Network Logic

  describe "toIp" $ do
    it "produces a valid ip" $ do
      property (\m -> validIp (toIp m)) 

    it "shows the same as Bits" $ do
      property (\m -> show (toIp m) == show (Bits m))

  describe "hostNum" $ do
    it "always produces a positive number" $ do
      property (\m -> hostNum m >= 0)

    it "gives the right value for some examples" $ do
      hostNum (Mask 9) `shouldBe` 8388606
      hostNum (Mask 19) `shouldBe` 8190
      hostNum (Mask 26) `shouldBe` 62

  describe "subnetNum" $ do
    it "always produces a positive number for class A networks" $ do 
      property (\(ArbA (i, m)) -> fromMaybe 0 (subnetNum i m) >= 0)

    it "always produces a positive number for class B networks" $ do 
      property (\(ArbB (i, m)) -> fromMaybe 0 (subnetNum i m) >= 0)

    it "always produces a positive number for class C networks" $ do 
      property (\(ArbC (i, m)) -> fromMaybe 0 (subnetNum i m) >= 0)

    it "gives the right value for some examples" $ do
      subnetNum (Octets 110 0 0 0) (Mask 26) `shouldBe` Just 262144
      subnetNum (Octets 150 0 0 0) (Mask 26) `shouldBe` Just 1024 
      subnetNum (Octets 222 0 0 0) (Mask 26) `shouldBe` Just 4 

      subnetNum (Octets 110 0 0 0) (Mask 19) `shouldBe` Just 2048
      subnetNum (Octets 150 0 0 0) (Mask 19) `shouldBe` Just 8 

      subnetNum (Octets 110 0 0 0) (Mask 9) `shouldBe` Just 2

  describe "subnetAddr" $ do
    it "is idempotent" $ do
      property (\i m -> subnetAddr i m == subnetAddr (subnetAddr i m) m)

    it "is the identity for ips that came from masks" $ do
      property (\m -> subnetAddr (toIp m) m == toIp m)

    it "always produces a valid ip" $ do
      property (\i m -> validIp (subnetAddr i m))

    it "gives the right value for some examples" $ do
      subnetAddr (Octets 110 220 34 15) (Mask 8) `shouldBe` Octets 110 0 0 0  
      subnetAddr (Octets 110 220 34 15) (Mask 16) `shouldBe` Octets 110 220 0 0  
      subnetAddr (Octets 110 220 34 15) (Mask 24) `shouldBe` Octets 110 220 34 0  
      subnetAddr (Octets 110 220 34 15) (Mask 11) `shouldBe` Octets 110 192 0 0 
      subnetAddr (Octets 110 220 34 15) (Mask 30) `shouldBe` Octets 110 220 34 12
      subnetAddr (Octets 110 220 34 15) (Mask 31) `shouldBe` Octets 110 220 34 14 
      subnetAddr (Octets 110 220 34 15) (Mask 32) `shouldBe` Octets 110 220 34 15 

  describe "start" $ do
    it "gives the right value for some examples" $ do
      start (Octets 110 220 34 15) (Mask 8) `shouldBe` Octets 110 0 0 1  
      start (Octets 110 220 34 15) (Mask 16) `shouldBe` Octets 110 220 0 1  
      start (Octets 110 220 34 15) (Mask 24) `shouldBe` Octets 110 220 34 1  
      start (Octets 110 220 34 15) (Mask 11) `shouldBe` Octets 110 192 0 1 
      start (Octets 110 220 34 15) (Mask 30) `shouldBe` Octets 110 220 34 13
      start (Octets 110 220 34 15) (Mask 31) `shouldBe` Octets 110 220 34 14 
      start (Octets 110 220 34 15) (Mask 32) `shouldBe` Octets 110 220 34 15 
      
  describe "broadcastAddr" $ do
    it "gives the right value for some examples" $ do 
      broadcastAddr (Octets 110 220 34 15) (Mask 8) `shouldBe` Octets 110 255 255 255  
      broadcastAddr (Octets 110 220 34 15) (Mask 16) `shouldBe` Octets 110 220 255 255 
      broadcastAddr (Octets 110 220 34 15) (Mask 24) `shouldBe` Octets 110 220 34 255
      broadcastAddr (Octets 110 220 34 15) (Mask 11) `shouldBe` Octets 110 223 255 255 
      broadcastAddr (Octets 110 220 34 15) (Mask 30) `shouldBe` Octets 110 220 34 15
      broadcastAddr (Octets 110 220 34 15) (Mask 31) `shouldBe` Octets 110 220 34 15
      broadcastAddr (Octets 110 220 34 15) (Mask 32) `shouldBe` Octets 110 220 34 15

  describe "end" $ do
    it "gives the right value for some examples" $ do 
      end (Octets 110 220 34 15) (Mask 8) `shouldBe` Octets 110 255 255 254  
      end (Octets 110 220 34 15) (Mask 16) `shouldBe` Octets 110 220 255 254 
      end (Octets 110 220 34 15) (Mask 24) `shouldBe` Octets 110 220 34 254
      end (Octets 110 220 34 15) (Mask 11) `shouldBe` Octets 110 223 255 254
      end (Octets 110 220 34 15) (Mask 30) `shouldBe` Octets 110 220 34 14
      end (Octets 110 220 34 15) (Mask 31) `shouldBe` Octets 110 220 34 15
      end (Octets 110 220 34 15) (Mask 32) `shouldBe` Octets 110 220 34 15

  -- Parsing

  describe "parseIP" $ do
    it "is the inverse of IP's show" $ do
      property (\i -> parseIP (T.pack $ show i) == i)

  describe "parseMask" $ do
    it "is the inverse of Slash's show" $ do
      property (\m -> parseMask (T.pack $ show $ Slash m) == (m, Cidr))

    it "is the inverse of Bits's show" $ do
      property (\m -> parseMask (T.pack $ show $ Bits m) == (m, Binary))
 
  -- Generator Functions

  describe "genClass" $ do
    it "always generates A, B, or C" $ do
      forM_ [0 :: Int ..100] $ \_ -> do
        c <- genClass
        c `shouldSatisfy` (`elem` [A, B, C])

  describe "genMask" $ do
    it "always generates a valid mask" $ do
      forM_ [0 :: Int ..100] $ \_ -> do
        (Mask i) <- genMask
        i `shouldSatisfy` (\x -> 8 <= x && x <= 32)

  describe "mkA" $ do
    it "always generates a class A ip" $ do
      property (\a b c d -> let ip = fst $ mkA a b c d 24 in whichClass ip == A)

    it "always generates a mask between 8 and 32" $ do
      property (\m -> let mask = snd $ mkA 192 0 0 1 m in unMask mask >= 8 && unMask mask <= 32)

    it "always generates a valid ip" $ do
      property (\a b c d -> validIp (fst $ mkA a b c d 24))

  describe "mkB" $ do
    it "always generates a class B ip" $ do
      property (\a b c d -> let ip = fst $ mkB a b c d 24 in whichClass ip == B)

    it "always generates a mask between 16 and 32" $ do
      property (\m -> let mask = snd $ mkB 192 0 0 1 m in unMask mask >= 16 && unMask mask <= 32)

    it "always generates a valid ip" $ do
      property (\a b c d -> validIp (fst $ mkB a b c d 24))

  describe "mkC" $ do
    it "always generates a class C ip" $ do
      property (\a b c d -> let ip = fst $ mkC a b c d 24 in whichClass ip == C)

    it "always generates a mask between 24 and 32" $ do
      property (\m -> let mask = snd $ mkC 192 0 0 1 m in unMask mask >= 24 && unMask mask <= 32)

    it "always generates a valid ip" $ do
      property (\a b c d -> validIp (fst $ mkC a b c d 24))

  describe "mkPrivate" $ do
    it "always makes an ip private" $ do
      property (\i -> isPrivate (mkPrivate i))

    it "always makes a valid ip" $ do
      property (\i -> validIp (mkPrivate i))
