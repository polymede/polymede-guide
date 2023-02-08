{-# LANGUAGE Haskell2010                #-}
{-# LANGUAGE OverloadedStrings          #-}

import qualified Data.ByteString.Char8        as B
import qualified Data.ByteString.Lazy.Char8   as LBS
import           Data.Multibase.Kit
import           Data.String

import           PolymedeJSONBase64Pad        as Base64Pad
import           PolymedeSimpleBase64Pad      as Base64Pad


main :: IO ()
main = do
    run_example MbBase64Pad Base64Pad.polymedeSimple Base64Pad.polymedeJSON
    run_example MbBase64Pad Base64Pad.polymedeSimple Base64Pad.polymedeJSON

run_example :: MbAlgorithm -> IO B.ByteString -> IO LBS.ByteString -> IO ()
run_example mba spl jsn = do
    putStrLn "========================"
    putStrLn $ "\ESC[1m" ++ drop 2 (show mba) ++ "\ESC[m"
    putStrLn "========================"
    putStrLn ""
    putStrLn "# simple example"
    putStrLn ""
    sr <- spl
    check sr
    putStrLn ""
    putStrLn "# JSON example"
    putStrLn ""
    jr <- jsn
    check jr
    putStrLn ""

check :: (IsString a,Eq a) => a -> IO ()
check x = case x=="hello world" of
    True  -> putStrLn "** \ESC[32mOK\ESC[m **"
    False -> fail "** NOT OK **"
