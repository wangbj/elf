module Data.ElfSpec (spec) where

import Test.Hspec

import Control.Exception (evaluate)
import qualified Data.ByteString as BS
import Data.Either
import System.IO

import Data.Elf


getBinaryFileContents :: FilePath -> IO BS.ByteString
getBinaryFileContents fname = withBinaryFile fname ReadMode BS.hGetContents


spec :: Spec
spec = do
    emptyContents   <- runIO $ getBinaryFileContents "./testdata/empty"

    describe "parseElf" $ do
        -- TODO: That was the original only test in this package. This test
        -- should be removed, and future versions of this library should return
        -- an 'Either ParseError Elf'.
        it "does not accept an empty elf" $
            evaluate (parseElf emptyContents) `shouldThrow` anyException
