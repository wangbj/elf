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
    tinyContents    <- runIO $ getBinaryFileContents "./testdata/tiny"
    bloatedContents <- runIO $ getBinaryFileContents "./testdata/bloated"

    describe "parseElf" $ do
        -- TODO: That was the original only test in this package. This test
        -- should be removed, and future versions of this library should return
        -- an 'Either ParseError Elf'.
        it "does not accept an empty elf" $
            evaluate (parseElf emptyContents) `shouldThrow` anyException

        let tinyElf = parseElf tinyContents
            bloatedElf = parseElf bloatedContents

        context "Headers parsing" $ do

            it "parses the version" $
                elfVersion tinyElf `shouldBe` 1

            it "parses the architecture" $ do
                elfClass tinyElf    `shouldBe` ELFCLASS64
                elfClass bloatedElf `shouldBe` ELFCLASS32

            it "parses the endianness" $ do
                elfData tinyElf `shouldBe` ELFDATA2LSB

            it "parses the OS ABI" $
                elfOSABI tinyElf `shouldBe` ELFOSABI_SYSV

            it "parses the type" $
                elfType bloatedElf `shouldBe` ET_EXEC

            it "parses the machine type" $ do
                elfMachine tinyElf    `shouldBe` EM_X86_64
                elfMachine bloatedElf `shouldBe` EM_386

            it "parses the entry point" $ do
                elfEntry tinyElf    `shouldBe` 0x4000e0
                elfEntry bloatedElf `shouldBe` 0x8048610

        context "Segment parsing" $ do
            let tinySegments    = elfSegments tinyElf
                bloatedSegments = elfSegments bloatedElf

            it "parses the right amount of segments" $ do
                length tinySegments    `shouldBe` 2
                length bloatedSegments `shouldBe` 9

            it "parses segment types" $
                let segmentTypes = map elfSegmentType tinySegments in
                segmentTypes `shouldBe` [PT_LOAD, PT_NOTE]

            it "parses segment flags" $ do
                let segmentFlags = map elfSegmentFlags tinySegments
                segmentFlags !! 0 `shouldMatchList` [PF_R, PF_X]
                segmentFlags !! 1 `shouldMatchList` [PF_R]
