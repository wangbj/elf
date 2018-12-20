module Data.ElfSpec (spec) where

import Test.Hspec

import Control.Exception (evaluate)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Either
import Data.Foldable (find)
import qualified Data.Map as Map
import Data.Maybe
import System.IO

import Data.Elf


getBinaryFileContents :: FilePath -> IO BS.ByteString
getBinaryFileContents fname = withBinaryFile fname ReadMode BS.hGetContents


spec :: Spec
spec = do
    emptyContents   <- runIO $ getBinaryFileContents "./testdata/empty"
    tinyContents    <- runIO $ getBinaryFileContents "./testdata/tiny"
    bloatedContents <- runIO $ getBinaryFileContents "./testdata/bloated"
    dynsymContents  <- runIO $ getBinaryFileContents "./testdata/vdso.elf"

    let tinyElf = parseElf tinyContents
        bloatedElf = parseElf bloatedContents
        dynsymElf  = parseElf dynsymContents

    describe "parseElf" $ do
        -- TODO: That was the original only test in this package. This test
        -- should be removed, and future versions of this library should return
        -- an 'Either ParseError Elf'.
        it "does not accept an empty elf" $
            evaluate (parseElf emptyContents) `shouldThrow` anyException

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

        context "Section parsing" $ do
            let tinySections    = elfSections tinyElf
                bloatedSections = elfSections bloatedElf

            it "parses the right amount of sections" $ do
                length tinySections    `shouldBe` 3
                length bloatedSections `shouldBe` 31

            it "parses the section in the right order" $ do
                map elfSectionName tinySections `shouldBe` [ "", ".text", ".shstrtab" ]

            it "parses the section types" $
                let sectionTypes = map elfSectionType bloatedSections in
                take 5 sectionTypes `shouldBe` [ SHT_NULL, SHT_PROGBITS, SHT_NOTE
                                               , SHT_NOTE, SHT_EXT 1879048182]

            it "parses the data" $
                let comment  = find (\sec -> elfSectionName sec == ".comment") bloatedSections
                    expected = C.pack . concat $ [ "GCC: (GNU) 6.3.1 20161221 (Red Hat 6.3.1-1)\NUL"
                                                 , "clang version 3.8.1 (tags/RELEASE_381/final)\NUL"
                                                 ]
                in
                fmap elfSectionData comment `shouldBe` Just expected

    describe "findSymbolDefinition" $ do
        let tinySymbols    = parseSymbolTables tinyElf
            bloatedSymbols = parseSymbolTables bloatedElf

        it "parses stripped symbol" $
            -- This binary was stripped
            concat tinySymbols `shouldSatisfy` all (isNothing . snd . steName)

        let namedBloatedSymbols =
                let go sym = fmap (\ name -> (name, sym)) $ snd (steName sym)
                in Map.fromList $ catMaybes $ map go $ concat bloatedSymbols

            member k = Map.member (C.pack k)
            (!?) m k = m Map.!? (C.pack k)

        it "parses symbol symbol names" $ do
            namedBloatedSymbols `shouldSatisfy` member "_init"
            namedBloatedSymbols `shouldSatisfy` member "main"

        let initSymbol  = namedBloatedSymbols !? "_init"
            fnameSymbol = namedBloatedSymbols !? "bloated.cpp"

        it "parses symbol address" $
            fmap steValue initSymbol `shouldBe` Just 0x0804850c

        it "parses symbol type" $ do
            fmap steType initSymbol  `shouldBe` Just STTFunc
            fmap steType fnameSymbol `shouldBe` Just STTFile
    describe "parse DynSym symbols" $ do
        let dynSymbols    = parseSymbolTables dynsymElf
        it "parses dyn symbol table" $ do
          dynSymbols `shouldNotBe` []
        it "parse (x86_64) vdso dyn symbols" $ do
          let dynSyms = concat dynSymbols
          filter (\e -> (snd . steName) e == (Just . C.pack) "__vdso_time") dynSyms `shouldNotBe` []
          filter (\e -> (snd . steName) e == (Just . C.pack) "__vdso_getcpu") dynSyms `shouldNotBe` []
          filter (\e -> (snd . steName) e == (Just . C.pack) "__vdso_clock_gettime") dynSyms `shouldNotBe` []
          filter (\e -> (snd . steName) e == (Just . C.pack) "__vdso_gettimeofday") dynSyms `shouldNotBe` []
