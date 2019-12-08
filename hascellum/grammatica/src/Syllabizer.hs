{-# LANGUAGE ExistentialQuantification, RecordWildCards, MultiWayIf  #-}
{-|

Module      : Syllabizer
Copyright   : (c) Robert Lee, 2019
License     : ISC

Maintainer  : robert.lee@chicago.vc
Stability   : None
Portability : non-portable (GHC extensions)

Description : Nomen grammatica

-}

{-
infixr 9  .
infixr 8  ^, ^^, ⋆⋆
infixl 7  ⋆, /, ‘quot‘, ‘rem‘, ‘div‘, ‘mod‘
infixl 6  +, -
infixr 6  <>
infixr 5  :, ++
infix  4  ==, /=, <, <=, >=, >
infixl 4  <$, <*>, <*, *>, <**>
infixr 3  &&
infixr 2  ||
infixl 1  ?, >>, >>=
infixr 1  =<<, <=<, >=>
infixr 0  $, $!, ‘seq‘

⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ Omega Symbol Key ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅
                   early or abnormal termination ⋅⋅⋅ Ω
                            termination (normal) ⋅⋅⋅ ω
                                    a new thread ⋅⋅⋅ ⋔
          code that can throw an error exception ⋅⋅⋅ ⏈
                                  loop-like code ⋅⋅⋅ ➿
                              a loop-like repeat ⋅⋅⋅ ↺
                           end of loop-like code ⋅⋅⋅ 🔚
               an uninterruptible exception mask ⋅⋅⋅ ☔
                code that can emit IO exceptions ⋅⋅⋅ ☢
                a warning about troublesome code ⋅⋅⋅ ⚠
  an imperative concerning imprudent code change ⋅⋅⋅ ⚡
                  a forbidden/nonsense condition ⋅⋅⋅ ⛞
                          a timed race condition ⋅⋅⋅ 🏁
⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅
-}

module Syllabizer

where

import Control.Monad (join, void)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as DL
import Data.Attoparsec.Text
import Control.Applicative ((<|>))

import Data.Either (isRight, fromRight)

-- End of imports ---------------------------------------------------------------------------------------------------------------------------------------

consonantes :: [] Text -- cōnsonāns cōnsonantēs f
consonantes = [ "qu", "ch", "ph", "th"
              , "b", "c", "d", "f", "g", "h", "k", "l", "m", "n", "p", "q", "r", "s", "t", "v", "x", "y", "z" -- , "i" -- requires backtracking.
              ]

vocales :: [] Text -- vōcālis vōcālēs f
vocales = ["ā", "ē", "ī", "ō", "ū", "a", "e", "i", "o", "u", "y"]

diphthongi :: [] Text -- diphthongus diphthongī f
diphthongi = ["ae", "au", "ei", "eu", "oe", "ui"]

liquids :: [] Text
liquids = ["l", "r"]

stops :: [] Text
stops = ["p", "b", "t", "d", "c", "g"]

stopLiquids :: [] Text
stopLiquids = do
  stop <- stops
  liquid <- liquids
  pure $ T.append stop liquid

parsedP :: forall b. Parser b -> T.Text -> Bool
parsedP parser = isRight . parseOnly parser

outerToken :: Text -> [SylPar]
outerToken x = fromRight [] $ parseOnly tokenIzer x

tokenToText :: [SylPar] -> Text
tokenToText x = sylparToText x

textToTexts :: Text -> [Text]
textToTexts t = fromRight [] $ parseOnly syllabizer2 t

textToSylparO :: Text -> [Text] -> [[SylPar]]
textToSylparO t ts = textToSylpar (outerToken t) ts

data SylPar = Vowel Text
            | Diphthong Text
            | Consonant Text
            | StopLiquid Text
              deriving (Show)

tokenIzer :: Parser [SylPar]
tokenIzer = do
  tokens <- (do early <- consonantalVowels
                tokens' <- tokes
                pure $  (early:[]) ++ tokens'
            ) <|> tokes

  endOfInput
  pure $ join tokens
  where
    tokes :: Parser [[SylPar]]
    tokes = many1 $ choice [diphthongs, consonantalVowelsI, vowels, stopLiquidy, consonants]

    consonantalVowels :: Parser [SylPar]
    consonantalVowels = do
      cv <- string "i" <|> string "ī"
      vowel <- choice $ map string vocales
      pure [Consonant cv, Vowel vowel]

    {- Between two vowels within a word i served in double capacity: as the vowel i forming a
       diphthong with the preceding vowel, and as the consonant like English y: reiectus ( = rei
       yectus) maior ( = mai yor), cuius ( = cui yus.) Otherwise it was usually a vowel.
    -}
    consonantalVowelsI :: Parser [SylPar] -- This needs more work to get it to run in the middle.
    consonantalVowelsI = do
      vowela <- choice $ map string vocales
      cv <- string "i" <|> string "ī"
      vowelb <- choice $ map string vocales
      pure [Vowel vowela, Consonant cv, Vowel vowelb]
           
    vowels      = (choice $ map string vocales    ) >>= pure . (:[]) . Vowel
    consonants  = (choice $ map string consonantes) >>= pure . (:[]) . Consonant
    diphthongs  = (choice $ map string diphthongi ) >>= pure . (:[]) . Diphthong
    stopLiquidy = (choice $ map string stopLiquids) >>= pure . (:[]) . StopLiquid

textToSylpar :: [SylPar] -> [Text] -> [[SylPar]]
textToSylpar sylpars texts = map mapF scans
  where
    mapF (d,t) = DL.take t $ DL.drop d sylpars
    lengths = map T.length texts
    scans = DL.scanl scanF (0,head lengths) (tail lengths)
    scanF (d,t) a = (d + t, a)

sylparToSyllables :: [[SylPar]] -> [Text]
sylparToSyllables sylpars = map (T.concat . map mapF) sylpars
  where
    mapF (Vowel      x) = x
    mapF (Consonant  x) = x
    mapF (Diphthong  x) = x
    mapF (StopLiquid x) = x

syllablesToText :: [Text] -> Text
syllablesToText ts = T.intercalate "-" ts

sylparToText :: [] SylPar -> Text
sylparToText xs = T.pack $ map mapF xs
  where
    mapF (Vowel      _) = 'V'
    mapF (Consonant  _) = 'C'
    mapF (Diphthong  _) = 'D'
    mapF (StopLiquid _) = 'S'

syllabizer2 :: Parser [Text]
syllabizer2 = do
  matched <- choice [ many1 $ choice [ cvcv, multiMidConsonant, midConsonant, conVowelCon, vowelCon, conVowelDip
                                     , stopLiquidVowelCons, stopLiquidVowel
                                     , dualVowel, vowelDip, vOrD
                                     ]
                    ]
  endOfInput
  pure $ join matched

  where
    cvcv :: Parser [Text]
    cvcv = string "CVCV" >> pure ["CV", "CV"]
    
    vowelCon :: Parser [Text]
    vowelCon = string "VC" >>= pure . (:[])

    stopLiquidVowelCons :: Parser [Text]
    stopLiquidVowelCons = string "SVC" >>= pure . (:[])

    stopLiquidVowel :: Parser [Text]
    stopLiquidVowel = string "SV" >>= pure . (:[])

    conVowelCon :: Parser [Text]
    conVowelCon = string "CVC" >>= pure . (:[])

    conVowelDip :: Parser [Text]
    conVowelDip = do
      void $ string "C"
      vd <- vowelOrDip
      pure [T.append "C" vd]

    vowelDip :: Parser [Text]
    vowelDip = string "VD" >> pure ["V","D"]

    vowelOrDip :: Parser Text
    vowelOrDip = string "V" <|> string "D"

    vOrD :: Parser [Text]
    vOrD = vowelOrDip >>= pure . (:[])

    dualVowel :: Parser [Text]
    dualVowel = string "VV" >> pure ["V","V"]

    midConsonant :: Parser [Text]
    midConsonant = string "VCV" >> pure ["V", "CV"]

    multiMidConsonant :: Parser [Text]
    multiMidConsonant = do
      void $ string "V"
      cs <- many1 (string "C") >>= pure . T.concat
      void $ string "V"
      pure [ T.append "V" $ T.init cs
           , "CV"
           ]




{-
pa-ter
mī-li-tēs
in-iū-ri-a
dī-vi-dō

mit-tō
tol-lō
-}
