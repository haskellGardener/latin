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
              , "b", "c", "d", "f", "g", "h", "k", "l", "m", "n", "p", "q", "r", "s", "t", "v", "x", "y", "z" -- , "i"
              ]

vocales :: [] Text -- vōcālis vōcālēs f
vocales = ["a", "e", "i", "o", "u", "y"]

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
  tokens <- many1 $ choice [diphthongs, vowels, stopLiquidy, consonants]
  endOfInput
  pure tokens
  where
    vowels      = (choice $ map string vocales    ) >>= pure . Vowel
    consonants  = (choice $ map string consonantes) >>= pure . Consonant
    diphthongs  = (choice $ map string diphthongi ) >>= pure . Diphthong
    stopLiquidy = (choice $ map string stopLiquids) >>= pure . StopLiquid

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

              
sylparToText :: [] SylPar -> Text
sylparToText xs = T.pack $ map mapF xs
  where
    mapF (Vowel      _) = 'V'
    mapF (Consonant  _) = 'C'
    mapF (Diphthong  _) = 'D'
    mapF (StopLiquid _) = 'S'

syllabizer2 :: Parser [Text]
syllabizer2 = do
  matched <- many1 $ choice [conVowelDip, stopLiquidVowelCons, stopLiquidVowel, conVowelCon, midConsonant, multiMidConsonant, dualVowel, vowelDip]
  -- endOfInput
  pure $ join matched

  where
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
                           
       
syllabizer :: Parser [Text]
syllabizer = do
  wheeee <- (many1 $ choice [pat1, conVowel, stopLiquidVowelCons, stopLiquidVowel, conVowelCon, conVowel, midConsonant, multiMidConsonant, dual])
            >>= pure . join
  endOfInput
  pure wheeee
  where
    -- vowDip :: Parser Text
    -- vowDip = vowels <|> diphthongs

    pat1 :: Parser [Text]
    pat1 = do slvc <- many1 stopLiquidVowel
              pure $ join slvc

    stopLiquidVowelCons :: Parser [Text]
    stopLiquidVowelCons = do
      stopLiquid <- choice $ map string stopLiquids
      vowel <- vowels
      consonant <- consonants
      pure [T.concat [stopLiquid, vowel, consonant]]

    stopLiquidVowel :: Parser [Text]
    stopLiquidVowel = do
      stopLiquid <- choice $ map string stopLiquids
      vowel <- vowels
      pure [T.append stopLiquid vowel]


    conVowelCon :: Parser [Text]
    conVowelCon = do cv <- conVowel >>= pure . T.concat
                     consonant <- consonants
                     pure [T.append cv consonant]

    conVowel :: Parser [Text]
    conVowel = do
      consonant <- consonants
      vowel <- vowels
      pure [T.append consonant vowel]


    dual :: Parser [Text]
    dual = choice [vowelDip, dualVowel]
    -- dipVowel = do vowel1 <- vowel
    --               diphthong1 <- diphthong
    --               pure [T.singleton vowel1, diphthong1]

    vowelDip = do diphthong <- diphthongs
                  vowel <- vowels
                  pure [diphthong, vowel]

    dualVowel = do vowel1 <- vowels
                   vowel2 <- vowels
                   pure [vowel1, vowel2]

    vowels     = choice $ map string vocales
    consonants = choice $ map string consonantes
    diphthongs = choice $ map string diphthongi


    midConsonant :: Parser [Text]
    midConsonant =
      do vowel1 <- vowels
         consonant <- consonants
         vowel2 <- vowels
         pure [vowel1, T.append consonant vowel2]

    multiMidConsonant :: Parser [Text]
    multiMidConsonant =
      do vowel1 <- vowels
         consonantSequence <- many1 consonants
         vowel2 <- vowels
         pure [ T.append vowel1 (T.concat $ init consonantSequence)
              , T.concat [last consonantSequence, vowel2]
              ]




{-
pa-ter
mī-li-tēs
in-iū-ri-a
dī-vi-dō

mit-tō
tol-lō
-}
