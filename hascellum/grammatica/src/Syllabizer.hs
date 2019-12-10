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

-- | N.B. Latin is a human language, and like all human languages it will be a bit messy.


{-
 see https://en.wikipedia.org/wiki/Latin_spelling_and_pronunciation


Syllable

To determine stress, syllable weight of the penult must be determined. To determine syllable weight,
words must be broken up into syllables.[57] In the following examples, syllable structure is
represented using these symbols: C (a consonant), K (a stop), R (a liquid), and V (a short vowel),
VV (a long vowel or diphthong).

Nucleus

Every short vowel, long vowel, or diphthong belongs to a single syllable. This vowel forms the
syllable nucleus. Thus magistrārum has four syllables, one for every vowel (a i ā u: V V VV V),
aereus has three (ae e u: VV V V), tuō has two (u ō: V VV), and cui has one (ui: VV).[58]

Onset and coda

A consonant before a vowel, or a consonant cluster at the beginning of a word, is placed in the same
syllable as the following vowel. This consonant or consonant cluster forms the syllable onset.[58]

    fēminae /feː.mi.nae̯/ (CVV.CV.CVV)
    vidēre /wi.deː.re/ (CV.CVV.CV)
    puerō /pu.e.roː/ (CV.V.CVV)
    beātae /be.aː.tae̯/ (CV.VV.CVV)
    graviter /ɡra.wi.ter/ (CCV.CV.CVC)
    strātum /straː.tum/ (CCCVV.CVC)

After this, if there is an additional consonant inside the word, it is placed at the end of the
syllable. This consonant is the syllable coda. Thus if a consonant cluster of two consonants occurs
between vowels, they are broken up between syllables: one goes with the syllable before, the other
with the syllable after.[59]

    puella /pu.el.la/ (CV.VC.CV)
    supersum /su.per.sum/ (CV.CVC.CVC)
    coāctus /ko.aːk.tus/ (CV.VVC.CVC)
    intellēxit /in.tel.leːk.sit/ (VC.CVC.CVVC.CVC)

There are two exceptions. A consonant cluster of a stop p t c b d g followed by a liquid l r between
vowels usually goes to the syllable after it, although it is also sometimes broken up like other
consonant clusters.[59]

    volucris /wo.lu.kris/ or /wo.luk.ris/ (CV.CV.KRVC or CV.CVK.RVC)

Heavy and light syllables

As shown in the examples above, Latin syllables have a variety of possible structures. Here are some
of them. The first four examples are light syllables, and the last six are heavy. All syllables have
at least one V (vowel). A syllable is heavy if it has another V or a VC after the first V. In the
table below, the extra V or VC is Upper, indicating that it makes the syllable heavy.

      v
    c v
  c c v
c c c v
    c v V
    c v C
    c v V C
      v V
      v C
      v V C

Thus, a syllable is heavy if it ends in a long vowel or diphthong, a short vowel and a consonant, a
long vowel and a consonant, or a diphthong and a consonant. Syllables ending in a diphthong and
consonant are rare in Classical Latin.

The syllable onset has no relationship to syllable weight; both heavy and light syllables can have
no onset or an onset of one, two, or three consonants.

In Latin a syllable that is heavy because it ends in a long vowel or diphthong is traditionally
called syllaba nātūrā longa ('syllable long by nature'), and a syllable that is heavy because it
ends in a consonant is called positiōne longa ('long by position'). These terms are translations of
Greek συλλαβὴ μακρά φύσει (syllabḕ makrá phýsei = 'syllable long by nature') and μακρὰ θέσει (makrà
thései = 'long by proposition'), respectively; therefore positiōne should not be mistaken for
implying a syllable "is long because of its position/place in a word" but rather "is treated as
'long' by convention". This article uses the words heavy and light for syllables, and long and short
for vowels since the two are not the same.[59]

 -}

consonantes :: [] Text -- cōnsonāns cōnsonantēs f
consonantes = [ "qu", "ch", "ph", "th"
              , "b", "c", "d", "f", "g", "h", "k", "l", "m", "n", "p", "q", "r", "s", "t", "v", "x", "y", "z" -- , "i" -- requires backtracking.
              ]
           ++ [ "QU", "CH", "PH", "TH"
              , "B", "C", "D", "F", "G", "H", "K", "L", "M", "N", "P", "Q", "R", "S", "T", "V", "X", "Y", "Z"
              ]

vocales :: [] Text -- vōcālis vōcālēs f
vocales = ["ā", "ē", "ī", "ō", "ū", "a", "e", "i", "o", "u", "y", "ă", "ĕ", "ĭ", "ŏ", "ŭ", "y̆"]
       ++ ["Ā", "Ē", "Ī", "Ō", "Ū", "A", "E", "I", "O", "U", "Y", "Ă", "Ĕ", "Ĭ", "Ŏ", "Ŭ", "Y̆"]

diphthongi :: [] Text -- diphthongus diphthongī f
diphthongi = ["ae", "au", "ei", "eu" {- sometimes: see deus -}, "oe", "ui"]

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
            | Absolute Text
            | Consonant Text
            | StopLiquid Text
              deriving (Show)

tokenIzer :: Parser [SylPar]
tokenIzer = do
  tokens <- alx <|>
            (do early <- consonantalVowels <|> ab <|> auto <|> auri <|> aux <|> au
                tokens' <- tokes
                pure $  (early:[]) ++ tokens'
            ) <|> tokes

  endOfInput
  pure $ join tokens
  where
    tokes :: Parser [[SylPar]]
    tokes = many1 $ choice [eus, eum, quae, qua, que, qui, quo, quu, diphthongs, consonantalVowelsI, vowels, stopLiquidy, consonants]

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
    -- ab is intolerable as it could be a-b... or ab-... The word after 'a' or 'ab' may be an actual word elsewhere.
    ab   = string "ab"   >> pure [Absolute "ab"]
    quu  = string "quu"  >> pure [Absolute "quu"]
    quo  = string "quo"  >> pure [Absolute "quo"]
    qui  = string "qui"  >> pure [Absolute "qui"]
    que  = string "que"  >> pure [Absolute "que"]
    qua  = string "qua"  >> pure [Absolute "qua"]
    quae = string "quae" >> pure [Absolute "quae"]
    eum  = string "eum"  >> pure [Vowel "e", Absolute "um"]
    eus  = string "eus"  >> pure [Vowel "e", Absolute "us"]
    auri = string "auri" >> pure [Vowel "au", Absolute "ri"]
    auto = string "auto" >> pure [Absolute "au", Absolute "to"]
    aux  = string "aux"  >> pure [Absolute "aux"]
    au   = string "au"   >> pure [Vowel "au"]

    alx  = string "alx"  >> pure [[Absolute "alx"]]

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
    mapF (Diphthong  x) = x -- Maybe recognize hiatus as well. Very tricky. e.g. 'deus' which is bisyllabe per diaersin
    mapF (Absolute   x) = x -- used for prefixes and suffixes that should not be treated as part of the rest.
    mapF (StopLiquid x) = x

syllablesToText :: [Text] -> Text
syllablesToText ts = T.intercalate "-" ts

sylparToText :: [] SylPar -> Text
sylparToText xs = T.pack $ map mapF xs
  where
    mapF (Vowel      _) = 'V'
    mapF (Consonant  _) = 'C'
    mapF (Diphthong  _) = 'D'
    mapF (Absolute   _) = 'A'
    mapF (StopLiquid _) = 'S'

syllabizer2 :: Parser [Text]
syllabizer2 = do
  matched <- choice [ many1 $ choice [ absolute
                                     , vac
                                     , endCvvc
                                     , vccvcvcvvc
                                     , vccvcvccvc
                                     , vccvcvcvv
                                     , vcccvccvv
                                     , vcvccvcvc
                                     , vcsvcvcvc
                                     , vsvcvcvvc
                                     , vcccvccvc
                                     , vccvcccvv
                                     , vcvcvvc
                                     , vcvcdc
                                     , cvcvc
                                     , vcvcc
                                     , vccdc
                                     , dcvcc
                                     , dccvv
                                     , vcsd
                                     , vcvc
                                     , cvcv
                                     , vcc
  
                                     , conVowelCon
                                     , vowelCon
                                     , conVowelDip
                                     , stopLiquidVowelCons
                                     , stopLiquidVowel
                                     , vOrD
                                     ]
                    ]
  endOfInput
  pure $ join matched

  where
    absolute :: Parser [Text]
    absolute = string "A" >> pure ["A"]

    endCvvc = string "CVVC" >> pure ["CV","VC"]
--    vccvcv    = string    "VCCVCV" >> pure ["VC","CV","CV"]       -- anteloquium /an.teˈlo.kʷi.um/
               
    vccvcvcvvc    = string    "VCCVCVCVVC" >> pure ["VC","CV","CV","CV","VC"]       -- anteloquium /an.teˈlo.kʷi.um/
    vccvcvccvc    = string    "VCCVCVCCVC" >> pure ["VC","CV","CVC","CVC"]          -- architectus
    vsvcvcvvc     = string     "VSVCVCVVC" >> pure ["V","SV","CV","CV","VC"]        -- acrocorium
    vcvccvcvc     = string     "VCVCCVCVC" >> pure ["V","CVC","CV","CVC"]           -- abortivum a.borˈtiː.wum/
    vcsvcvcvc     = string     "VCSVCVCVC" >> pure ["VC","SV","CV","CVC"]
    vccvcvcvv     = string     "VCCVCVCVV" >> pure ["VC","CV","CV","CV","V"]        -- abdicatio
    vccvcccvv     = string     "VCCVCCCVV" >> pure ["VC","CVCC","CV","V"]           -- assumptio
    vcccvccvv     = string     "VCCCVCCVV" >> pure ["VC","CCVC","CV","V"]           -- abscessio
    vcccvccvc     = string     "VCCCVCCVC" >> pure ["VC","VCVC","CVC"]              -- afflictor
--    vcvccvcv      = string      "VCVCCVCV" >> pure ["VC","VC","CV","CV"]
--    vccvcvcc      = string      "VCCVCVCC" >> pure ["VC","CV","CVCC"]               -- accidens
--    vcccvccv      = string      "VCCCVCCV" >> pure ["VC","CCVC","CV"]               -- abscissa
--    vacvccdc      = string      "VACVCCDC" >> pure ["V","A","CVC","CDC"]            -- aquilunguis
    vcvcvvc       = string       "VCVCVVC" >> pure ["V","CV","CV","VC"]             -- abigeus
--    vcvccvc       = string       "VCVCCVC" >> pure ["VC","VC","CVC"]                -- abactor
--    vccdccv       = string       "VCCDCCV" >> pure ["VC","CDC","CV"]                -- anguilla /anˈɡʷiːl.la/
--    vsvcvc        = string        "VSVCVC" >> pure ["V","SV","CVC"]                 -- ablātor /abˈlaː.tor/
    vcvcdc        = string        "VCVCDC" >> pure ["V","CV","CDC"]                 -- aculeus
--    vcdcvc        = string        "VCDCVC" >> pure ["V","CD","CVC"]                 -- acoetis
--    vcccvc        = string        "VCCCVC" >> pure ["VC","CCVC"]                    -- anthrax
--    vvccv         = string         "VVCCV" >> pure ["V","VC","CV"]                  -- aorta
    vcvcc         = string         "VCVCC" >> pure ["V","CVCC"]                     -- amans /ˈa.mans/
--    vccvc         = string         "VCCVC" >> pure ["VC","CVC"]                     -- abbas
    vccdc         = string         "VCCDC" >> pure ["VC","CDC"]                     -- anguis
    dcvcc         = string         "DCVCC" >> pure ["D","CVCC"]                     -- auceps
    dccvv         = string         "DCCVV" >> pure ["DC","CV","V"]                  -- auctio
    cvcvc         = string         "CVCVC" >> pure ["CV","CVC"]                     -- pater
    vcvc          = string          "VCVC" >> pure ["VC","VC"]                      -- acor
    vcsd          = string          "VCSD" >> pure ["VC","SD"]                      -- ancrae /ˈan.krae̯/
    cvcv          = string          "CVCV" >> pure ["CV","CV"]
    vcc           = string           "VCC" >> pure ["VCC"]                          -- ars
    vac           = string           "VAC" >> pure ["V","AC"]

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

    vowelOrDip :: Parser Text
    vowelOrDip = string "V" <|> string "D"

    vOrD :: Parser [Text]
    vOrD = vowelOrDip >>= pure . (:[])




{-
pa-ter
mī-li-tēs
MĪLITĒS
in-iū-ri-a
dī-vi-dō

mit-tō
tol-lō
-}
