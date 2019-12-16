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

import Control.Monad (join)
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
diphthongi = ["ae", "au", "ei", "eu" {- rarely: see de-us -}, "oe", "ui"]

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
              deriving (Show, Eq)

sylText :: SylPar -> Text
sylText (Vowel      x) = x
sylText (Consonant  x) = x
sylText (Diphthong  x) = x
sylText (Absolute   x) = x
sylText (StopLiquid x) = x

isAbsolute :: SylPar -> Bool
isAbsolute (Absolute _) = True
isAbsolute _ = False

isConsonant :: SylPar -> Bool
isConsonant (Consonant _) = True
isConsonant _ = False

isStopLiquid :: SylPar -> Bool
isStopLiquid (StopLiquid _) = True
isStopLiquid _ = False

packEndConsonants :: [] SylPar -> [] SylPar
packEndConsonants toks
  | packed == Consonant T.empty = toks
  | otherwise                   = rest ++ (packed:[])
  where
    endCs = reverse . DL.takeWhile isConsonant $ reverse toks
    rest = DL.take (length toks - length endCs) toks
    packed = Consonant . T.concat $ map (\(Consonant x) -> x) endCs

packStartConsonants :: [] SylPar -> [] SylPar -- consider packing trailing Consanants after starting Absolute
packStartConsonants toks
  | packed == Consonant T.empty = toks
  | otherwise                   = absolutes ++ packed:rest
  where
-- process absolute and readd after packing.
    eitherSC x = isStopLiquid x || isConsonant x
    absolutes  = DL.takeWhile isAbsolute toks
    remToks    = DL.dropWhile isAbsolute toks
    begCs      = DL.takeWhile eitherSC remToks
    rest       = DL.dropWhile eitherSC remToks
    packed     = Consonant . T.concat $ map sylText begCs

tokenIzer :: Parser [SylPar]
tokenIzer = do
  tokens <- alx <|>
            (do early <- choice [ consonantalVowels, ab
                                , auto, auri, aux, au
                                , appl, appr, apt, app, ap
                                ]
                tokens' <- tokes
                pure $  (early:[]) ++ tokens'
            ) <|> tokes

  endOfInput
  pure . packStartConsonants . packEndConsonants $ join tokens
  where
    tokes :: Parser [[SylPar]]
    tokes = many1 $ choice [chi, eus, eum, quae, qua, quen, que, qui, quo, quu, diphthongs, consonantalVowelsI, vowels, stopLiquidy, consonants]

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

    -- much of the following are specializations, and can get tedious.
    -- ab is intolerable as it could be a-b... or ab-... The word after 'a' or 'ab' may be an actual word elsewhere.
    ab   = string "ab"   >> pure [Absolute "ab"]
    ap   = string "ap"   >> pure [Absolute "a", Consonant "p"]
    app  = string "app"  >> pure [Vowel "a", Consonant "p", Consonant "p"]
    apt  = string "apt"  >> pure [Vowel "a", Consonant "p", Consonant "t"]
    appl = string "appl" >> pure [Vowel "a", Consonant "p", StopLiquid "pl"]
    appr = string "appr" >> pure [Vowel "a", Consonant "p", StopLiquid "pr"]
    quu  = string "quu"  >> pure [Absolute "quu"]
    quo  = string "quo"  >> pure [Absolute "quo"]
    qui  = string "qui"  >> pure [Absolute "qui"]
    que  = string "que"  >> pure [Absolute "que"]
    quen = string "quen" >> pure [Absolute "quen"] -- UGLY. The system should better understand that the ue is a Vowel, but is inseparable from 'q'
    qua  = string "qua"  >> pure [Absolute "qua"]
    quae = string "quae" >> pure [Absolute "quae"]
    eum  = string "eum"  >> pure [Vowel "e", Absolute "um"]
    eus  = string "eus"  >> pure [Vowel "e", Absolute "us"]
    auri = string "auri" >> pure [Vowel "au", Absolute "ri"]
    auto = string "auto" >> pure [Absolute "au", Absolute "to"]
    aux  = string "aux"  >> pure [Absolute "aux"]
    chi  = string "chi"  >> pure [Absolute "chi"]
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
sylparToSyllables sylpars = map (T.concat . map sylText) sylpars

syllablesToText :: [Text] -> Text
syllablesToText ts = T.intercalate "-" ts

sylparToText :: [] SylPar -> Text
sylparToText xs = T.pack $ map mapF xs
  where
    mapF (Vowel      _) = 'V'
    mapF (Consonant  _) = 'C'
    mapF (Diphthong  _) = 'V'
    mapF (Absolute   _) = 'A'
    mapF (StopLiquid _) = 'S'

syllabizer2 :: Parser [Text]
syllabizer2 = do
  matched <- (do x <- many1 general
                 endOfInput
                 pure x
             ) <|>
             (do x <- many1 general2
                 endOfInput
                 pure x
             )

  pure $ join matched
  where
    general = choice [ absolute
                     , cvac -- coquus
                     , cvvcccvv
                     , cvccccvc
                     , cvcvcccvv
                     , cvccsvccvc
                     , cvccvccsvc
                     , cvccsvccvv
                     , cvsvvcv
                     , cvcccvvc
                     , cvcvccsvc
                     , mcvC
                     , mcv
                     , mvc
                     , vccvcv
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
                     , vccvcvcc -- Must have for accidens
                     , cvccvccsvccvv
                     , cvcccvsvcvv
                     , cvccvccsvccvc
                     , cvcaccvcvv
                     , cvcaccvc
                     , cvcccvv
                     , cvcaccvv
                     , cvcvcccvc
                     , cvcvaccvc
                     , cvcvaccv
                     , cvaccvc
                     , cvaccv   -- cachinnatio
                     , cvcccvc
                     , cvcccvcvc
                     , vcvcvvc
                     , vcvcc
                     , cvc
                     , vc
                     , cv
                     , stopLiquidVowelCons
                     , stopLiquidVowel
                     , singleVowel
                     ]

    general2 = choice [ absolute
                      , vac
                      , endCvvc
                      , cvccsv
                      , vccvcvcvvc
                      , vccvcvccvc
                      , vccvcvcvv
                      , vcccvccvv
                      , vcvccvcvc
                      , vcsvcvcvc
                      , vsvcvcvvc
                      , vcccvccvc
                      , vccvcccvv
                      , cvcvaccvc
                      , vcvcvvc
                      , cvcvaccv
                      , cvcaccvv
                      , cvcccvc
                      , cvccsvccvc
                      , cvccvccsvccvv
                      , cvccvccsvccvc
                      , cvcaccvcvv
                      , cvcccvsvcvv
                      , cvcccvv
                      , cvcaccvc
                      , cvcvcccvc
                      , cvcccvcvc
                      , cvaccvc
                      , cvaccv
                      , cvcvc
                      , vcvcc
                      , cvcv
                      , vcc -- This is a mess, and should only apply as a last resort.
                      , cvc
                      , vc
                      , cv
                      , stopLiquidVowelCons
                      , stopLiquidVowel
                      , singleVowel
                      ]

    absolute :: Parser [Text]
    absolute = string "A" >> pure ["A"]
    cvaccv        = string "CVACCV"        >> pure ["CV","AC","CV"]
    cvaccvc       = string "CVACCVC"       >> pure ["CV","AC","CVC"]
    cvcccvcvc     = string "CVCCCVCVC"     >> pure ["CVCC","CV","CVC"]             -- camptaules
    cvcccvc       = string "CVCCCVC"       >> pure ["CVCC","CVC"]                  -- campter
    cvcvcccvc     = string "CVCVCCCVC"     >> pure ["CV","CVC","CCVC"]             -- catarrhus

    cvcvaccvc     = string "CVCVACCVC"     >> pure ["CV","CV","AC","CVC"]          -- catechismus
    cvcvaccv      = string "CVCVACCV"      >> pure ["CV","CV","AC","CV"]           -- catechista
    cvcccvv       = string "CVCCCVV"       >> pure ["CV","CC","CV","V"]            -- coemptio

    cvcccvsvcvv   = string "CVCCCVSVCVV"   >> pure ["CVC","CCV","SV","CV","V"]     -- conflagratio
    cvcaccvcvv    = string "CVCACCVCVV"    >> pure ["CVC","AC","CV","VC","V"]      -- conquassatio /kon.kʷasˈsaː.ti.oː/

    cvcaccvv      = string "CVCACCVV"      >> pure ["CVC","AC","CV","V"]           -- conquestio
    cvcaccvc      = string "CVCACCVC"      >> pure ["CVC","AC","CVC"]              -- conquestus
    cvccsvccvv    = string "CVCCSVCCVV"    >> pure ["CVC","CSVC","CV","V"]         -- conscriptio
    cvccsvccvc    = string "CVCCSVCCVC"    >> pure ["CVC","CSVC","CVC"]            -- conscriptor
    cvccvccsvc    = string "CVCCVCCSVC"    >> pure ["CVC","CVCC","SVC"]            -- contemptrix
    cvac          = string "CVAC"          >> pure ["CV","AC"]                     -- coquus
    
    cvccvccsvccvv = string "CVCCVCCSVCCVV" >> pure ["CVC","CVC","CSVC", "CV", "V"] -- circumscriptio
    cvccvccsvccvc = string "CVCCVCCSVCCVC" >> pure ["CVC","CVC","CSVC", "CVC"]     -- circumscriptor

    cvccccvc      = string "CVCCCCVC"      >> pure ["CVCC", "CCVC"]                -- darmstadtium
    cvcvccsvc     = string "CVCVCCSVC"     >> pure ["CV","CVC","CSVC"]             -- defenstrix

    cvcvcccvv     = string "CVCVCCCVV"     >> pure ["CV","CVCC","CV","V"]          -- defunctio
    cvvcccvv      = string "CVVCCCVV"      >> pure ["CV","CV","CCV","V"]           -- diarrhoea


                    
    cvcccvvc      = string      "CVCCCVVC" >> pure ["CVCC","CV","VC"]               -- a-parctias
    endCvvc       = string          "CVVC" >> pure ["CV","VC"]
    cvccsv        = string        "CVCCSV" >> pure ["CVCC","SV"]                    -- borchgravius
    vccvcv        = string        "VCCVCV" >> pure ["VC","CV", "CV"]
    cvsvvcv       = string       "CVSVVCV" >> pure ["CV","SV","V","CV"]             -- special for bibliothe...
    vccvcvcvvc    = string    "VCCVCVCVVC" >> pure ["VC","CV","CV","CV","VC"]       -- anteloquium /an.teˈlo.kʷi.um/
    vccvcvccvc    = string    "VCCVCVCCVC" >> pure ["VC","CV","CVC","CVC"]          -- architectus
    vsvcvcvvc     = string     "VSVCVCVVC" >> pure ["V","SV","CV","CV","VC"]        -- acrocorium
    vcvccvcvc     = string     "VCVCCVCVC" >> pure ["V","CVC","CV","CVC"]           -- abortivum a.borˈtiː.wum/
    vcsvcvcvc     = string     "VCSVCVCVC" >> pure ["VC","SV","CV","CVC"]
    vccvcvcvv     = string     "VCCVCVCVV" >> pure ["VC","CV","CV","CV","V"]        -- abdicatio
    vccvcccvv     = string     "VCCVCCCVV" >> pure ["VC","CVCC","CV","V"]           -- assumptio
    vcccvccvv     = string     "VCCCVCCVV" >> pure ["VC","CCVC","CV","V"]           -- abscessio
    vcccvccvc     = string     "VCCCVCCVC" >> pure ["VC","VCVC","CVC"]              -- afflictor
    vccvcvcc      = string      "VCCVCVCC" >> pure ["VC","CV","CVCC"]               -- accidens
    vcvcvvc       = string       "VCVCVVC" >> pure ["V","CV","CV","VC"]             -- abigeus
    vcvcc         = string         "VCVCC" >> pure ["V","CVCC"]                     -- amans /ˈa.mans/
    cvcvc         = string         "CVCVC" >> pure ["CV","CVC"]                     -- pater
    cvcv          = string          "CVCV" >> pure ["CV","CV"]
    vcc           = string           "VCC" >> pure ["VCC"]                          -- ars
    vac           = string           "VAC" >> pure ["V","AC"]
    cvc           = string           "CVC" >> pure ["CVC"]
    cv            = string            "CV" >> pure ["CV"]
    vc            = string            "VC" >> pure ["VC"]
    singleVowel   = string             "V" >> pure ["V"]

    mcvC = do -- prettifier
      mcv' <- mcv
      _ <- string "C"
      pure $ init mcv' ++ ["CVC"]

    mcv = many1 $ string "CV" -- prettifier

    mvc = many1 $ string "VC" -- prettifier

    stopLiquidVowelCons :: Parser [Text]
    stopLiquidVowelCons = string "SVC" >>= pure . (:[])

    stopLiquidVowel :: Parser [Text]
    stopLiquidVowel = string "SV" >>= pure . (:[])
{-
pa-ter
mī-li-tēs
MĪLITĒS
in-iū-ri-a
dī-vi-dō

mit-tō
tol-lō
-}
