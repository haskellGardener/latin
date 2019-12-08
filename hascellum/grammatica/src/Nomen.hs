{-# LANGUAGE RecordWildCards, MultiWayIf  #-}
{-|

Module      : Nomen
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

module Nomen

where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as DL
-- import Data.Ord

data Casus
  = Nominativus | Vocativus | Accusativus | Genetivus | Dativus | Ablativus -- -- | Locativus
    deriving (Ord, Show, Eq, Enum)

data Genera
  = Masculinum | Femininum | Neutrum
    deriving (Ord, Show, Eq, Enum)

data Numeri
  = Singularis | Pluralis
    deriving (Ord, Show, Eq, Enum)

data NDeclinationes
  = NomenPrima | NomenSecunda | NomenTertia | NomenQuarta | NomenQuinta
    deriving (Ord, Show, Eq, Enum)

data NominisSpecies
  = Proprium | Appellativum | Collectivum
    deriving (Ord, Show, Eq, Enum)

data Nomen
  = Nomen { paradigma             :: [] Paradigma
          , declinatio            :: NDeclinationes
          , dictionariumArticulum :: Text           -- Nominativus Singularis dictionary entry
          , articulumSuffixum     :: Text           -- Nominativus Singularis dictionary ending
          , genetivusRadix        :: Text           -- Genetivus Singularis dictionary entry radix
          , nomenSpecies          :: NominisSpecies -- Default is Appellativum
          , nomenGenus            :: Genera
          }
    deriving (Show, Eq)

data Paradigma
  = Paradigma { casus           :: Casus
              , genera          :: Genera
              , numerus         :: Numeri
              , radix           :: Text
              , suffixum        :: Text
              , resPerfectum    :: Text
              }
    deriving (Show, Eq)

instance Ord Paradigma where
  compare a b = casus a `compare` casus b


{-
see: http://dcc.dickinson.edu/grammar/latin/3rd-declension-pure-i-stems-m-f
Third declension I-stem rules:

  1. Pure i-stems:

    a. Masculine and feminine parisyllabic (having the same number of syllables in the nominative and genitive singular)
       nouns in -is and four in -er.

    b. Neuters in -e, -al, and -ar.

  2. Mixed i-stems, declined in the singular like consonant stems, the plural like i-stems.


  3rd Declension: Pure I-stems, m. / f.
  
  66. Masculine and feminine parisyllabic nouns in -is form the nominative singular by adding s to
      the stem. Four stems in bri- and tri- do not add s to form the nominative, but drop i and insert e
      before r . These are imber, linter, ūter, venter.

  Pure I-stem, N
  3rd Declension: Pure I-stems, m. / f.
  3rd Declension: Mixed I-stem
  
  68. In neuters the nominative is the same as the stem, with final i changed to e (mare, stem
      mari-). But most nouns1 in which the i of the stem is preceded by -āl- or -ār- lose the final
      vowel and shorten the preceding ā.
  
  animăl, stem animāli-2
  
  a. Neuters in -e, -al, and -ar have -ī in the ablative singular, -ium in the genitive plural, and
     -ia in the nominative and accusative plural.
  
  animal, animālī, -ia, -ium

-}








                
labefaceApex :: Text -> Text
labefaceApex input = T.map mapF input
  where
    mapF 'ā' = 'a'
    mapF 'ō' = 'o'
    mapF 'ē' = 'e'
    mapF 'ū' = 'u'
    mapF 'ī' = 'i'
    mapF a = a

facApex :: Text -> Text
facApex input = T.map mapF input
  where
    mapF 'a' = 'ā'
    mapF 'o' = 'ō'
    mapF 'e' = 'ē'
    mapF 'u' = 'ū'
    mapF 'i' = 'ī'
    mapF a = a

scribeResPerfectumSingularis :: Nomen -> [] Text
scribeResPerfectumSingularis Nomen {..} = map resPerfectum $ DL.sort $ filter ((== Singularis) . numerus) paradigma

scribeResPerfectumPluralis :: Nomen -> [] Text
scribeResPerfectumPluralis Nomen {..} = map resPerfectum $ DL.sort $ filter ((== Pluralis) . numerus) paradigma

faceParadigma :: NDeclinationes -> Text -> Text -> Text -> NominisSpecies -> Genera -> Maybe Nomen
faceParadigma _ "" _ _ _ _ = Nothing
faceParadigma _ _ _ "" _ _ = Nothing
faceParadigma NomenTertia _ "" _ _ _ = Nothing
faceParadigma ndecl dictionariumArticulum articulumSuffixum genetivusRadix nomenSpecies genera_pp =
  case mdeclinatio of
    Nothing -> Nothing
    Just declinatio -> facParadigma $ nparadigma declinatio
  where
    mdeclinatio = DL.find ((ndecl ==) . ndeclinatio) declinationes
    facParadigma paradigma' =
      if | T.length dictionariumArticulum <= 1 -> Nothing
         | DL.null paradigma' -> Nothing
         | DL.any (\Paradigma {..}
                     -> casus   == Nominativus
                     && genera  == genera_pp
                     && numerus == Singularis
                  ) paradigma'
           -> Just $ finalize paradigma'
         | otherwise -> Nothing

    finalize paradigma' =
      Nomen { paradigma  = map mapF $ filter percolaF paradigma'
            , declinatio = ndecl
            , nomenGenus = genera_pp
            , ..
            }

    percolaF :: Paradigma -> Bool -- percōlō, percōlāre, percōlāve, percōlātum   (imperativus >> percōlā)
    percolaF = (genera_pp ==) . genera
      
    mapF :: Paradigma -> Paradigma
    mapF Paradigma {..} = subMapF ndecl casus genera numerus
      where
        subMapF :: NDeclinationes -> Casus -> Genera -> Numeri -> Paradigma
        subMapF NomenTertia Nominativus _ Singularis 
          = Paradigma { radix = ""
                      , resPerfectum = dictionariumArticulum
                      , ..
                      }
        subMapF NomenTertia Accusativus Neutrum Singularis 
          = Paradigma { radix = ""
                      , resPerfectum = dictionariumArticulum
                      , ..
                      }
        subMapF NomenSecunda Vocativus _ Singularis 
          = if |    "ius" `T.isSuffixOf` labefaceApex dictionariumArticulum
                 || "ium" `T.isSuffixOf` labefaceApex dictionariumArticulum
                 -> Paradigma { radix = genetivusRadix 
                              , resPerfectum =  T.append genetivusRadix "e"
                              , ..
                              }
               | otherwise
                 -> Paradigma { radix = genetivusRadix
                              , resPerfectum = dictionariumArticulum
                              , ..
                              }
        subMapF NomenQuinta Genetivus _ Singularis
          = if | "ies" `T.isSuffixOf` labefaceApex dictionariumArticulum
                 -> Paradigma { radix = genetivusRadix 
                              , resPerfectum = T.append genetivusRadix "ēī"
                              , suffixum = "ēī"
                              , ..
                              }
               | otherwise
                 -> Paradigma { radix = genetivusRadix 
                              , resPerfectum = T.append genetivusRadix "eī"
                              , suffixum = "eī"
                              , ..
                              }
        subMapF NomenQuinta Dativus _ Singularis
          = if | "ies" `T.isSuffixOf` labefaceApex dictionariumArticulum
                 -> Paradigma { radix = genetivusRadix 
                              , resPerfectum = T.append genetivusRadix "ēī"
                              , suffixum = "ēī"
                              , ..
                              }
               | otherwise
                 -> Paradigma { radix = genetivusRadix
                              , resPerfectum = T.append genetivusRadix "eī"
                              , suffixum = "eī"
                              , ..
                              }
        subMapF _ Nominativus _ Singularis
          = Paradigma { radix = ""
                      , resPerfectum = dictionariumArticulum
                      , ..
                      }
        subMapF _ _ _ _
          = Paradigma { radix = genetivusRadix
                      , resPerfectum = T.append genetivusRadix suffixum
                      , ..
                      }

data Declinatio =
  Declinatio { ndeclinatio :: NDeclinationes
             , nparadigma  :: [] Paradigma
             }

declinationes :: [] Declinatio
declinationes =
  [ Declinatio { ndeclinatio = NomenPrima
               , nparadigma =
                 [ Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "a"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "a"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ae"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ae"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "am"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "am"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "as"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "as"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ae"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ae"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ārum"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ārum"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ae"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ae"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "īs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "īs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ā"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ā"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "īs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "īs"
                             , resPerfectum = ""
                             }
                 ]
               }
  , Declinatio { ndeclinatio = NomenSecunda
               , nparadigma =
                 [ Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "us"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "er"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "um"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "a"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Vocativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = ""
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Vocativus
                             , genera = Femininum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = ""
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Vocativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "um"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Vocativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Vocativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Vocativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "a"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "um"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "um"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "um"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ōs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ōs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "a"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Neutrum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ōrum"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ōrum"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ōrum"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ō"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ō"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ō"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "īs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "īs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "īs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ō"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ō"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ō"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "īs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "īs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "īs"
                             , resPerfectum = ""
                             }
                 ]
               }
  , Declinatio { ndeclinatio = NomenTertia
               , nparadigma =
                 [ Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = ""
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = ""
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = ""
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ēs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ēs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "a"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "em"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "em"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = ""
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ēs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ēs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "a"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "is"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "is"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Neutrum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "is"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "um"
                             , resPerfectum = ""
                             }
                 -- , Paradigma { casus = Genetivus
                 --             , genera = Masculinum
                 --             , numerus = Pluralis
                 --             , radix = ""
                 --             , suffixum = "īum"
                 --             , resPerfectum = ""
                 --             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "um"
                             , resPerfectum = ""
                             }
                 -- , Paradigma { casus = Genetivus
                 --             , genera = Femininum
                 --             , numerus = Pluralis
                 --             , radix = ""
                 --             , suffixum = "īum"
                 --             , resPerfectum = ""
                 --             }
                 , Paradigma { casus = Genetivus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "um"
                             , resPerfectum = ""
                             }
                 -- , Paradigma { casus = Genetivus
                 --             , genera = Neutrum
                 --             , numerus = Pluralis
                 --             , radix = ""
                 --             , suffixum = "īum"
                 --             , resPerfectum = ""
                 --             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ibus"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ibus"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ibus"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "e"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "e"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "e"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ibus"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ibus"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ibus"
                             , resPerfectum = ""
                             }
                 ]
               }
  , Declinatio { ndeclinatio = NomenQuarta
               , nparadigma =
                 [ Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "us"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "us"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ū"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ūs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ūs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ua"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "um"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "um"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ū"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ūs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ūs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ua"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ūs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ūs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Neutrum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ūs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "uum"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "uum"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "uum"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "uī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "uī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ū"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ibus"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ibus"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ibus"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ū"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ū"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ū"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ibus"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ibus"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ibus"
                             , resPerfectum = ""
                             }
                 ]
               }
  , Declinatio { ndeclinatio = NomenQuinta
               , nparadigma =
                 [ Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ēs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ēs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ēs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ēs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "em"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "em"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ēs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ēs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "eī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "eī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ērum"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ērum"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ēī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ēī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ēbus"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ēbus"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ē"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "ē"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ēbus"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ēbus"
                             , resPerfectum = ""
                             }
                 ]
               }
  ]




{- For testing

NomenSecunda Masculinum
[ 
    ( "filius" 
    , "filiī" 
    ) 
, 
    ( "filie" 
    , "filiī" 
    ) 
, 
    ( "filium" 
    , "filiōs" 
    ) 
, 
    ( "filiī" 
    , "filiōrum" 
    ) 
, 
    ( "filiō" 
    , "filiīs" 
    ) 
, 
    ( "filiō" 
    , "filiīs" 
    ) 
]

NomenSecunda Masculinum
[ 
    ( "ager" 
    , "agrī" 
    ) 
, 
    ( "ager" 
    , "agrī" 
    ) 
, 
    ( "agrum" 
    , "agrōs" 
    ) 
, 
    ( "agrī" 
    , "agrōrum" 
    ) 
, 
    ( "agrō" 
    , "agrīs" 
    ) 
, 
    ( "agrō" 
    , "agrīs" 
    ) 
]

NomenSecunda Neutrum
[ 
    ( "bellum" 
    , "bella" 
    ) 
, 
    ( "bellum" 
    , "bella" 
    ) 
, 
    ( "bellum" 
    , "bella" 
    ) 
, 
    ( "bellī" 
    , "bellōrum" 
    ) 
, 
    ( "bellō" 
    , "bellīs" 
    ) 
, 
    ( "bellō" 
    , "bellīs" 
    ) 
] 

maybe [] scribeResPerfectumSingularis $ faceParadigma NomenSecunda "bellum" "ī" "bell" Appellativum Neutrum
[ "bellum" 
, "bellum" 
, "bellum" 
, "bellī" 
, "bellō" 
, "bellō" 
] 

maybe [] scribeResPerfectumPluralis    $ faceParadigma NomenSecunda "bellum" "ī" "bell" Appellativum Neutrum 
[ "bella" 
, "bella" 
, "bella" 
, "bellōrum" 
, "bellīs" 
, "bellīs" 
] 














NomenQuinta Masculinum Femininum
[ 
    ( "diēs" 
    , "diēs" 
    ) 
, 
    ( "diem" 
    , "diēs" 
    ) 
, 
    ( "diēī" 
    , "diērum" 
    ) 
, 
    ( "diēī" 
    , "diēbus" 
    ) 
, 
    ( "diē" 
    , "diēbus" 
    ) 
]

NomenQuinta Femininum
[ 
    ( "rēs" 
    , "rēs" 
    ) 
, 
    ( "rem" 
    , "rēs" 
    ) 
, 
    ( "reī" 
    , "rērum" 
    ) 
, 
    ( "reī" 
    , "rēbus" 
    ) 
, 
    ( "rē" 
    , "rēbus" 
    ) 
] 

NomenQuarta Femininum
[ 
    ( "manus" 
    , "manūs" 
    ) 
, 
    ( "manum" 
    , "manūs" 
    ) 
, 
    ( "manūs" 
    , "manuum" 
    ) 
, 
    ( "manuī" 
    , "manibus" 
    ) 
, 
    ( "manū" 
    , "manibus" 
    ) 
] 


NomenQuarta Neutrum
[ 
    ( "cornū" 
    , "cornua" 
    ) 
, 
    ( "cornū" 
    , "cornua" 
    ) 
, 
    ( "cornūs" 
    , "cornuum" 
    ) 
, 
    ( "cornū" 
    , "cornibus" 
    ) 
, 
    ( "cornū" 
    , "cornibus" 
    ) 
] 















-}
