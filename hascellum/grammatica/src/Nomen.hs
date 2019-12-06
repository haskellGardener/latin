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

-- Will need to build a sort based on Casus.

noMacron :: Text -> Text
noMacron input = T.map mapF input
  where
    mapF 'ā' = 'a'
    mapF 'ō' = 'o'
    mapF 'ē' = 'e'
    mapF 'ū' = 'u'
    mapF 'ī' = 'i'
    mapF a = a

scribeResPerfectumSingularis :: Nomen -> [] Text
scribeResPerfectumSingularis Nomen {..} = map resPerfectum $ filter ((== Singularis) . numerus) paradigma

scribeResPerfectumPluralis :: Nomen -> [] Text
scribeResPerfectumPluralis Nomen {..} = map resPerfectum $ filter ((== Pluralis) . numerus) paradigma

faceParadigma :: NDeclinationes -> Text -> Text -> Text -> NominisSpecies -> Genera -> Maybe Nomen
faceParadigma _ "" _ _ _ _ = Nothing
faceParadigma _ _ _ "" _ _ = Nothing
faceParadigma NomenTertia _ "" _ _ _ = Nothing
-- faceParadigma NomenTertia dictionariumArticulum articulumSuffixum genetivusRadix nomenSpecies = Nothing
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
      Nomen { paradigma  = map mapF $ filter filterF paradigma'
            , declinatio = ndecl
            , nomenGenus = genera_pp
            , ..
            }

    filterF :: Paradigma -> Bool
    filterF = (genera_pp ==) . genera
      
    mapF :: Paradigma -> Paradigma
    mapF Paradigma {..} = subMapF ndecl casus genera numerus
      where
        subMapF :: NDeclinationes -> Casus -> Genera -> Numeri -> Paradigma
        subMapF NomenSecunda Vocativus _ Singularis 
          = if |    "ius" `T.isSuffixOf` noMacron dictionariumArticulum
                 || "ium" `T.isSuffixOf` noMacron dictionariumArticulum
                 -> Paradigma { radix = genetivusRadix 
                              , resPerfectum =  T.append genetivusRadix "e"
                              , ..
                              }
               | otherwise
                 -> Paradigma { radix = genetivusRadix 
                              , resPerfectum = T.append genetivusRadix suffixum
                              , ..
                              }
        subMapF NomenQuinta Genetivus _ Singularis
          = if | "ies" `T.isSuffixOf` noMacron dictionariumArticulum
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
          = if | "ies" `T.isSuffixOf` noMacron dictionariumArticulum
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
                             , suffixum = "e"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Vocativus
                             , genera = Femininum
                             , numerus = Singularis
                             , radix = ""
                             , suffixum = "er"
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
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "īum"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "um"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "īum"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "um"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "īum"
                             , resPerfectum = ""
                             }
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
                             , suffixum = "ēn"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , radix = ""
                             , suffixum = "ē"
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
                             , suffixum = "ēbus"
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
