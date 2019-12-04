{-# LANGUAGE RecordWildCards, MultiWayIf  #-}


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

data NominisGenus
  = Proprium | Appellativum | Collectivum
    deriving (Ord, Show, Eq, Enum)

data Nomen
  = Nomen { paradigma           :: [] Paradigma
          , declinatio          :: NDeclinationes
          , lexiconArticulum    :: Text         -- Nominativus Singularis dictionary entry
          , articulumTerminatur :: Text         -- Nominativus Singularis dictionary ending
          , genetivusStem       :: Text         -- Genetivus Singularis dictionary entry stem
          , nomenGenera         :: NominisGenus -- Default is Appellativum
          }
    deriving (Show, Eq)

data Paradigma
  = Paradigma { casus      :: Casus
              , genera     :: Genera
              , numerus    :: Numeri
              , stem       :: Text
              , terminatur :: Text
              , resPerfectum :: Text
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

faceParadigma :: NDeclinationes -> Text -> Text -> Text -> NominisGenus -> Maybe Nomen
faceParadigma _ "" _ _ _ = Nothing
faceParadigma _ _ _ "" _ = Nothing
faceParadigma NomenTertia _ "" _ _ = Nothing
-- faceParadigma NomenTertia lexiconArticulum articulumTerminatur genetivusStem nomenGenera = Nothing
faceParadigma ndecl lexiconArticulum articulumTerminatur genetivusStem nomenGenera =
  case mdeclinatio of
    Nothing -> Nothing
    Just declinatio -> facParadigma $ nparadigma declinatio
  where
    mdeclinatio = DL.find ((ndecl ==) . ndeclinatio) declinationes
    facParadigma paradigma' =
      if | T.length lexiconArticulum <= 1 -> Nothing
         | DL.null paradigma' -> Nothing
         | DL.any (\Paradigma {..}
                     -> casus   == Nominativus
                     && genera  == Masculinum
                     && numerus == Singularis
                     && noMacron terminatur `T.isSuffixOf` lexiconArticulum
                  ) paradigma'
           -> Just $ finalize paradigma'
         | otherwise -> Nothing

    finalize paradigma' =
      Nomen { paradigma  = map mapF paradigma'
            , declinatio = ndecl
            , ..
            }

    mapF :: Paradigma -> Paradigma
    mapF Paradigma {..} |    casus   == Nominativus
                          && genera  == Masculinum
                          && numerus == Singularis
                          = Paradigma { stem = ""
                                      , resPerfectum = lexiconArticulum
                                      , ..
                                      }
                        | otherwise = Paradigma { stem = genetivusStem
                                                , resPerfectum = T.append genetivusStem terminatur
                                                , ..
                                                }

data Declinatio = Declinatio { ndeclinatio :: NDeclinationes
                             , nparadigma :: [] Paradigma
                             }

declinationes :: [] Declinatio
declinationes =
  [ Declinatio { ndeclinatio = NomenPrima
               , nparadigma =
                 [ Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "a"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "a"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ae"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ae"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "am"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "am"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "as"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "as"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ae"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ae"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ārum"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ārum"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ae"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ae"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "īs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "īs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ābus"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ābus"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ā"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ā"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "īs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "īs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ābus"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ābus"
                             , resPerfectum = ""
                             }
                 ]
               }
  , Declinatio { ndeclinatio = NomenSecunda
               , nparadigma =
                 [ Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "us"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "er"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "um"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "a"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Vocativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "e"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Vocativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "er"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Vocativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "um"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Vocativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Vocativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Vocativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "a"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "um"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "um"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "um"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ōs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ōs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "a"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ōrum"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ōrum"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ōrum"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ō"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ō"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ō"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "īs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "īs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "īs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ō"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ō"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ō"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "īs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "īs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "īs"
                             , resPerfectum = ""
                             }
                 ]
               }
  , Declinatio { ndeclinatio = NomenTertia
               , nparadigma =
                 [ Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = ""
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = ""
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = ""
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ēs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ēs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "a"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "em"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "em"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = ""
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ēs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ēs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "a"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "is"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "is"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "is"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "um"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "īum"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "um"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "īum"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "um"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "īum"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ibus"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ibus"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ibus"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "e"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "e"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "e"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ibus"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ibus"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ibus"
                             , resPerfectum = ""
                             }
                 ]
               }
  , Declinatio { ndeclinatio = NomenQuarta
               , nparadigma =
                 [ Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "us"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "us"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ū"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ūs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ūs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ua"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "um"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "um"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ū"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ūs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ūs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ua"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ūs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ūs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ūs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "uum"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "uum"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "uum"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "uī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "uī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ū"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ibus"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ibus"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ibus"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ū"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ū"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ū"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ibus"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ibus"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ibus"
                             , resPerfectum = ""
                             }
                 ]
               }
  , Declinatio { ndeclinatio = NomenQuinta
               , nparadigma =
                 [ Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ēs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ēs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ēs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ēs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "em"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "em"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ēs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ēs"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ēī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ēī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ērum"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ērum"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ēī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ēī"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ēn"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ē"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ē"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ēbus"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ēbus"
                             , resPerfectum = ""
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ēbus"
                             , resPerfectum = ""
                             }
                 ]
               }
  ]
