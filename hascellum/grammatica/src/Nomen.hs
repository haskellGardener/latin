

module Nomen

where

import Data.Text (Text)

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
          , lexiconArticulum    :: Text         -- Nominative singular dictionary entry
          , articulumTerminatur :: Text         -- Nominative singular dictionary ending
          , genetivusStem       :: Text         -- Genetivus singular dictionary entry stem
          , nomenGenera         :: NominisGenus -- Default is Appellativum
          }
    deriving (Show, Eq)

data Paradigma
  = Paradigma { casus      :: Casus
              , genera     :: Genera
              , numerus    :: Numeri
              , stem       :: Text
              , terminatur :: Text
              }
    deriving (Show, Eq)

-- Will need to build a sort based on Casus.

data Declension = Declension { ndeclinatio :: NDeclinationes
                             , nparadigma :: [] Paradigma
                             }

declensions :: [] Declension
declensions =
  [ Declension { ndeclinatio = NomenPrima
               , nparadigma =
                 [ Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "a"
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "a"
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ae"
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ae"
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "am"
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "am"
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "as"
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "as"
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ae"
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ae"
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ārum"
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ārum"
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ae"
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ae"
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "īs"
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "īs"
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ābus"
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ābus"
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ā"
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ā"
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "īs"
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "īs"
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ābus"
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ābus"
                             }
                 ]
               }
  , Declension { ndeclinatio = NomenSecunda
               , nparadigma =
                 [ Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "us"
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "er"
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "um"
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ī"
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ī"
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "a"
                             }
                 , Paradigma { casus = Vocativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "e"
                             }
                 , Paradigma { casus = Vocativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "er"
                             }
                 , Paradigma { casus = Vocativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "um"
                             }
                 , Paradigma { casus = Vocativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ī"
                             }
                 , Paradigma { casus = Vocativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ī"
                             }
                 , Paradigma { casus = Vocativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "a"
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "um"
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "um"
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "um"
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ōs"
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ōs"
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "a"
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ī"
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ī"
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ī"
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ōrum"
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ōrum"
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ōrum"
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ō"
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ō"
                             }
                 , Paradigma { casus = Dativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ō"
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "īs"
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "īs"
                             }
                 , Paradigma { casus = Dativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "īs"
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ō"
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ō"
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ō"
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "īs"
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "īs"
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "īs"
                             }
                 ]
               }
  , Declension { ndeclinatio = NomenTertia
               , nparadigma =
                 [ Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = ""
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ēs"
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ēs"
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "a"
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "em"
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "em"
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = ""
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ēs"
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ēs"
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "a"
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "is"
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "is"
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "is"
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "um"
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "īum"
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "um"
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "īum"
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "um"
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "īum"
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ī"
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ī"
                             }
                 , Paradigma { casus = Dativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ī"
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ibus"
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ibus"
                             }
                 , Paradigma { casus = Dativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ibus"
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "e"
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "e"
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "e"
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ibus"
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ibus"
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ibus"
                             }
                 ]
               }
  , Declension { ndeclinatio = NomenQuarta
               , nparadigma =
                 [ Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "us"
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "us"
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ū"
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ūs"
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ūs"
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ua"
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "um"
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "um"
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ū"
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ūs"
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ūs"
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ua"
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ūs"
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ūs"
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ūs"
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "uum"
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "uum"
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "uum"
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "uī"
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "uī"
                             }
                 , Paradigma { casus = Dativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ū"
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ibus"
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ibus"
                             }
                 , Paradigma { casus = Dativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ibus"
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ū"
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ū"
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Neutrum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ū"
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ibus"
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ibus"
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Neutrum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ibus"
                             }
                 ]
               }
  , Declension { ndeclinatio = NomenQuinta
               , nparadigma =
                 [ Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ēs"
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ēs"
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ēs"
                             }
                 , Paradigma { casus = Nominativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ēs"
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "em"
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "em"
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ēs"
                             }
                 , Paradigma { casus = Accusativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ēs"
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ēī"
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ēī"
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ērum"
                             }
                 , Paradigma { casus = Genetivus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ērum"
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ēī"
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ēī"
                             }
                 , Paradigma { casus = Dativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ēn"
                             }
                 , Paradigma { casus = Dativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ē"
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ē"
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Singularis
                             , stem = ""
                             , terminatur = "ē"
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Masculinum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ē"
                             }
                 , Paradigma { casus = Ablativus
                             , genera = Femininum
                             , numerus = Pluralis
                             , stem = ""
                             , terminatur = "ē"
                             }
                 ]
               }
  ]
