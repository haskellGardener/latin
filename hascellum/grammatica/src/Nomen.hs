

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
declensions = [ Declension { ndeclinatio = NomenPrima
                           , nparadigma = [ Paradigma { casus = Nominativus
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
              ]
