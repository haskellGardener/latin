{-# LANGUAGE RecordWildCards, MultiWayIf, TemplateHaskell, QuasiQuotes  #-}
{-|

Module      : ExemplaParadigma
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

module ExemplaParadigma

where

-- Local Imports

import Nomen
  
-- Explicit Imports

import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text)

-- Qualified Imports

import qualified Data.List          as DL
import qualified Data.Text          as T
import qualified Data.Text.Lazy.IO  as TIO
import qualified Text.Blaze.Internal as TBI

-- Undisciplined Imports

import Text.Hamlet
import Text.Blaze.Html.Renderer.Text

-- End of Imports
-- --------------------------------------------------------------------------------------------------------------------------------------------------------

-- faceParadigma NomenPrima "familia" "ae" "famili" Appellativum Femininum 

nominaSecunda :: [(NDeclinationes, Text, Text, Text, Genera, NominisSpecies)]
nominaSecunda = map (\(da, as, gr, mfn, ns) -> (NomenSecunda, da, as, gr, mfn, ns))
  [ ("hortus","ī","hort",Masculinum,Appellativum)
  , ("modus" ,"ī","mod",Masculinum,Appellativum)
  , ("deus"  ,"ī","de",Masculinum,Appellativum)
  , ("locus" ,"ī","loc",Masculinum,Appellativum)
  , ("animus","ī","anim",Masculinum,Appellativum)
  , ("vitium","ī","viti",Neutrum,Appellativum)
  -- , ("","","",Genera,Appellativum)
  -- , ("","","",Genera,Appellativum)
  -- , ("","","",Genera,Appellativum)
  -- , ("","","",Genera,Appellativum)
  -- , ("","","",Genera,Appellativum)
  -- , ("","","",Genera,Appellativum)
  -- , ("","","",Genera,Appellativum)
  -- , ("","","",Genera,Appellativum)
  -- , ("","","",Genera,Appellativum)
  -- , ("","","",Genera,Appellativum)
  -- , ("","","",Genera,Appellativum)
  -- , ("","","",Genera,Appellativum)
  -- , ("","","",Genera,Appellativum)
  -- , ("","","",Genera,Appellativum)
  -- , ("","","",Genera,Appellativum)
  -- , ("","","",Genera,Appellativum)
  ]
