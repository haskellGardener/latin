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

import Data.Maybe (catMaybes)
import Data.Text (Text)

-- Qualified Imports

-- import qualified Data.List          as DL
import qualified Data.Text          as T
import qualified Data.Text.Lazy.IO  as TIO
-- import qualified Text.Blaze.Internal as TBI

-- Undisciplined Imports

-- import Text.Hamlet
import Text.Blaze.Html.Renderer.Text

-- End of Imports
-- --------------------------------------------------------------------------------------------------------------------------------------------------------

-- faceParadigma NomenPrima "familia" "ae" "famili" Appellativum Femininum 

nominaSecunda :: [(NDeclinationes, Text, Text, Text, Genera, NominisSpecies)]
nominaSecunda = filter (\(_, da, _, _, _, _) -> not $ T.null da)
              $ map (\(da, as, gr, mfn, ns) -> (NomenSecunda, da, as, gr, mfn, ns))
  [ ("hortus"     ,"ī"    ,"hort"     ,Masculinum ,Appellativum)
  , ("modus"      ,"ī"    ,"mod"      ,Masculinum ,Appellativum)
  , ("deus"       ,"ī"    ,"de"       ,Masculinum ,Appellativum)
  , ("locus"      ,"ī"    ,"loc"      ,Masculinum ,Appellativum)
  , ("animus"     ,"ī"    ,"anim"     ,Masculinum ,Appellativum)
  , ("vitium"     ,"ī"    ,"viti"     ,Neutrum    ,Appellativum)
  , ("caelum"     ,"ī"    ,"cael"     ,Neutrum    ,Appellativum)
  , ("vinculum"   ,"ī"    ,"vincul"   ,Neutrum    ,Appellativum)
  , ("bellum"     ,"ī"    ,"bell"     ,Neutrum    ,Appellativum)
  , ("annus"      ,"ī"    ,"ann"      ,Neutrum    ,Appellativum)
  , ("malum"      ,"ī"    ,"mal"      ,Neutrum    ,Appellativum)
  , ("benificium" ,"ī"    ,"benifici" ,Neutrum    ,Appellativum)
  , ("puer"       ,"ī"    ,"puer"     ,Masculinum ,Appellativum)
  , ("fatum"      ,"ī"    ,"fat"      ,Masculinum ,Appellativum)
  , ("populus"    ,"ī"    ,"popul"    ,Masculinum ,Appellativum)
  , ("vīcīnus"    ,"ī"    ,"vīcīn"    ,Masculinum ,Appellativum)
  , ("nātus"      ,"ī"    ,"nāt"      ,Masculinum ,Appellativum)
  , ("amīcus"     ,"ī"    ,"amīc"     ,Masculinum ,Appellativum)
  , ("sīgnum"     ,"ī"    ,"sīgn"     ,Neutrum    ,Appellativum)
  , ("ingenium"   ,"ī"    ,"ingeni"   ,Neutrum    ,Appellativum)
  , ("oculus"     ,"ī"    ,"ocul"     ,Masculinum ,Appellativum)
  , ("somnus"     ,"ī"    ,"somn"     ,Masculinum ,Appellativum)
  , ("cōnsilium"  ,"ī"    ,"cōnsili"  ,Neutrum    ,Appellativum)
  , ("equus"      ,"ī"    ,"equ"      ,Masculinum ,Appellativum)
  , ("vōtum"      ,"ī"    ,"vōt"      ,Neutrum    ,Appellativum)
  , ("ventus"     ,"ī"    ,"vent"     ,Masculinum ,Appellativum)
  , ("dominus"    ,"ī"    ,"domin"    ,Masculinum ,Appellativum)
  , ("aurum"      ,"ī"    ,"aur"      ,Neutrum    ,Appellativum)
  , ("proelum"    ,"ī"    ,"proel"    ,Neutrum    ,Appellativum)
  , ("numerus"    ,"ī"    ,"numer"    ,Masculinum ,Appellativum)
  , ("rēgnum"     ,"ī"    ,"rēgn"     ,Neutrum    ,Appellativum)
  , ("studium"    ,"ī"    ,"studi"    ,Neutrum    ,Appellativum)
  , ("campus"     ,"ī"    ,"camp"     ,Masculinum ,Appellativum)
  , ("perīculum"  ,"ī"    ,"perīcul"  ,Neutrum    ,Appellativum)
  , ("legatus"    ,"ī"    ,"legat"    ,Masculinum ,Appellativum)
  , ("marītus"    ,"ī"    ,"marīt"    ,Masculinum ,Appellativum)
  -- , (""           ,"ī"    ,""         ,Masculinum ,Appellativum)
  -- , (""           ,"ī"    ,""         ,Masculinum ,Appellativum)
  -- , (""           ,"ī"    ,""         ,Masculinum ,Appellativum)
  -- , (""           ,"ī"    ,""         ,Masculinum ,Appellativum)

  ]


generator :: [(NDeclinationes, Text, Text, Text, Genera, NominisSpecies)] -> IO ()
generator species = mapM_ (TIO.putStrLn . renderHtml . scribeNomenParadigma) . catMaybes $ map faceParadigmaAlt species


nominaSecundaGrammatica :: [(NDeclinationes, Text, Text, Text, Genera, NominisSpecies)]
nominaSecundaGrammatica = filter (\(_, da, _, _, _, _) -> not $ T.null da)
              $ map (\(da, as, gr, mfn, ns) -> (NomenSecunda, da, as, gr, mfn, ns))
  [ ("verbum"      ,"ī"    ,"verb"      ,Neutrum    ,Appellativum)
  , ("adiectīvum"  ,"ī"    ,"adiectīv"  ,Neutrum    ,Appellativum)
  , ("adverbium"   ,"ī"    ,"adverbi"   ,Neutrum    ,Appellativum)
  , ("ablātīvus"   ,"ī"    ,"ablātīv"   ,Masculinum ,Appellativum)
  , ("alphabētum"  ,"ī"    ,"alphabēt"  ,Neutrum    ,Appellativum)
  , ("articulus"   ,"ī"    ,"articul"   ,Masculinum ,Appellativum)
  , ("membrum"     ,"ī"    ,"membr"     ,Neutrum    ,Appellativum)
  , ("vocābulum"   ,"ī"    ,"vocābul"   ,Neutrum    ,Appellativum)
  , ("datīvus"     ,"ī"    ,"datīv"     ,Masculinum ,Appellativum)
  , ("dēminūtīvum" ,"ī"    ,"dēminūtīv" ,Neutrum    ,Appellativum)
  , ("diphthongus" ,"ī"    ,"diphthong" ,Femininum  ,Appellativum)
  , ("encliticum"  ,"ī"    ,"enclitic"  ,Neutrum    ,Appellativum)
  , ("vitium"      ,"ī"    ,"viti"      ,Neutrum    ,Appellativum)
  , ("mendum"      ,"ī"    ,"mend"      ,Neutrum    ,Appellativum)
  , ("genetīvus"   ,"ī"    ,"genetīv"   ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  , (""            ,"ī"    ,""          ,Masculinum ,Appellativum)
  ]
