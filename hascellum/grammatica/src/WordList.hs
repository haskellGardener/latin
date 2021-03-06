module WordList where
import Data.Text (Text)

import qualified Data.List as DL

import Syllabizer
import WordsAtoC
import WordsDtoG
import WordsHtoM
import WordsNtoS

irun :: Text -> Text
irun x = tokenToText $ outerToken x

run :: Text -> Text
run x = syllablesToText . sylparToSyllables . textToSylparO x . textToTexts . tokenToText $ outerToken x

run3 :: [] Text -> [] (Text, Text, Text)
run3 list = DL.zip3 list (map irun list) (map run list)

workinglist :: [] Text
workinglist = DL.concat [ ablist
                        , aclist
                        , aflist
                        , aglist
                        , allist
                        , amlist
                        , anlist
                        , aplist
                        , aqlist
                        , arlist
                        , aslist
                        , atlist
                        , aulist
                        , avlist
                        , balist
                        , belist
                        , bilist
                        , bllist
                        , bolist
                        , brlist
                        , bulist
                        , calist
                        , celist
                        , chlist
                        , cilist
                        , cllist
                        , colist
                        , crlist
                        , culist
                        , cylist
                        , dalist
                        , delist
                        , dilist
                        , dolist
                        , drlist
                        , dulist
                        , dylist
                        , eblist
                        , eclist
                        , eflist
                        , eglist
                        , eilist
                        , ellist
                        , emlist
                        , enlist
                        , eplist
                        , eqlist
                        , erlist
                        , eslist
                        , etlist
                        , eulist
                        , evlist
                        , exlist
                        , falist
                        , felist
                        , filist
                        , fllist
                        , folist
                        , frlist
                        , fulist
                        , galist
                        , gelist
                        , gilist
                        , gllist
                        , golist
                        , grlist
                        , gulist
                        , gylist
                        , halist
                        , helist
                        , hilist
                        , holist
                        , hulist
                        , hylist
                        , ialist
                        , iblist
                        , iclist
                        , idlist
                        , ielist
                        , iglist
                        , illist
                        , imlist
                        , inlist
                        , iolist
                        , irlist
                        , islist
                        , itlist
                        , iulist
                        , k_list
                        , lalist
                        , lelist
                        , lilist
                        , lolist
                        , lulist
                        , lylist
                        , malist
                        , melist
                        , milist
                        , molist
                        , mulist
                        , mylist
                        , nalist
                        , nelist
                        , nilist
                        , nolist
                        , nulist
                        , nylist
                        , oblist
                        , oclist
                        , odlist
                        , oelist
                        , oflist
                        , ollist
                        , omlist
                        , onlist
                        , oplist
                        , orlist
                        , oslist
                        , otlist
                        , palist
                        , pelist
                        , phlist
                        , pilist
                        , pllist
                        , polist
                        , prlist
                        , pslist
                        , pulist
                        , pylist
                        , qulist
                        , r_list -- Lazy
                        ]
