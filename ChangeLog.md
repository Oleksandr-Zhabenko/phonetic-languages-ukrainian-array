# Revision history for phonetic-languages-ukrainian-array

## 0.1.0.0 -- 2020-12-31

* First version. Released on an unsuspecting world.

## 0.2.0.0 -- 2021-07-24

* Second version. Added two new functions to 'grow' the lines to the needed value. Some code improvements.

## 0.2.1.0 -- 2021-07-24

* Second version revised A. Changed the prepareGrowTextN function (and renamed it to prepareGrowTextMN) so that it
has some additional meaning.

## 0.3.0.0 -- 2021-08-30

* Third version. Added new variants of the concatenations. Some code improvements. Fixed several issues with
consequently going complex words and a prepositions (not very common but actually usable and possible variant).
Added some improvements to deal with some rare cases.

## 0.3.1.0 -- 2021-08-30

* Third version revised A. Fixed issue with the getBFstL' used with repeated by the first element in the tuples list.

## 0.4.0.0 -- 2021-09-01

* Fourth version. Moved from the phonetic-languages-plus the Ukrainian concatenations reversing functionality.
Added for this also a code for the executable unconcatUkr.

## 0.5.0.0 -- 2021-09-02

* Fifth version. Added more reverse concatenations variants to the module Ukrainian.ReverseConcatenations.
Introduced additional first argument to specify whether to use just almost 'safe' reverse concatenations (the
default one) or also some 'risky' ones where you can obtain splitted normal words.

## 0.6.0.0 -- 2021-09-04

* Sixth version. Added new 'tuned' functions. Exported now also the isUkrainianL function.

## 0.6.1.0 -- 2021-09-09

* Sixth version revised A. Fixed issue with language pragma of BangPatterns extension.

## 0.7.0.0 -- 2021-10-30

* Seventh version. Switched to the Case.Hashable.Cuckoo.getBFstL' function where possible. Updated the
dependencies appropriately.

## 0.8.0.0 -- 2021-10-31

* Eigth version. Switched back to the CaseBi.Arr.getBFstLSorted' functionality.

## 0.8.1.0 -- 2022-01-13

* Eigth version revised A. Changed the behaviour of the participles "б" / "ж" so that now they by default are appended to the
previous word, if any. Added for this a new function participleConc.

## 0.8.2.0 -- 2022-01-13

* Eigth version revised B. Fixed issue with just partially defined participleConc function.

## 0.9.0.0 -- 2022-01-14

* Ninth version. Changed the behaviour of many functions in case of sequences of official parts of language like participles,
prepositions etc, so that they are concatenated together with the next word. This is not always suitable, but is sensible
in many cases and allows to preserve more meaning.

## 0.9.1.0 -- 2022-01-14

* Ninth version revised A. Taken into account some more complex participle usage situations, though still not all possible.
Some rather uncommon (rarely used) options are still not properly converted.

## 0.9.2.0 -- 2022-01-29

* Ninth version revised B. Fixed issue with participles "ж / би" and their variants. 

## 0.9.3.0 -- 2022-03-24

* Ninth version revised C. Updated the dependencies boundaries to support the latest versions of GHC and Cabal.

## 0.10.0.0 -- 2023-02-03

* Tenth version. Switched to NoImplicitPrelude extension. Updated the metadata and dependencies boundaries.

## 0.11.0.0 -- 2023-10-01

* Eleventh version. Added new generalized functions to prepare text. Added README.md file. 

## 0.12.0.0 -- 2023-11-16

* Twelfth version. Changed the prepareTextN2 function so that also '=' is not filtered out.

## 0.12.1.0 -- 2023-11-16

* Twelfth version revised A. Downgraded the prepareTextN2 function so that it is like in the 0.11.0.0 version and introduced the function prepareTextN3 with '=' is being not filtered out.

## 0.12.2.0 -- 2024-02-22

* Twelfth version revised B. Changed dependency to intermediate-structures instead of mmsyn5. Added Git repository with bug-tracker. Some changes in README.md. 

