( This document was extracted from http://mind.sourceforge.net/mind4th.html )
( mind. f  written by  "Mentifex" Arthur Murray                     )
( 3sep08A.F [mf080903.html] -- modification of 1sep08A.F Mind.Forth )
( This file may be named "Mind.f" or any "filename.f" you choose. )
( Rename any occurrence of Mind.f.txt as simply Mind.f for Win32Forth. )
( This file is to be run with Win32Forth by issuing three commands: )
( win32for.exe [ENTER] )
( fload Mind.f [ENTER] )
( ALIFE [ENTER]. )
( To halt the artificial mind, press the ESCAPE key at any time. )
\ Change.log elements:
\ 20jan08A.F fixes topic-jumping bug and attains cognitive chain reaction.
\ 20jan08B.F leaves in but comments out diagnostics for further debugging.
\ 22jan08A.F tightens up activation-settings to prevent spurious thoughts.
\ 22jan08B.F calls EGO resuscitation upon detection of repetitious thought.
\ 24jan08A.F changes verbClear into verbClip to merely reduce activations.
\ 27jan08A.F uses "EEG" and "redux" to revive oldest concept during lull.
\ 29jan08A.F comments out "actset" because Moving Wave sets one activation.
\ 30jan08A.F removes "trough"; comments out "uract"; fixes verbClip module.
\  1feb08A.F removes "actset" and "uract"; improves activation settings.
\ 24aug08A.F adds a "num" (number) flag to the flag-panel of the psi array.
\ 25aug08A.F tests new Article module by inserting "the" before all nouns.
\ 27aug08A.F adds "num" to English array with value-transfer in Reify.
\ 29aug08A.F avoids SVO in preparation for thinking with verbs of being.
\ 30aug08A.F inaugurates practice of stating entelechy goals for modules.
\ 31aug08A.F uses psiDecay to keep verbs of being at a default activation.
\  1sep08A.F converts whatAuxSDo into whatIs to initiate verbs of being.
\  2sep08A.F reintroduces Conjoin module from 16aug02A,F source code.
\  3sep08A.F introduces knowledge-base traversal module kbTraversal.

DECIMAL  ( use decimal numbers )
variable act 0 act ! ( INSTANTIATE; OLD- & NEWCONCEPT; REIFY; etc. )
variable actbase ( 5dec2007 For recognition discrimination in audRecog )
variable adverbact 0 adverbact ! ( 29aug2008 Test for adverb-insertion )
variable aud  ( enVocab; Audition; Speech: auditory recall-tag )
variable audjuste ( 25aug2008 For nounPhrase motjuste aud to SPEECH )
variable audstop  ( 1jan2008 A flag to stop SPEECH after one word )
variable back ( to replace "bulge" for "pre" items in spreadAct )
variable bday ( ALIFE; HCI -- day of birth )
variable beg  1 beg !  ( Audition; audSTM: word beginning flag )
variable bhour ( ALIFE; HCI -- hour of birth )
variable bias 5 bias ! ( Parser; newConcept: an expected POS )
variable bminute ( ALIFE; HCI -- minute of birth )
variable bmonth ( ALIFE; HCI -- month of birth )
variable bsecond ( ALIFE; HCI -- second of birth )
variable byear ( ALIFE; HCI -- year of birth )
variable caller  ( 13jan2008 Debug-identifier of calling module )
variable cns  1024 cns !  ( "central nervous system" array size )
variable coda  128 coda !  ( span of memory to recycle in Rejuvenate )
variable conj  ( from 16aug2002 oldConcept; Conjoin: conjunction )
variable ctu ( continuation-flag for "Aud" array phonemes )
variable decpsi1 ( decremend concept 1 to accelerate de-activation )
variable decpsi2 ( decremend concept 2 for avoiding excess repetition )
variable decpsi3 ( decremend concept 3 to keep track of most recent psi )
variable detour  ( 17dec2007 Abort-flag in case of insufficient knowledge )
variable dirobj  ( flag to indicate seeking of a direct object )
variable dopsi   ( 22jan2008 direct-object-psi for calculating "thotnum" )
variable edge  0 edge !  ( Rejuvenate: edge-of-thought flag )
variable EEG 1 EEG ! ( for EGO safety measure if users neglect the AI )
variable en6 ( 27aug2008 enVocab flag recall-vector "aud" in Rejuvenate )
variable enx ( new-, oldConcept; Instantiate; Reify: x-fer Psi-En )
variable eot ( end-of-text for use in AUDITION )
variable fex ( new-, oldConcept; enVocab: Psi-to-English fiber-out )
variable fin ( new-, oldConcept; enVocab: English-to-Psi fiber-in )
variable fyi 0 fyi ! ( for rotation through display modalities )
variable hipsi   ( 14jan2008 "high-psi" tag on wavecrest concept )
( I = Index in loops; does not require a fetch "@" )
variable img  ( visRecog: for future use as "image" )
\ variable inert  0 inert !  ( Audition; Security: EGO-trigger )
variable inert  0 inert !  ( .echo; Think; English: EGO-trigger )
variable IQ 5 IQ ! ( an invitation to code an IQ algorithm )
variable jdex   \ 4ap2007 Testing a Reify subordinate loop index
variable jolt 0 jolt ! ( 31mar2007 Being replaced with "nounval" )
variable jrt ( "junior time" for memories moved in Rejuvenate )
variable jux  0 jux !  ( Parser; Instantiate: a JUXtaposed word )
variable kbpsi  ( 20jan2008 an interim knowledge-base psi )
variable kbtv  1 kbtv !  ( 3sep2008 KB-traversal trigger )
variable krt ( Knowledge Representation time "t" for future use )
variable lastword  0 lastword !  ( 2apr2007 For zeroing "seq" tags. )
variable len  ( length, for avoiding non-words in AUDITION )
variable lexact  ( 4apr2007 Testing a lexical :act" for Reify )
variable lopsi  ( 14jan2008 "low-psi" tag on just-crested concept )
variable match  ( oldConcept; SPEECH:  end-of-word flag for control )
variable midway 1 midway ! ( a backwards time-limit for searches )
variable monopsi ( 26jul2002 For use in audRecog module )
variable motjuste ( best word for selection as part of a thought )
variable nen  0 nen ! ( English lexical concept number )
variable nlt  0 nlt !  ( not-later-than, for isolating time-points )
variable nounval 0 nounval ! ( For transfer from nounPhrase to nounAct )
variable nphrnum 0 nphrnum ! ( 27aug2008 nounPhrase number for Article )
variable nphrpos 0 nphrpos ! ( 29aug2008 for testing in ENGLISH )
variable num 0 num !  ( 24aug2008 number-flag for the psi array )
variable obstat ( 15jan2008 Lets Audition psi-damp a reentrant word. )
variable oldact ( to show the source of spreading activations )
variable oldpos ( for treating verb part-of-speech as a special case )
variable oldpsi ( to show the source of spreading activations )
variable onset 0 onset ! ( of a word stored in auditory memory )
variable opt  ( option, for flushing out a desired part of speech )
variable ordo 0 ordo ! ( 2apr2007 from JSAI; Audition word-order )
variable pho  ( Listen; Audition; Speech: phoneme of input/output )
variable pos  ( old- & newConcept; enVocab: part-of-speech )
variable pov  ( point-of-view: 35# for internal; 42* for external )
variable pre  ( previous concept associated with another concept )
variable precand  ( 2apr2007 inviolate "pre" candidate from JSAI )
variable predpos 0 predpos ! ( 29aug2008 part of speech in Predicate )
variable preset 0 preset !  ( 2apr2007 For setting Instantiate "pre" )
variable prevtag  ( 2apr2007 from JSAI; for use in Instantiate )
variable psi  ( associative tag from auditory engram to Psi mindcore )
variable psi1 ( 30mar2007 verbAct: activation-level at each node of verb )
variable psi6 ( temporary enx for use by verbPhrase in Tutorial mode )
variable psi7 ( 24aug2008 replacement for psi6 displaced by num )
variable psibase ( 5dec2007 With winning actbase, a winning psibase. )
variable questype  ( from 16aug 2002 oldConcept; Conjoin: "question-type" )
variable quiet 1 quiet ! ( Listen; Audition: a status flag for input )
variable recon 0 recon ! ( Eagerness to seek reconnaissance answers. )
variable redux  ( 27jan2008 For oldest concept to be revived. )
variable reject 0 reject ! ( For orchestration of sentence-generation. )
variable residuum 0 residuum ! ( psiDamp -- post-thought activation )
variable rjc  0 rjc ! ( Rejuvenate: counter of rejuvenations over life )
variable rsvp 1000 rsvp ! ( AUDITION/LISTEN response-time counter )
variable rv ( "recall-vector" for diagnostic display of thought processes )
variable seq  ( subSEQuent concept associated with another concept )
variable spike  ( 1aug2005: for potential use in spreadAct )
variable spt  ( audSTM; AUDITION: blank space time )
variable subj     ( 15sep2005 flag to supercharge subject-nouns )
variable supsi  ( 22jan2008 subject-psi for calculating "thotnum" )
variable t  0 t ! ( time as incremented during auditory memory storage )
variable t2s    ( 1jan2008 auditory text-to-speech index for SPEECH )
variable thot1    ( 22jan2008 for detecting repetitions )
variable thot2    ( 22jan2008 for detecting repetitions )
variable thot3    ( 22jan2008 for detecting repetitions )
variable thotcyc  ( 22jan2008 for seeking repetition in a cycle )
variable thotnum  ( 22jan2008 A numeric concatenation of psi numbers )
variable topic  ( topic for a question to be asked )
variable tov 1 tov ! ( TABULARASA; REIFY; ENGLISH; .echo: time-of-voice )
variable tsday    ( LISTEN -- for headers in transcript mode )
variable tshour   ( LISTEN )
variable tsminute ( LISTEN )
variable tsmonth  ( LISTEN )
variable tssecond ( LISTEN )
variable tsyear   ( LISTEN )
variable tult ( AUDITION; audSTM: t penultimate, or time-minus-one )
variable txen ( 4apr2007 Reify: time of transfer to English lexicon )
variable unk  ( "unknown" variable for general use )
variable upnext  ( 29jan2008 Flag lets new input de-crest previous. )
variable urpre ( original pre for safeguarding during function-calls )
variable urpsi ( original German:ur psi for use in psiDamp, etc. )
\ variable vault 158 vault !  ( enBoot; audSTM; Rejuvenate: bootstrap )
variable vault 202 vault !  ( 29mar2007 adding data from JavaScript AI )
variable vbpsi   ( 22jan2008 verb-psi for calculating "thotnum" )
variable verbinc ( 27apr2007 verb-increment for use in verbAct module )
variable verbval  ( 3apr2007 For transfer from verbPhrase to verbAct.)
variable zone  ( ACTIVATE; SPREADACT: time-zone for "pre" and "seq" )

\ Array code for Psi - En - Aud memory channel arrays.
:  CHANNEL   ( size num -< name >- )  \ ATM 21apr2002.
  CREATE   ( Returns address of newly named channel. )
  OVER     ( #r #c -- #r #c #r ) \ E.g., cns 6 cns
  ,        ( Stores number of rows from stack to array.)
  * CELLS  ( Feeds product of columns * rows to ALLOT.)
  ALLOT    ( Reserves given quantity of cells for array.)
  DOES>    ( member; row col -- a-addr )
  DUP @    ( row col pfa #rows ) \ Duplicates top item.
  ROT *    ( row pfa col-index ) \ Productizes top two.
  ROT +    ( pfa index ) \ Adds the product to the row#.
  1 +      ( because the first cell has the number of rows.)
  CELLS +  ( from number of items to number of bytes in offset )
;  \ End of code for arrays to be created with "cns" size.

\ Memory channel arrays.  ATM 21jul2002; 24aug2008; 27aug2008
cns @  8  CHANNEL  psi{  ( Mindcore concept array "psi" )
cns @  7  CHANNEL   en{  ( English lexicon array "en" )
cns @  6  CHANNEL  aud{  ( Auditory memory channel array "aud" )

:  PSI-CLEAR  \ ATM 21apr2002; or your ID & date.
  1   t @  1 +  DO  \ At all memory-points up to the present,
    0 I 1 psi{ !    \ store zero as the activation value.
  -1 +LOOP          \ Loop backwards by decrements of one.
;  \ End of PSI-CLEAR; return to EGO.

:  TABULARASA  \ ATM 21apr2002; 27aug2008

  0 unk !  \ Use "unk" to count from zero to seven.
  1 tov !  \ Time-of-voice for .echo use.
  BEGIN  cns @  1  DO
    0 I  unk @  psi{ !
  LOOP
  1  unk +!
  unk @  8  <  WHILE  \ 27aug200 psi array has seven (7) flags
  REPEAT

  0 unk !  \ Use "unk" to count from zero to six.
  1 tov !  \ Time-of-voice for .echo use.
  BEGIN  cns @  1  DO
    0 I  unk @   en{ !
  LOOP
  1  unk +!
  unk @  6  <  WHILE   \ 27aug2008 en{ array has six (6) flags.
  REPEAT

  0 unk !  \ Use "unk" to count from zero to five.
  1 tov !  \ Time-of-voice for .echo use.
  BEGIN  cns @  1  DO
    0 I  unk @  aud{ !
  LOOP
  1  unk +!
  unk @  6  <  WHILE
  REPEAT
  cns @  1  DO
    32   I  0   aud{ !  \ Blank out aud{ with ASCII "32" blanks.
  LOOP
;  \  End of TABULARASA; return to ALIFE.


\ verbClear is an attempt to let subject-nouns activate only
\ truly associated verbs because all verbs are at zero activation.
\ The end of SVO calls verbClear so that user-input may still
\ activate a verb before the AI Mind responds to the user input.
:  verbClear ( remove activation from all verbs ) \  1sep2008
  midway @  t @  DO     \  Loop backwards in recent time.
    I     5 psi{ @ 8 = IF  \ 24aug2008 Moving "pos" over by one.
      0 I 1 psi{ !    \ 2apr2007 Set verbs to zero activation.
    THEN              \ 2apr2007 End of test for pos=8 verbs.
  -1  +LOOP  \  End of backwards loop looking for pos=8 verbs.
;  \ End of function verbClear.


\ verbClip is a module that clips activation on verbs down to
\ a subconscious level when the Audition module starts to process
\ sensory input, so that no verb forms a spurious sentence.
:  verbClip ( lower activation on all verbs ) \  1sep2008
  midway @  t @  DO     \  Loop backwards in recent time.
    I     5 psi{ @ 8 = IF   \ 24aug2008 If word is a verb...
      I   1 psi{ @ 20 > IF  \ 30jan2008 If activation is high...
     20 I 1 psi{ !    \ 24jan2008 Comparable with "residuum".
      THEN            \ 30jan2008 End of test for high activation.
    THEN              \  2apr2007 End of test for pos=8 verbs.
  -1  +LOOP  \  End of backwards loop looking for pos=8 verbs.
;  \ End of function verbClip.


\  psiDecay is called from psiDamp or elsewhere to make
\  all non-zero (positive) mindcore psi1 activations decrease a
\  little, so as to simulate neuronal excitation-decay for the
\  purpose of letting stray activations in the subconscious
\  mind dwindle away over time down to a zero activation.
\  Thinking keeps activations high; psiDecay lowers them.
\  If necessary, an ego-boosting Ego module may assert activation.
\  In a robot mind with an upper tier of activation for concepts
\  riding a moving wave of consciousness, psiDecay lets after-thought
\  concepts sink gradually through a lower tier of residual activation.
:  psiDecay ( let conceptual activations dwindle ) \  2sep2008
  fyi @ 2 > IF CR  \ Skip display during tutorial mode.
  ."       psiDecay called to reduce all conceptual activations." CR
  THEN
  midway @  t @  DO  \  Loop backwards in recent time.
    I  1  psi{ @  1 -        I  1  psi{ !       \ 23jan2006
    I  1  psi{ @  0 < IF  0  I  1  psi{ ! THEN  \  4aug2005
    \ 21sep2005 Next line of active code zeroes out the "DO"
    \ concept so that question elements will not linger.
    I  0  psi{ @ 59 = IF  0  I  1  psi{ ! THEN  \ Zero out DO.
    \ 5sep2005 Next line of active code treats concept "WHAT" as a
    \ special case where zero activation elecits positive answers.
    I  0  psi{ @ 54 = IF  0  I  1  psi{ ! THEN  \ Zero out WHAT.

    \ 31aug2008 Verbs of being will have a minor priority by default:
    I     0    psi{ @ 57 = IF  \  1sep2008 For concept #57 "AM"
      I   1    psi{ @  8 < IF  \  1sep2008 If "act" is below eight
      \ 8 I 1  psi{ !          \  1sep2008 Reset low "act" at eight.
        1 I 1  psi{ !          \  1sep2008 Pehaps abandon this technique.
      THEN  \  1sep2008 End of test for activation lower than 8.
    THEN    \ 31aug2008 Keep "AM".

    I      0   psi{ @ 66 = IF   \  1sep2008 For concept #66 "IS"
      I    1   psi{ @  8 < IF   \  1sep2008 If "act" is below eight
      \ 8  I 1 psi{ !           \  1sep2008 Reset low "act" at eight.
        1 I 1  psi{ !          \  1sep2008 Pehaps abandon this technique.
      THEN  \  1sep2008 End of test for activation lower than 8.
    THEN    \ 31aug2008 Keep "IS".

    I      0   psi{ @ 67 = IF    \  1sep2008 For concept #67 "ARE"
      I    1   psi{ @  8 < IF    \  1sep2008 If "act" is below eight
      \ 8  I 1 psi{ !            \  1sep2008 Reset low "act" at eight.
         1 I 1  psi{ !          \  1sep2008 Pehaps abandon this technique.
      THEN  \  1sep2008 End of test for activation lower than 8.
    THEN  \ 31aug2008 Keep "ARE".

    \ 2sep2008 Next line of active code zeroes out the "ARE"
    \ concept so that input questions are not tied to "ARE".
    I  0  psi{ @ 67 = IF  0  I  1  psi{ ! THEN  \ Zero out ARE.


    \ 2sep2008 Next line of active code zeroes out the "IS"
    \ concept so that question elements will not linger.
    I  0  psi{ @ 66 = IF  0  I  1  psi{ ! THEN  \ Zero out IS.
    \ 2sep2008 Note: Code above may need to involve only WHAT IS...?


  -1  +LOOP  \  End of finding and reducing positive activations.
;  \  End of psiDecay; return to psiDamp or elsewhere.


\  psiDamp is called from nounPhrase or verbPhrase
\  to substantially knock down the activation of a concept
\  that has just been thought, so that another concept may
\  rise from the subconscious up into the AI consciousness.
\  33-48 = consciousness tier where concepts win selection.
\  17-32 = subconscious where concepts remain available.
\   1-16 = noise tier below logical association threshold.
\  Meanwhile the psidamped concept remains available in
\  the subconscious mind for inclusion in another thought.
:  psiDamp ( reduce activation of a concept ) \ 15jan2008
\ 12 residuum ! \ 31mar2007 residuum and its value come from JSAI.
\ 20 residuum ! \  6jan2008 Prevent verbPhrase from aborting valid verbs.
  16 residuum ! \  1feb2008 Optimizing overall activation settings.
\ fyi @ 2 > IF CR  \ 14jan2008 Retain as valuable for Diagnostic mode.
  fyi @ 2 > IF CR  \ 20jan2008 Temporary display in all modes.
  ."     psiDamp called for urpsi = " urpsi @ .
\ ."  and residuum = " residuum @ . CR  \ 13jan2008 for clarity.
  ."  by module ID #" caller @ .        \ 13jan2008 for clarity.
  caller @  42 = IF ." whatAuxSDo " THEN  \ 13jan2008
  caller @  51 = IF ." auxVerb "    THEN  \ 13jan2008
  caller @  62 = IF ." verbPhrase " THEN  \ 13jan2008
  caller @  66 = IF ." nounPhrase " THEN  \ 13jan2008
  caller @ 104 = IF ." Audition "   THEN  \ 13jan2008
  caller @ 148 = IF ." Activate "   THEN  \ 13jan2008
  CR
  0 caller !  \ 13jan2008 Reset caller-ID for safety.
  THEN
  midway @  t @  DO     \ 7apr2007 Avoid "array boundary problem"
            I  0  psi{ @ urpsi @ = IF  \ If psi0 is found,
 residuum @ I  1  psi{ ! THEN  \ Set in psiDamp or in calling module.
  -1 +LOOP                     \ End of backwards loop.
  0 residuum !  \ Safety measure in case value comes from elsewhere.
  psiDecay  \ 15jan2008 Minor deactivation of all concepts simultaneously.
;   \  psiDamp returns to nounPhrase, verbPhrase, etc.


\  enDamp is called from nounPhrase or verbPhrase
\  to de-activate all concepts in the English lexicon.
:  enDamp ( deactivate English lexicon concepts ) \ atm 14oct2005
  midway @  t @  DO  \ Loop backwards from current time.
    0 I  1  en{ !    \ Store zero activation at en1.
  -1 +LOOP           \ End of backwards loop.
;  \  enDamp returns to nounPhrase or verbPhrase.


\  audDamp is called from AUDITION upon recognition of
\  a known word, and resets auditory engram activations
\  to zero so that additional words may be recognized.
:  audDamp ( deactivate auditory engrams ) \ atm 14oct2005
  midway @  t @  DO    \ Loop backwards through time.
    0 I  1 aud{ !      \ Replace excitation with zero.
  -1 +LOOP
;  \  end of audDamp; return to AUDITION.

\ Print-to-screen "dot" functions for trouble-shooting mode.
\ .psi shows the contents of the deep mindcore "Psi".
:  .psi ( show concepts in the Psi array ) \ 24aug2008
  CR  ." Psi mindcore concepts"
  CR  ." time: psi act num jux pre pos seq enx "
  t @ 1+  midway @ DO  \  Look as far back as "midway".
    I    0  psi{ @ 0 > IF
      CR I . ." : "
      I  0  psi{ @ . ." "  \ psi
      I  1  psi{ @ . ." "  \ act
      I  2  psi{ @ . ." "  \ num  \ 24aug2008 insertion
      I  3  psi{ @ . ." "  \ jux
      I  4  psi{ @ . ." "  \ pre
      I  5  psi{ @ . ." "  \ pos
      I  6  psi{ @ . ." "  \ seq
      I  7  psi{ @ enx ! enx @ .  \ enx = transfer-to-English.
      enx @ 0 > IF
        ." to "
        I unk !
        0 aud !  \ First zero out the auditory recall-vector.
        midway @ unk @  DO
          I   0 en{ @  enx @ = IF
          \ I 5 en{ @  aud  !  \ Fetch any stored recall-vector.
            I 6 en{ @  aud  !  \ 27aug2008 Fetch stored recall-vector.
            aud @ 0= NOT IF    \ If the auditory tag is not zero,
              BEGIN            \ begin the next sequence;
                aud @ 0 aud{ @ EMIT  \ display each engram;
                1 aud +!             \ increment the auditory tag.
                aud @ 0 aud{ @ 32 =  \ When the engram is a SPACE,
              UNTIL                  \ stop fetching engrams.
              ."  "                  \ Display a space.
            THEN  \ End of test for non-zero auditory engrams.
            0 aud !  \ Zero out the auditory associative tag.
          LEAVE  ( One engrammed word is enough. )
          THEN    \ End of search for English lexical sample.
        -1  +LOOP   \ End of loop through English lexicon.
      THEN  \ end of if-clause looking for "enx" transfers
    THEN    \ end of if-clause looking for Psi concepts
  LOOP  CR ." time: psi act num jux pre pos seq enx "  0 unk ! \ 24aug2008
  CR ." You may enter .psi .en .aud to view memory engrams, or"
  CR ." ALIFE [ENTER] to erase all memories and restart the Mind."
  CR
;  \  End of .psi -- called when user types:  .psi [ENTER]

\ .en displays the English lexicon array "en{".
:  .en ( show vocabulary in the English lexicon array ) \  1sep2008
  CR ." English lexical fibers"
\ CR ." t nen act fex pos fin aud:"
  CR ." t nen act num fex pos fin aud:"
  t @  1+  midway @  DO
    I  0  en{ @  unk !
    unk @  0 > IF ( display positive data )
      CR I . unk @ . ." "
      I 1 en{ @ . ." "
      I 2 en{ @ . ." "
      I 3 en{ @ . ." "
      I 4 en{ @ . ." "
      I 5 en{ @ . ." "  \ 27aug2008 Bumped down by "num"
      I 6 en{ @ aud !  aud @ . ."  to "  \ 27aug2008 Bumped by "num"
      BEGIN
        aud @ 0 aud{ @ EMIT  1 aud +!
        aud @ 0 aud{ @ 32 =  \ Using a blank SPACE-bar.
      UNTIL
      ."  "
      0 aud !  \ Zero out the auditory associative tag.
    THEN
  LOOP
  0 unk !
  CR ." t nen act num fex pos fin aud" CR
  CR ." You may enter .psi .en .aud to view memory engrams, or"
  CR ." ALIFE [ENTER] to erase all memories and restart the Mind."
  CR
;  \  End of .en -- called when user types:  .en [ENTER]

\ .aud & .echo display the auditory memory channel.
:  .aud ( show engrams in the auditory memory array ) \ atm 14oct2005
  CR ." Auditory memory nodes"
  CR  ."  t pho act pov beg ctu psi"
  t @  1+  1 DO           ( Show the entire Aud channel.)
\ t @  1+  t @  36 -  DO  ( Show the last 36 "phonemes".)
    CR    I . ." "
    I 2 aud{ @ 123  =  IF
      ."     { "
    THEN  \ From SVO.
    I 0 aud{ @  33  <  IF
      ."  "  ( show a blank )
    ELSE
      I 0 aud{ @ EMIT ."  "
      I 1 aud{ @ .     ." "
      I 2 aud{ @ EMIT ."  "
      I 3 aud{ @ .     ." "
      I 4 aud{ @ .     ." "
      I 5 aud{ @ .
    THEN
    I 2 aud{ @ 125  =  IF
      ."     } "
    THEN  \ From SVO.
  LOOP
  CR ." You may enter .psi .en .aud to view memory engrams, or"
  CR ." ALIFE [ENTER] to erase all memories and restart the Mind."
  CR
;  \ End of .aud -- called when user types:  .aud [ENTER]

: .echo ( show what the robot just said ) \  1feb2008
  ( As on Usenet, user responds _below_ what the AI has said. )
  fyi @ 2 = IF       \ 29jan2008 Test for the display-mode.
  CR ." Tutorial mode is now in effect. Enter input or wait for output."

    EEG @ 0 = IF   \  1feb2008 Explain better what is happening.
      CR ." Duplicate thought may have been detected."
    THEN           \  1feb2008 End of EEG-test.

  THEN               \ 29jan2008 End of fyi-test.
  CR  ." Robot: "    \ 26jul2002 A repeat of fast-scrolling output.
\ inert @ 1 < IF   \ Avoid showing spurious repetition of output.
\ inert @ 6 < IF   \ 1apr2007
\ inert @ 6 < IF   \ 5dec2007 Remove inert-test while debugging audRecog.
    t @ tov @  DO  \ 8mar2005 Loop from time-of-voice to now..
      I 0  aud{ @  0 =  IF   \ If a zero is stored
        ."  "                \ display a blank space;
      ELSE                   \ otherwise
        I   2 aud{ @  42 = NOT IF  \ 1apr2007 Only if internal pov
          I 0 aud{ @ EMIT      \ display the stored character.
        THEN           \ 1apr2007 End of test for internal pov.
      THEN             \ End of test of stored character.
    LOOP               \ End of loop through auditory memory.
\ THEN  \ End of "inert" test to avoid spurious repetitions.
;  \ End of .echo; return to AUDITION.

\ SPREADACT is called by ACTIVATE
\ to spread activation among concepts.
\ It follows "pre" and "seq" tags to find related concepts.
: SPREADACT  ( spreading Activation )  \  1sep2008
  pre @ 0 > IF   \  if a pre(vious) concept exists...
  \ midway @  zone @  DO   \ Time-zone search.
    zone @ 7 -   zone @  DO   \ 12sep2005 As for seq, so for pre.
      I 0  psi{ @   pre @  =  IF  \  Find the "pre" concept...
                1   I  1 psi{ +!  \ 8sep2005 minor "pre" boost
                    I  1 psi{ @  0 < IF   \ 7aug2005
                0   I  1 psi{  !          \ Reset to zero;
                    THEN   \ Do not permit negative activations.
        I 1 psi{ @ 63 > IF  \ If activation-limit has been exceeded
          63 I 1 psi{ !     \ set the activation back to the limit.
        THEN
        I zone  @  6 - > IF LEAVE THEN  \ Expect no long words.
      THEN       \  End of middle if-clause.
    -1  +LOOP    \  End of backwards loop.
  THEN           \  End of outer if-clause.
  seq @ 0 > IF   \  If a sub(seq)uent concept exists,
  \ 7jan2008 [ ] A more general search-find-exit coding is needed here.
    zone @ 9 +   zone @  DO   \ 14dec2007 Debugging slosh-over.
  \ fyi @ 1 > IF              \ 14dec2007 Inspect zone for debugging.
  \   CR ." spredAct: seq zone = " zone @ .   \ 14dec2007 Debugging.
  \ THEN          \ 14dec2007 End of fyi test to debug slosh-over.
      I 0  psi{ @   seq @  =  IF  \ If a match of "seq" is found,
        fyi @ 2 = IF  \ 9nov2005 In Tutorial show spreading activation.
          pov @ 35 = IF  \ 24sep2005 Do not interrupt input with messages.
            0 psi7 !         \ 24aug2008 safety measure.
            midway @ t @ DO  \ Look beyond verb for the "seq" concept
              I   0  psi{ @   oldpsi @  =  IF  \ If "oldpsi" match found,

                I 7  psi{ @   psi7 !    \ 24aug2008 Get the enx as psi6
                LEAVE                   \ Stop looking after one find.
              THEN       \  End of check for the "oldpsi" concept
            -1  +LOOP    \  End of backwards search for "oldpsi" concept
            midway @ t @ DO  \ Use enx to get the aud recall-vector
              I   0  en{ @    psi7 @ = IF  \ 27aug2008
                I 6  en{ @  rv ! \ 27aug2008 Store auditory recall-vector.
                LEAVE    \ Use only the most recent auditory engram.
              THEN
            -1  +LOOP    \ End of backwards search for "psi6" vocab item.
            0 rv !  \ Zero out the auditory associative tag.
            midway @ t @ DO  \ Look beyond verb for the "seq" concept
              I   0  psi{ @   seq @  =  IF  \ If a match of "seq" found,
                I 7  psi{ @   psi7 !    \ 24aug2008 Get the enx as psi7
                LEAVE                   \ Stop looking after one find.
              THEN       \  End of check for the "seq" concept
            -1  +LOOP    \  End of backwards search for "seq" concept
            midway @ t @ DO  \ Use enx to get the aud recall-vector
              I   0  en{ @    psi7 @ = IF  \ 27aug2008
                I 6  en{ @  rv ! \ 27aug2008 Store auditory recall-vector.
                LEAVE    \ Use only the most recent auditory engram.
              THEN
            -1  +LOOP    \ End of backwards search for "psi6" vocab item.
            rv @ 0 > IF  \ Avoid crashes if rv is negative.
              BEGIN
                rv @ 0 aud{ @ EMIT  1 rv +!
                rv @ 0 aud{ @ 32 =  \ Using a blank SPACE-bar.
              UNTIL        \ Stop when a blank space is found.
            THEN
            0 rv !  \ Zero out the auditory associative tag.
            ."  "  \ 9nov2005 For gap between words on one line.
          THEN    \ 24sep2005 End of test to display only internal thought.
        THEN      \  9nov2005 End of test for Tutorial-only mode.
        fyi @ 2 > IF  \ 9nov2005 In diagnostics show spreading activation.
          pov @ 35 = IF  \ 24sep2005 Do not interrupt input with messages.
            CR
            0 psi7 !         \ 24aug2008 safety measure.
            midway @ t @ DO  \ Look beyond verb for the "seq" concept
              I   0  psi{ @   oldpsi @  =  IF  \ If "oldpsi" match found,
                I 7  psi{ @   psi7 !    \ 24aug2008 Get the enx as psi6
                LEAVE                   \ Stop looking after one find.
              THEN       \  End of check for the "oldpsi" concept
            -1  +LOOP    \  End of backwards search for "oldpsi" concept
            midway @ t @ DO  \ Use enx to get the aud recall-vector
              I   0  en{ @    psi7 @ = IF  \ 27aug2008
                I 6  en{ @  rv ! \ 27aug2008 Store auditory recall-vector.
                LEAVE    \ Use only the most recent auditory engram.
              THEN
            -1  +LOOP    \ End of backwards search for "psi6" vocab item.
            rv @ 0 > IF  \ Avoid crashes if rv is negative.
              BEGIN
                rv @ 0 aud{ @ EMIT  1 rv +!
                rv @ 0 aud{ @ 32 =  \ Using a blank SPACE-bar.
              UNTIL        \ Stop when a blank space is found.
            THEN
            0 rv !  \ Zero out the auditory associative tag.
            ."  #" oldpsi @ . ." w. act " oldact @ .
            ." at I = " I . ." sending spike "
            spike @ . ." to seq #" seq @ .
            midway @ t @ DO  \ Look beyond verb for the "seq" concept
              I   0  psi{ @   seq @  =  IF  \ If a match of "seq" found,
                I 7  psi{ @   psi7 !    \ 24aug2008 Get the enx as psi7
                LEAVE                   \ Stop looking after one find.
              THEN       \  End of check for the "seq" concept
            -1  +LOOP    \  End of backwards search for "seq" concept
            midway @ t @ DO  \ Use enx to get the aud recall-vector
              I   0  en{ @    psi7 @ = IF  \ 27aug2008
                I 6  en{ @  rv ! \ 27aug2008 Store auditory recall-vector.
                LEAVE    \ Use only the most recent auditory engram.
              THEN
            -1  +LOOP    \ End of backwards search for "psi6" vocab item.
            rv @ 0 > IF  \ Avoid crashes if rv is negative.
              BEGIN
                rv @ 0 aud{ @ EMIT  1 rv +!
                rv @ 0 aud{ @ 32 =  \ Using a blank SPACE-bar.
              UNTIL        \ Stop when a blank space is found.
            THEN
            0 rv !  \ Zero out the auditory associative tag.
            ."  at act " I 1 psi{ @ . ." yields "  \ after increment...
          THEN    \ 24sep2005 End of test to display only internal thought.
        THEN      \  9nov2005 End of test for Diagnostic mode.
        spike   @   I  1 psi{ +!  \ 2aug2005 "spike" and not "bulge"
        fyi @ 2 > IF     \ 9nov2005 In Diagnostic-only mode...
          pov @ 35 = IF  \ 24sep2005 Do not interrupt input with messages.
            I 1 psi{ @ . \ show accumulating activation
            fyi @ 2 > IF \ In Diagnostic mode...
              ." and zone = " zone @ .      \ 9sep2005
            THEN         \ End of test of "fyi" flag status.
          THEN    \ 24sep2005 End of test to display only internal thought.
        THEN             \ End of test for Tutorial mode
          fyi @ 3 = IF    \ In diagnostic mode...
            I 1 psi{ @ . ." (lim = 63) for t=" I rv !
              BEGIN      \ Start looking for a word-beginning.
                -1 rv +!             \ decrement rv until
                rv @ 3 aud{ @ 1 =    \ ... a beg=1 is found
              UNTIL
              rv @ .   \ Show the time of the auditory engram.
              BEGIN
                rv @ 0 aud{ @ EMIT  1 rv +!
                rv @ 0 aud{ @ 32 =  \ Using a blank SPACE-bar.
              UNTIL        \ Stop when a blank space is found.
              ."  engram; spike = " spike @ .
              0 rv !  \ Zero out the auditory associative tag.
           THEN
          I 1 psi{ @ 63 > IF  \ If activation-limit has been exceeded
            63 I 1 psi{ !     \ set the activation back to the limit.
          THEN
        I zone  @  6 + > IF
          fyi @ 2 > IF   \ 13oct2005 Make room for Transcript mode.
            CR ." executing LEAVE at zone = " zone @ .
          THEN           \ 9sep2005 end of debug-test
          LEAVE
        THEN     \  Expect no long words.
      THEN       \  End of middle if-clause.
    LOOP         \  End of forwards loop.
  THEN           \  End of outer if-clause.
;  \ End of SPREADACT; return to Activate or nounAct or verbAct.


\ nounAct is called from nounPhrase so as to activate equally all the
\ nodes of a noun-fiber with the same activation that won selection.
:  nounAct ( re-activate all recent nodes of a concept ) \ 1sep2008
  fyi @ 2 > IF CR  \ Too much detail for Tutorial mode.
  ."     Calling nounAct (not in AI4U). psi = " psi @ . CR
  THEN  \ Parsing and NLP generation may need separate activations.
  psi @  0 >  IF     \ to avoid psi = 0 = psi0
    fyi @ 2 > IF  \ 13oct2005 Leave room for Transcript display.
      CR ."       nounAct calls spreadACT to xfer proportionate "
      ." act. from each node of concept #" psi @ .
    THEN          \ 5sep2005 End of test for Tutorial mode.
    midway @   t @  DO  \ 7apr2007 Avoid "array boundary problem".
      I 0 psi{ @ psi @ = IF  \  If concept "psi" is found...
      I 1 psi{ @ psi1 !   \ 31mar2007 Extract the psi1 activation.
  nounval @  0 > IF nounval @ psi1 ! THEN  \ Set a minimum.

     psi1 @ 63 > IF  \ 22jan2008 Testing for excess of limit
     63 psi1 !       \ 22jan2008
     THEN            \ 22jan2008 Keep activation below 64.

     psi1 @ I  1 psi{ !   \  3apr2007 Prevent a zero "spike" value.

            I  0 psi{ @  54 = IF  \ 15oct2005 Check for #54 "WHAT"
          0 I  1 psi{ !   \ Zero "WHAT" for sake of other concepts.
            THEN   \ 15oct2005 End of test for concept #54 "WHAT"
                  12 spike !  ( 3apr2007 Aim for ample spikes.)
          I  4 psi{ @  pre !  ( 24aug2008 for use in SPREADACT )
          I  6 psi{ @  seq !  ( 24aug2008 for use in SPREADACT )
          I           zone !  ( for use in SPREADACT )
          I  1 psi{ @  0 = IF  0 spike ! THEN
          I  1 psi{ @  5 > IF 12 spike ! THEN  \ 31mar2007 from JSAI
       \  1feb2008 Trying larger spikes for sake of slosh-0ver:
          I  1 psi{ @ 10 > IF 14 spike ! THEN  \  1feb2008
          I  1 psi{ @ 15 > IF 16 spike ! THEN  \  1feb2008
          I  1 psi{ @ 20 > IF 18 spike ! THEN  \  1feb2008
          I  1 psi{ @ 25 > IF 20 spike ! THEN  \  1feb2008
          I  1 psi{ @ 30 > IF 22 spike ! THEN  \  1feb2008
          I  1 psi{ @ 35 > IF 24 spike ! THEN  \  1feb2008
          I  1 psi{ @ 40 > IF 26 spike ! THEN  \  1feb2008
          I  1 psi{ @ 45 > IF 28 spike ! THEN  \  1feb2008
          I  1 psi{ @ 50 > IF 30 spike ! THEN  \  1feb2008
          I  1 psi{ @ 55 > IF 32 spike ! THEN  \  1feb2008
          I  1 psi{ @ 60 > IF 34 spike ! THEN  \  1feb2008

        psi @ oldpsi ! \ 6sep2005 To show source of spreading activations.
          I  1 psi{ @ oldact !  \ To show source of spreading activations.
          I  5 psi{ @ oldpos !  \ 24aug2008 verbs as a special case
        seq @  0 > IF    \ 3apr2007 From JSAI: Only call if warranted.
          SPREADACT             ( for spreading activation )
        THEN   \ 3apr2007 End of available-seq-test for calling spreadAct
        precand @ pre !  \ 3apr2007 From JSAI
        0 oldpos !  \ 8sep2005 safety measure
        0 oldpsi !  \ 6sep2005 safety measure.
        0 oldact !  \ 6sep2005 safety measure.
        0  pre !              \ blank out "pre" for safety
      THEN                    \ end of test for "psi" concept
      \ 3apr2007 If the active concept is not the selection-winner...
      I    0 psi{ @ psi @ = NOT IF  \ If concept "psi" is not found...
         I 5 psi{ @ 5 = IF  \ 24aug2008 If the concept is also-ran noun

            I 1 psi{ @ unk !  \ 9apr2007 Extract the psi1 activation.
              0 unk !         \ 9ap22007 Reset for safety.
            I 1 psi{ @ < 0 IF \ 3apr2007 If psi1 is below zero
          0 I 1 psi{ !        \ If activation is below zero.
           THEN   \ 3apr2007 End of test for subzero activation
         THEN   \ 3apr2007 End of test to detect also-ran nouns.
      THEN   \ 3apr2007 End of test for non-psi concepts.
    -1  +LOOP                 \ end of backwards loop
  THEN  \  End of check for non-zero "psi" from nounPhrase module.
  0 spike !  \  1feb2008 Try to prevent carry-over elsewhere.
;  \ End of nounAct; return to the nounPhrase calling module.


\ verbAct is called from verbPhrase so as to engender a nodal slosh-over
\ of subject-noun plus verb activation onto a correct direct object.
:  verbAct ( re-activate all recent nodes of a verb )  \ 24aug2008
  verbval @  33 <  IF   \ 27apr2007 If verbval is at subconscious level
    33  verbval @ -  verbinc !  \ 27apr2007 Store the verb-increment.
    ELSE  0 verbinc !  \ 27apr2007 Otherwise set verbinc to zero.
  THEN  \ 27apr2007 End of test of verbval from verbPhrase module.

  fyi @ 2 > IF CR  \ Enough detail for Diagnostic but not Tutorial mode.
  ."     Calling verbAct (not in AI4U). psi = " psi @ . CR
  THEN  \ Parsing and NLP generation may need separate activations.
  psi @  0 >  IF     \ to avoid psi = 0 = psi0

    fyi @ 2 > IF  \ 13oct2005 Leave room for Transcript display.
      CR ."       verbAct calls spreadACT to xfer proportionate "
      ." act. from each node of concept #" psi @ .
    THEN          \ 5sep2005 End of test for Tutorial mode.

    psi @ oldpsi !  \ 3apr2007 From JSAI: Preserve oldpsi for spreadAct
    midway @   t @ DO  \  7apr2007 Avoid "array boundary problem"
      I 0 psi{ @ psi @ = IF  \  If concept "psi" is found...

        fyi @ 1 > IF      \  9jun2006 If in Tutorial mode or higher...
          I 1 psi{ @ 8 > IF  \ 13jun2006 If verb node has an activation
          \ high enough to indicate that slosh-over is happening.
            ." +"         \ Simple character to suggest slosh-over.
          THEN            \ End of activation-test.
        THEN              \ End of test for Tutorial display mode.

            I  1 psi{ @  psi1 !  \ 30mar2007 Verb-node activation level.
              \  psi1 @  psi1 +! \ 30mar2007 Double the psi1 value.
            \ verbinc @  psi1 +! \ 27apr2007 Add "verb increment".
            \ verbinc @  psi1 +! \ 22jan2008 Comment out for test.
              \     16  psi1 +! \ 30mar2007 Amplify for slosh-over.
     psi1 @ I  1 psi{ !   \  30mar2007 Set activation on each psi-node.
            I  0 psi{ @  54 = IF  \ 15oct2005 Check for #54 "WHAT"
          0 I  1 psi{ !   \ Zero "WHAT" for sake of other concepts.
            THEN   \ 15oct2005 End of test for concept #54 "WHAT"

          I  1 psi{ @ 63 > IF  \ If activation-limit has been exceeded
            63 I 1 psi{ !      \ set the activation back to the limit.
          THEN  \ 14sep2005 Back to 63 limit for porting to JavaScript AI.
          I  4 psi{ @  pre !  ( 24aug2008 for use in SPREADACT )
          I  6 psi{ @  seq !  ( 24aug2008 for use in SPREADACT )
          I           zone !  ( for use in SPREADACT )
 \ 30mar2007: Subject & verb activation sloshes over onto direct object.
          I  1 psi{ @  0 = IF  0 spike ! THEN
          I  1 psi{ @  0 > IF  1 spike ! THEN  \  1feb2008
          I  1 psi{ @  5 > IF  2 spike ! THEN  \  1feb2008
          I  1 psi{ @ 10 > IF  8 spike ! THEN  \  1feb2008
          I  1 psi{ @ 15 > IF 16 spike ! THEN  \  1feb2008
          I  1 psi{ @ 20 > IF 20 spike ! THEN  \  1feb2008
          I  1 psi{ @ 25 > IF 24 spike ! THEN  \  1feb2008
          I  1 psi{ @ 30 > IF 28 spike ! THEN  \  1feb2008
          I  1 psi{ @ 35 > IF 32 spike ! THEN  \  1feb2008
          I  1 psi{ @ 40 > IF 36 spike ! THEN  \  1feb2008
          I  1 psi{ @ 45 > IF 40 spike ! THEN  \  1feb2008
          I  1 psi{ @ 50 > IF 44 spike ! THEN  \  1feb2008
          I  1 psi{ @ 55 > IF 48 spike ! THEN  \  1feb2008
          I  1 psi{ @ 60 > IF 52 spike ! THEN  \  1feb2008

        psi @ oldpsi ! \ 6sep2005 To show source of spreading activations.
          I  1 psi{ @ oldact !  \ To show source of spreading activations.
          I  5 psi{ @ oldpos !  \ 24aug2008 verbs as a special case
        seq @  0 > IF           \ 3apr2007 JSAI: Insist on a positive "seq".
          SPREADACT             ( for spreading activation )
        THEN                    \ 3apr2007 End of test for a positive "seq"
        0 oldpos !  \ 8sep2005 safety measure
        0 oldpsi !  \ 6sep2005 safety measure.
        0 oldact !  \ 6sep2005 safety measure.
        0  pre !              \ blank out "pre" for safety
        0  seq !              \ blank out "seq" for safety
      THEN                    \ end of test for "psi" concept
    -1  +LOOP                 \ end of backwards loop
  THEN  \  End of check for non-zero "psi" from verbPhrase module.
  0 verbinc !  \ 27apr2007 Reset verb-increment to zero for safety.
;  \ End of verbAct; return to the verbPhrase calling module.


\ ACTIVATE is called from OLDCONCEPT so as to
\ reactivate older nodes of a newly active concept.
:  ACTIVATE ( re-activate all recent nodes of a concept )  \  1sep2008
  fyi @ 2 > IF CR  \ Too much detail for Tutorial mode.
  ."     Calling ACTIVATE. psi = " psi @ . CR
  THEN  \ Parsing and NLP generation may need separate activations.
  0 spike !   \ 1apr2007 From Mind.Forth to JSAI to Mind.Forth
  psi @  0 >  IF     \ to avoid psi = 0 = psi0

    fyi @ 2 > IF  \ 13oct2005 Leave room for Transcript display.
      CR ."       ACTIVATE calls spreadACT to xfer proportionate "
      ." act. from each node of concept #" psi @ .
    THEN          \ 5sep2005 End of test for Tutorial mode.

    midway @   t @  DO  \ 7apr2007 Avoid "array boundary problem"
      I 0 psi{ @ psi @ = IF  \  If concept "psi" is found...
         16 I  1 psi{ +!  \ 1apr2007 Minor increment as done in JSAI.
            I  0 psi{ @  54 = IF  \ 15oct2005 Check for #54 "WHAT"
          0 I  1 psi{ !   \ Zero "WHAT" for sake of other concepts.
            THEN   \ 15oct2005 End of test for concept #54 "WHAT"
          I  1 psi{ @ 63 > IF  \ If activation-limit has been exceeded
            63 I 1 psi{ !      \ set the activation back to the limit.
          THEN  \ 14sep2005 Back to 63 limit for porting to JavaScript AI.
                               1 spike !       \ 1apr2007 From JSAI
          I  1 psi{ @  0 = IF  0 spike ! THEN  \ 1apr2007 From JSAI
          I  1 psi{ @  5 > IF  7 spike ! THEN  \ 1apr2007 From JSAI
          I  1 psi{ @ 10 > IF  8 spike ! THEN  \ 1apr2007 From JSAI
          I  1 psi{ @ 15 > IF  9 spike ! THEN  \ 1apr2007 From JSAI
          I  1 psi{ @ 20 > IF 10 spike ! THEN  \ 1apr2007 From JSAI
          I  1 psi{ @ 25 > IF 11 spike ! THEN  \ 1apr2007 From JSAI
          I  1 psi{ @ 30 > IF 12 spike ! THEN  \ 1apr2007 From JSAI
          I  1 psi{ @ 35 > IF 13 spike ! THEN  \ 1apr2007 From JSAI
          I  1 psi{ @ 40 > IF 14 spike ! THEN  \ 1apr2007 From JSAI
          I  1 psi{ @ 45 > IF 15 spike ! THEN  \ 1apr2007 From JSAI
          I  1 psi{ @ 50 > IF 16 spike ! THEN  \ 1apr2007 From JSAI
          I  1 psi{ @ 55 > IF 17 spike ! THEN  \ 1apr2007 From JSAI
          I  1 psi{ @ 60 > IF 18 spike ! THEN  \ 1apr2007 From JSAI
                psi @ oldpsi !  \ To show source of spreading activations.
          I  1 psi{ @ oldact !  \ To show source of spreading activations.
          I  5 psi{ @ oldpos !  \ 24aug2008 verbs as a special case
          I  4 psi{ @  pre !  ( 24aug2008 for use in SPREADACT )
          I  6 psi{ @  seq !  ( 24aug2008 for use in SPREADACT )
          I           zone !  ( for use in SPREADACT )
        SPREADACT             ( for spreading activation )
        0 oldpos !            \ safety measure
        0 oldpsi !            \ safety measure.
        0 oldact !            \ safety measure.
        0    pre !            \ blank out "pre" for safety
        0    seq !            \ blank out "seq" for safety
        0   psi1 !            \ Precaution against spurious psi1.
        1  spike !            \ A basic value as a reset.
      THEN                    \ end of test for "psi" concept
    -1  +LOOP                 \ end of backwards loop

\   148 caller !        \ 13jan2008 Activate identified by AI4U page.

    0 caller !          \ 13jan2008 Reset caller-ID for safety.
    0 urpsi !     \ 13jan2008 Reset for safety.
  THEN  \  End of check for non-zero "psi" from OLDCONCEPT.
;  \ End of ACTIVATE; return to OLDCONCEPT.


\  INSTANTIATE is called from the PARSER
\  module to create a new node of a Psi concept.
:  INSTANTIATE ( create a concept-fiber node )  \ 27aug2008
  precand @ 0 > IF precand @ pre ! THEN  \ 2apr2007 From JSAI.
  ordo @ 1 = IF  0 prevtag ! THEN  \ 2apr2007 For initial words.
  lastword @ 1 = IF   \ 2apr2007 If lastword-condition is true...
    0 seq !       \ 2apr2007 No seq tag for final thought-concept.
    0 lastword !  \ 2apr2007 Reset for next time.
  THEN  \ 2apr2007 End of test for last word in a sentence.
  ( concept fiber psi )             psi @  t @  0 psi{ !
  ( Set "act" activation level. )   act @  t @  1 psi{ +!
  ( Set "num" number flag       )   num @  t @  2 psi{ ! \ 24aug2008
  ( Store JUXtaposition tags. )     jux @  t @  3 psi{ ! \ 24aug2008
  ( Store PREvious associand. ) prevtag @  t @  4 psi{ ! \ 24aug2008
  ( Store functional pos code. )    pos @  t @  5 psi{ ! \ 24aug2008
  ( Store the subSEQuent tag. )     seq @  t @  6 psi{ ! \ 24aug2008
  ( Store the EN-transfer tag. )    enx @  t @  7 psi{ ! \ 24aug2008
  0 num !          \ 27aug2008 Reset to prevent carry-overs.
  0 preset !       \  2apr2007 Reset for safety.
  psi @ prevtag !  \  2apr2007 For the next instantiation.
  ordo @ 1 > IF    \  2apr2007 New way of inserting "seq" tags.
    psi @ seq !    \  2apr2007 Current psi will be previous "seq".
    vault @  t @ 2 -  DO   \ Do not change bootstrap in "vault".
       I 1 psi{ @  0 > IF  \ Upon finding the most recent concept
       \ seq @ I 5 psi{ !   \ Perhaps use "psi" without using "seq"?
         seq @ I 6 psi{ !   \ 24aug2008 "num" pushes other psi-flags
\ CR ." Inst: lastword = " lastword @ .  ." & seq = " seq @ . CR
         LEAVE         \ Store one instance, then "leave" the loop.
       THEN            \ End of test to find most recent concept.
    -1 +LOOP           \ Loop backwards by decrements of one.
  THEN                 \ End of test for post-initial word order.
  0 seq !  \ 2apr2007 Possibly to prevent spurious "seq" engrams.
;  \ End of INSTANTIATE; return to the Parser module.


\  enVocab is called from bootstrap, NEWCONCEPT or OLDCONCEPT
\  to create a node on a quasi-concept-fiber by "attaching"
\  to it associative tags for En(glish) vocab(ulary).
:  enVocab ( English Vocabulary node creation ) \ 27aug2008
  ( Number "nen" of English )       nen @  t @  0  en{ !
  ( Do not store the activation level; it is a transient.)
  ( Store "num" number tag. )       num @  t @  2  en{ ! \ 27aug2008
  ( Store mindcore EXit tag. )      fex @  t @  3  en{ ! \ 27aug2008
  ( Store part of speech "pos".)    pos @  t @  4  en{ ! \ 27aug2008
  ( Store mindcore IN tag. )        fin @  t @  5  en{ ! \ 27aug2008
  ( Store the auditory "aud" tag. ) aud @  t @  6  en{ ! \ 27aug2008
;  \ End of enVocab; return to OLDCONCEPT or NEWCONCEPT.


\ PARSER is called from oldConcept or newConcept
\ to help the Artificial Mind comprehend verbal input
\ by properly assigning associative tags with flags.
:  PARSER ( determine the part of speech ) \  1feb2008
  \ The "bias" has no control over recognized oldConcept words:
  5 bias !  \ Initial bias is for a noun=5.
  36 act !  \  1feb2008 A value subject to optimization.
  INSTANTIATE  \ Create a new instance of a Psi concept.
  \ After a word is instantiated, expectations may change.
  \ Recognitions and expectations are pos-codeterminants.
  pos @ 5 = IF  8 bias !  THEN  \ After noun, expect a verb.
  pos @ 8 = IF  5 bias !  THEN  \ After verb, expect a noun.
  psi @  jux !  \ but only for the next time around, not now.
;  \ End of Parser; return to oldConcept or newConcept.


\ OLDCONCEPT is called from AUDITION to create a
\ fresh concept-node for a recognized input word.
:  OLDCONCEPT ( recognize a known word ) \  1sep2008
 \  CR ." oldC: t = " t @ . ." and psi = " psi @ . CR \ 2apr2007 test
  24 act !         \  1feb2008 A value subject to optimization.
  \ 7sep2005 Actual value may be set in the ACTIVATE module. .
  midway @ t @ DO  \ Cycle back through the English lexicon.
    I 0 en{ @ psi @ = IF   \ If psi-tag matches lexical concept,

    I 2 en{ @    0 > IF    \ 27aug2008 If it is greater than zero,
    I 2 en{ @  num ! THEN  \ 27aug2008 Retain num(ber) values.

    I 3 en{ @    0 > IF \ 27aug2008 If it is greater than zero,
    I 3 en{ @  fex ! THEN  \ 27aug2008 retrieve the fiber-out flag.

    I 4 en{ @    0 > IF \ 27aug2008  So as to parse by word-recognition,
    I 4 en{ @  pos ! THEN \ 27aug2008 retrieve most recent part-of-speech.

    I 5 en{ @    0 > IF \ 27aug2008 If it is greater than zero,
    I 5 en{ @  fin ! THEN  \ 27aug2008 retrieve the fiber-in flag.
    LEAVE        \ Use only the most recent English engram-node.
    THEN         \ End of searching if-clause.
  -1 +LOOP       \ End of the backwards loop.
  \ Next code stores "psi" directly instead of calling enVocab,
  \ so that value of "psi" will not interfere with "nen" value:
  ( "psi" as found by audRecog )    psi @  t @  0  en{ !
  ( Store zero activation. )          0    t @  1  en{ !
  ( Store "num" number tag. )       num @  t @  2  en{ ! \ 27aug2008
  ( Store mindcore EXit tag. )      fex @  t @  3  en{ ! \ 27aug2008
  ( Store part of speech "pos".)    pos @  t @  4  en{ ! \ 27aug2008
  ( Store mindcore IN tag. )        fin @  t @  5  en{ ! \ 27aug2008
  ( Store the auditory "aud" tag. ) aud @  t @  6  en{ ! \ 27aug2008
  \ The Robot Mind as a seed AI for Technological Singularity
  \ approaches artificial consciousness in the following code:
  pov @  35 = IF fex @ psi ! THEN   \ during internal (#) "pov";
  pov @  42 = IF fin @ psi ! THEN   \ external (*) "pov"
  psi @  enx !      \ Assume Psi number = En(glish) number.
  psi @ 54 = IF 8 act ! THEN  \ Subactivate question "what".
  psi @ 55 = IF 8 act ! THEN  \ Subactivate question "who".
  psi @ 59 = IF 8 act ! THEN  \ Subactivate auxiliary "do".
  psi @  7 = IF 1 act ! THEN  \ 25aug2008 Article "the"
  PARSER            \ Determine the part-of-speech "pos".
    0 pos !         \ Reset the part-of-speech "pos" flag.
  fyi @ 2 > IF CR          \ In Diagnostic mode.
  ."   from oldConcept "   \ Seeing what calls Activate.
  THEN              \ End of test for Diagnostic mode
  pov @ 42 = IF       \ 5jun2006 Only activate external input.
    pre   @  urpre !  \ Hold value of "pre" safe during ACTIVATE.
    ACTIVATE          \ Re-activate recent nodes of the concept.
    urpre @    pre !  \ Restore the value of "pre".
  THEN                \ 5jun2006 Internal mode calls superAct direcly.
  0 act !       \ Reset for safety.
  \ 26jul2002 Diagnostic test code:
  pov @ 35 = IF  \ 26jul2002 Make sure pov is internal,
    1 match !  \ 26jul2002 Declare word-match for SPEECH.
  THEN  \ 26jul2002 End of test for internal pov=35 state.
;  \ End of OLDCONCEPT; return to AUDITION.


\  NEWCONCEPT is called from AUDITION when the Robot
\  AI Mind must learn the concept of a new word.
:  NEWCONCEPT ( machine learning of new concepts ) \ 29jan2008
  1 nen +!     \ Increment "nen" beyond English bootstrap concepts.
  nen @ psi !  \ Let psi & n(umeric) En(glish) have same identifier.
  nen @ fex !  \ Let f(iber)-ex also the same numeric identifier.
  nen @ fin !  \ Let f(iber)-in have the same numeric identifier.
  bias @ pos ! \ Expectancy from Parser module.
  enVocab ( to create an ENglish vocabulary node )
  0 fex !       \ blank the fiber-out flag;
  0 fin !       \ blank the fiber-in flag;
  nen @  enx !  \ Set the transfer-to-English "enx" flag.
  PARSER        \ Determine the part-of-speech "pos", then instantiate.
  0 pos !       \ Reset the part-of-speech variable.
  0 act !       \ Reset for safety.
;  \ End of NEWCONCEPT; return to AUDITION.


\ The audRecog module aims for the following entelechy goals.
\ [ ] Recognize animal vocalizations as well as human speech.
\ [ ] Receive Morse and other codes as well as speech or text.
\ [ ] Detect human foreign-language accents as per language.
\ [ ] Recognize particular voices of particular individuals.
\ [ ] Switch from keyboard input to recognizing acoustic speech.
\ [ ] Recognize words despite slight variations from correct form.
\ [ ] Detect prefixes, infixes and suffixes.
\ [ ] Recognize multiple roots within compound nouns.
\ [ ] Recognize both singular and plural forms of the same noun.
\ [X] Recognize plural noun forms as a word leading to a concept.
\ audRecog (auditory recognition) comparator is called from audSTM
\ to recognize words by matching input phonemes against memory.
\ Anyone may code gusRecog, olfRecog, tacRecog, or visRecog.
:  audRecog ( auditory Recognition )  \  1sep2008
  0  psi !   \ Safety meaure.
  8 act !      \ 5dec2007 Try to discriminate among incremental acts.
  0 actbase !  \ 5dec2007 Start with zero to look for higher act.
  midway @  spt @ DO    \ Search from blank time back to midway.
    I 0 aud{ @ pho @ = IF  \ If incoming pho matches stored aud0;
      I 1 aud{ @ 0 = IF    \ if matching engram has no activation;
        I 3 aud{ @ 1 = IF  \ if beg=1 on matching no-act aud engram;
        \ Presence or absence of aud5 does not matter in next code
        \ because it can only involve a one-letter beg=1 match:
        \ I 5 aud{ @ psi !  \ fetch any aud5 as a potential psi.
          I 4 aud{ @ 1 = IF   \ If beg-aud has ctu=1 continuing,
            8 I 1+   1 aud{ ! \ activate the N-I-L character,
            \ so that match-up may continue past first char.
            0 psi !  \ Revoke any assignment of a match.
          ELSE       \ From JavaScript AI code.

          len @ 1 = IF  \ 5dec2007 Set monopsi for single chars.
            I 5 aud{ @  monopsi !  \ A tentative match-up.

         \  CR ." audRecog psi = " psi @ .         \ 23dec2007
         \  CR ." audRecog monopsi = " monopsi @ . \ 23dec2007

          THEN   \ 5dec2007 End of test for one char length.

          THEN   \ end of test for continuation of beg-aud
        THEN   \ end of test for a beg(inning) non-active aud0
      THEN   \ end of test for matching aud0 with no activation
      \ The following can happen only with non-initial chars
      \ and could also be expressed with an ELSE statement:
      I 1 aud{ @ 0 > IF  \ If matching aud0 has activation,
        0 psi !          \ zero out any previous psi-tag,
        \ because obviously the match-up is not complete.
        I 4 aud{ @ 1 = IF   \ If act-match aud0 has ctu=1 continuing,
          2 act +!  \ 5decd007 Increment act for discrimination.
          0 psi !           \ because match-up is not yet complete,
      \   8 I 1+   1 aud{ ! \ activate the N-I-L character.
      act @ I 1+   1 aud{ ! \ 5dec2007 Increment for discrimination.
        THEN    \ end of test for active-match aud0 continuation

        \  9jan2008 Special measures for recog of two-letter words.
        \  9jan2008 If necessary, trap for all lengths up to a limit.
        I 4 aud{ @ 0 = IF   \  9jan2008 If ctu=0 indicates end of word
          len @ 2 = IF   \  9jan2008 If len(gth) is only two characters.
        \   CR ." audRecog Length at end = " len @ .   \  9jan2008 Test
        \   I 1 aud{ @ 8 = IF  \  9jan2008 If act is at eight
            I 1 aud{ @ 0 > IF  \  9jan2008 Or test for eight (8).
              I 5 aud{ @ psibase !  \  9jan2008 Assume a match.
        \     CR psibase @ .  ." May be a two-letter match. "
            THEN               \  9jan2008 End of test for act=8
          THEN           \  9jan2008 End of test for two-letter words.
        THEN             \  9jan2008 End of test for end of word.
        \  9jan2008 Above bug-fix may require refinement later.

        \ 23dec2007 A test for activation is now being introduced,
        \ because an aboriginal bug has been failing to detect
        \ previously unknown words and has instead been picking up
        \ spurious psi-tags from questionable sources.

   \  I 1 aud{ @ 0 > IF  \ 23dec2007 If matching aud0 has activation,
      I 1 aud{ @ 8 > IF  \ 23dec2007 If activation higher than initial

 \  CR ." audRecog pre-ctu-test act = "  I 1 aud{ @ .  \ 23dec2007 test

        I 4 aud{ @ 0 = IF   \ If matching word-engram now ends,

 \  CR ." audRecog ctu=zero act = "  I 1 aud{ @ .  \ 23dec2007 test

          I 1 aud{ @ actbase @ > IF  \ 5dec2007 Test for high act.
            I 5 aud{ @ psi ! \ fetch the potential psi-tag,
            \ which may be a valid recognition if the input stops.

         \  CR ." audRecog no-ctu psi = " psi @ . \ 23dec2007 test

            I 5 aud{ @ psibase !  \  5dec2007 Hold onto winner.
            I 1 aud{ @ actbase !  \  5dec2007 Winner is new actbase.

          THEN   \ 5dec2007 End of test for act higher than actbase.
        ELSE
          0 psi !  \  No match if the stored word does not end.
          monopsi @ 0 > IF   \ but if a one-char was found,
            monopsi @ psi !  \ use the one-letter recognition.
            0 monopsi !      \ Reset monopsi to zero.
          THEN               \ End of inner test.
        \  0 psi !  \  No match if the stored word does not end.
        THEN   \ End of test for final char that has a psi-tag.
      THEN     \ 23dec2007 End of test for engram-activation above eight.

      THEN     \ End of test for matching aud0 with activation.
    THEN       \ End of test for a character matching "pho".
  -1 +LOOP     \ End of looping backwards from "spt".

  0 act !      \ 5dec2007 Reset act to zero.
  0 actbase !  \ 5dec2007 Reset to zero.

  psibase @ 0 > IF      \ 5dec2007
     psibase @  psi !   \ 5dec2007 Use any winning psibase value.
  THEN                  \ 5dec2007

\ CR ." debug audRecog; psi & monopsi = " psi @ .  monopsi @ . \ 4dec07

    psi @ 0 = IF         \ If no multi-char recognition,
      monopsi @ 0 > IF   \ but if a one-char was found,

\  CR ." in audRecog; monopsi & len = " monopsi @ . len @ . \ 5dec07
\  CR ." in audRecog; monopsi & len = " monopsi @ . len @ . \  9jan2008

       len @ 2 < IF  \ 5dec2007 Prevent monopsi interference w. psi.

         monopsi @ psi !  \ use the one-letter recognition.
       THEN          \ 5dec2007 End of test to ensure single char monopsi.
       0 monopsi !      \ Reset monopsi to zero.

       psi @ 0 = IF   \ 5dec2007 If psi is still at zero...
         psibase @ 0 > IF  \ 5dec2007 If there is a positive psibase...
            psibase @ psi !  \ 5dec2007 Make psibase the psi output.
            \ 5dec2007 Make sure here is the right place for psibase?
         THEN         \ 5dec2007 End of test for positive psibase.
       THEN           \ 5dec2007 End of test for zero psi value.

     THEN               \ End of inner test.
   THEN  \ From JavaScript AI Mind code.
   0 psibase !  \ 5dec2007 Reset.
;  \ End of audRecog; return to short term memory audSTM.


\ audSTM is called from the AUDITION module.
:  audSTM ( auditory Short Term Memory ) \ atm 14oct2005
  t @ vault @ > IF  \ If time has advanced beyond bootstrap,
    pho @ 32 > IF  audRecog  THEN  ( ASCII 32 = SPACE-bar )
  THEN  \ end of test to prevent "recognition" of bootstrap.
    t @ 1-  0 aud{ @  0 = IF  1 beg !  THEN  \ zero  2sep2005 X 0=
    t @ 1-  0 aud{ @ 32 = IF  1 beg !  THEN  \ SPACE-bar.
    pho @  t @  0 aud{ !  \  Store the pho(neme) at time t
 \      0  t @  1 aud{ !  \  Store no act(ivation) level.
    pov @  t @  2 aud{ !  \  point-of-view: internal #, external *
    beg @  t @  3 aud{ !  \  beg(inning)?  1 Yes or 0 No.
    ctu @  t @  4 aud{ !  \  continuation? 1=Y or 0 = No.
    ctu @ 0 = IF  \ 27jul2005 Store no false recognitions.
      psi @  t @  5 aud{ !  \  ultimate psi tag # to a concept.
      \ 0 psi !  \ 26jul2002 Safety precaution reset.
    THEN  \ 27jul2005 end of attempt to avoid false recognitions.
    pho @ 32 = IF t @ spt !  THEN  \ Update "space" time.
;  \ End of audSTM; return to AUDITION.

\ LISTEN is attentive for the entry of a single keystroke.
:  LISTEN ( preparation for Audition )  \  1feb2008
  rsvp @  1  DO  \ Value may be tailored to the CPU speed.
    KEY? IF      \ If a character from a key is waiting,
      KEY pho !  \ get a pho(neme) keystroke from the user.
      \ 25jul2002 Eliminate "quiet", set "pov" instead?
      0 quiet !  \ Zero means that user input is in progress.
      pho @  8 = IF 7 EMIT THEN \ 29apr2005 Backspace?
      pho @  9 = IF  \ 30apr2005 if user presses Tab.

        1000 rsvp !  \ 22jan2008 Slow down for human input.

        pho @ 13 = IF  1 lastword !  THEN  \ 3apr2007

        fyi @ 0 = IF CR CR \ 25may2006 Show header only once.
     TIME&DATE tsyear ! tsmonth ! tsday ! tshour ! tsminute ! tssecond !
  ." Transcript of AI Mind interview at "
  tshour @ . tsminute @ . tssecond @ . ." o'clock on " tsday @ .
    tsmonth @  1 = IF ." January "   THEN
    tsmonth @  2 = IF ." February "  THEN
    tsmonth @  3 = IF ." March "     THEN
    tsmonth @  4 = IF ." April "     THEN
    tsmonth @  5 = IF ." May "       THEN
    tsmonth @  6 = IF ." June "      THEN
    tsmonth @  7 = IF ." July "      THEN
    tsmonth @  8 = IF ." August "    THEN
    tsmonth @  9 = IF ." September " THEN
    tsmonth @ 10 = IF ." October "   THEN
    tsmonth @ 11 = IF ." November "  THEN
    tsmonth @ 12 = IF ." December "  THEN
    tsyear @ . 8 EMIT 46 EMIT CR  \ Backspace + period & carriage-return.
\ ." Concept-activation numbers are for use by AI therapist or mind-tender."
\         CR
        THEN

        1 fyi +!  \ Increment display-mode flag by one.
        fyi @ 3 > IF 0 fyi ! THEN \ Limit display-mode flag 0 to 3.
        fyi @ 0 = IF CR
 ." Normal display mode. Tab 1 = Transcript; 2 = Tutorial; 3 = Diagnostic."
          CR
        THEN
        fyi @ 2 = IF CR  \ 13oct2005 Leaving room for Transcript display.
 ."   Tutorial mode reveals the internal thinking of the AI Mind." CR CR
        THEN
        fyi @ 3 = IF CR  \ 13oct2005 Leaving room for Transcript display.
 ." Diagnostic messages - ignore during input until you press ENTER."
        CR
        THEN
        0  pho !  \ Replace Tab value with zero to prevent instantiation.
      THEN         \ End of test for pressing of Tab key.
      pho @ 27 = IF  \ If ESCape key "27" is pressed...
        0 nounval !               \ 29jan2008
        0 lopsi !   0 hipsi !
        CR
        CR ." User Command:  halt" 0 pho ! 0 rjc ! 0 fyi !
        CR ." You may enter .psi .en .aud to view memory engrams, or"
        CR ." ALIFE [ENTER] to erase all memories and restart the Mind."
        CR ." Type 'bye' to quit Forth, then EXIT to quit DOS."
        CR
        QUIT
      THEN
      pho @ 0 > IF   \ 22jul2005 After zeroing HT-9, prevent display.
        pho @ EMIT   \ Display the character on-screen.
      THEN           \ End of test to disregard Tab-key entries.
      \  Next lines convert all ASCII input to upper case:
      pho @ DUP 96 > IF
        DUP 123 < IF
          32 -
        THEN
      THEN  pho !    \ Save the uppercase value as "pho" again.
      LEAVE  \ Abandon loop upon keypress
      ELSE
      ."  "  \ Show EITHER a keystroke OR a blank.
    THEN
    8  EMIT  \  This backspace is paired with the ELSE line.
  LOOP
;  \  End of LISTEN; return to AUDITION.


\ AUDITION handles the input of ASCII as phonemes.
:  AUDITION ( accept auditory input )  \  1feb2008
  0 match !  \ Precaution for sake of SPEECH.
  0 upnext !  \ 29jan2008 For psi-damping previous crest-word.
  0 urpsi !   \ 13jan2008 Lest restarting AI retain false "urpsi".


\ fyi @ 1 > IF   \ 13jan2008
  \ CR ."   Audition: lopsi & hipsi = " lopsi @ . hipsi @ . \ 14jan2008
\ THEN           \ 13jan2008

\ 1 ordo +!  \ 2apr2007 Increment "ordo" for each word.
  t @ nlt !

  pov @  42 = IF   \ If user is entering input,
    fyi @ 2 = IF  \ 30nov2007 In Tutorial mode only...
 ."     Audition calls Listen (Tab key will slow the AI down)." CR
    THEN
     t @ spt !   \ set "space" time before start of input;
     t @ 8 >  IF  .echo  THEN  ( show output of AI )
     CR ." Human: "
  THEN  \ 26jul2002 Testing not for "quiet" but for "pov".

 \ CR ." Aud42: pho pov psi lopsi kb hipsi = "       \ 21jan2008
 \ pho @ .  pov @ . psi @ . lopsi @ . kbpsi @ . hipsi @ . \ 21jan2008

  \ The following loop accepts user entry or AI re-entry:
  60  0  DO    \  Accept entry of up to 60 characters.

    pov @ 35 = IF       \ 13jan2008 During internal re-entry...
      1 upnext +!       \ 29jan2008 Increment upnext per character.
      upnext @ 1 = IF   \ 29jan2008 A once-per-cycle event.
        obstat @ 0 = IF \ 15jan2008 If nothing stands in the way...

        \ CR ." Aud: pov psi lopsi hipsi = "
        \ pov @ . psi @ . lopsi @ . hipsi @ . \ 20j8

        \ CR ."   Aud-pov=" pov @ . ." psi kbpsi lopsi hipsi " \ 21jan2008
        \ psi @ . kbpsi @ . lopsi @ . hipsi @ .                \ 21jan2008

        \ hipsi @ lopsi !  \ 20jan2008 In case lopsi was blank.

          kbpsi @ lopsi !  \ 20jan2008 As soon as engrams start reentering.
          0 kbpsi !        \ 20jan2008 Reset kbpsi.

          lopsi @ urpsi !  \ 14jan2008
        \ 104 caller !  \ 13jan2008 Audition identified by AI4U page.
        \  35 caller !  \ 15jan2008 Temporary test of too many calls.
         3535 caller !  \ 20jan2008 Troublshooting MWA.

\ CR ." Aud: pho pov psi lopsi kb hipsi = "       \ 20jan2008
\  pho @ .  pov @ . psi @ . lopsi @ . kbpsi @ . hipsi @ . \ 20jan2008

          pho @ 64 > IF  \ 20jan2008 If character is alphabetical...
            psiDamp      \ 13jan2008 Knock down cresting concept.
          THEN           \ 20jan2008 End of ASCII test for A, B, C...

       \  hipsi @ lopsi !  \ 14jan2008 "Changing of the guard."
       \  hipsi @ lopsi !  \ 18jan2008 Commenting out as test.
          hipsi @ lopsi !  \ 20jan2008 Commenting out as test.

          0 caller !    \ 13jan2008 Reset "caller" for safety.
          0 urpsi !     \ 13jan2008 Reset urpsi
          1 obstat !    \ 15jan2008 No more psiDamp until end of SPEECH
        THEN            \ End of test for quasi-nihil-obstat condition.
      THEN              \ 29jan2008 End of upnext-trigger test.

\ CR ." 35-AUD: psi & topic = " psi @ . topic @ .  \ 27jan2008 A test.
    \ topic @ redux !  \ 27jan2008 Prepare to revive oldest concept.

      EEG @ 0 = IF        \ 27jan2008 If EEG has flatlines....
        psi @ 0 > IF      \  2sep2008 Catch oldest concept sent by EGO.
           psi @ redux !  \ 27jan2008 Prepare to revive "redux".

           fyi @ 2 = IF   \  2sep2008 If Tutorial mode is on...
             7 EMIT  \ 2sep2008 Make a noise to focus attention.
             CR ."   Resurecting oldest concept #" redux @ . \ 2sep2008
             CR  \ 2sep2008
           THEN  \ 2sep2008 End of test for Tutorial mode.

        THEN              \  2sep2008 EGO sent first post-vault psi.
      THEN                \ 27jan2008 End of "EEG" test.

    THEN                \ 13jan2008 End of POV-test.

    \ 26jul2002 Because SENSORIUM and reentrant SPEECH both
    \ use AUDITION, we must sequester LISTEN from SPEECH
    \ so as not to slow down the process of reentry.
    pov @ 42 = IF   \ 35=internal; 42=external.
      LISTEN     \  Check for user input.
      pho @ 0 > IF   \ 30aug2005 Upon any human user input,

        \ 3sep2008 The KB-traversal trigger "kbtv" starts out with
        \ a value of one so that the incipient AI will say something.
        \ Any user input at all between Rejuvenate sweeps will
        \ prevent kbtv from triggering KB-traversal. If there is a
        \ prolonged dearth of user input, kbtv will increment and
        \ reactivate a different enBoot concept after Rejuvenate.
        0 kbtv !  \  3sep2008 User input obviates need for KBT.

        1 upnext +!  \ 29jan2008 Increment upnext per character.
        upnext @ 1 = IF \ 29jan2008 A once-per-cycle event.

        \ psiDecay   \ 22jan2008 Reduce background activation.
          verbClip   \ 24jan2008 Help the "detour" mechanism.

        \ lopsi @ urpsi !  \ 14jan2008 Using lopsi & hipsi.
          hipsi @ urpsi !  \ 16jan2008 Before hipsi becomes lopsi.
        \ lopsi @ urpsi !  \ 20jan2008

        \ fyi @ 1 > IF   \ 13jan2008 test
        \   CR ."   Ext. Aud: lopsi & hipsi = " lopsi @ . hipsi @ .
        \   CR ."   Ext. Aud: calling psiDamp w urpsi " urpsi @ .
        \ THEN           \ 13jan2008 End of fyi-test.

          104 caller !  \ 13jan2008 Audition identified by AI4U page.
       \ 4242 caller !  \ 20jan2008 Temporary test.
          psiDamp  \ 10jan2008 Knock down cresting concept.
          hipsi @ lopsi !  \ 14jan2008 Also above for internal thought.
          0 caller !    \ 13jan2008 Reset "caller" for safety.
          0 urpsi !     \ 10jan2008 Reset urpsi
        THEN            \ 10 jan2008 End of flag-test.

        1000 rsvp !  \ insrease the wait-time for humans.
      THEN           \ End of test for any input by humans.
      I 59 = IF  \ 30aug2005 During last calls to Listen...
        \ CR ." Call #" I . ."  being made to LISTEN. "
        pho @ 0 = IF  \ If no human input character is present...
          \ ."     No human input has been entered; reducing wait time.
          100 rsvp !   \ 30aug2005 Leave enough time for a keypress.
        ELSE           \ If a human is trying to enter input...
          1000 rsvp !  \ Restore the longer waiting time.
        THEN  \ End of check for absence of input (pho = zero)
      THEN       \ End of check for last call to Listen.
    THEN  \ End of check for expecting external human input.
    \ A CR-13 should come from SVO or LISTEN:
    \ Here we may need to insert a carriage-return CR
    \ if no external user enters any input, or if
    \ an entity fails to complete an entry of input.
    pho @  0 > IF  \ prevents gaps in recording of input;
      1 t +!       \ increment t only if a char is stored
    THEN
    pho @ 13 = IF  \ If a carriage-return "13" comes in,
       \ Eliminate "quiet", set "pov" instead?
       1 quiet !   \ set the "quiet" flag to "1" (true);
      35 pov !     \ 25jul2002 Set "pov" to "internal".
   \   1 lastword ! \ 2apr2007 To reset "seq" tag.
       1 beg !     \ set the beginning-flag to one;
      13 eot !     \ set the end-of-text flag to 13;
      32 pho !     \ convert the carrier "pho" to a SPACE.
      CR           \ To show a carriage-return 13.
    \  0 ordo !    \ 2apr2007 Reset the word-order counter.

    \  CR ."   Audition: psi at CR = " psi @ .      \  4jan2008 test
    \  CR ."   Audition: nen at CR = " nen @ .      \  4jan2008 test
    \  CR ."   Audition: topic at CR = " topic @ .  \  4jan2008 test

       1 lastword ! \ 3apr2007 To reset "seq" tag.

     \ psi @ urpsi !  \  4jan2008 For sake of psiDamp.
     \ psiDamp        \  4jan2008 To leave "residuum" on input word.
    THEN

    pho @ 27 =  IF  \ If ESCape key "27" is pressed...
      CR ." AUDITION: halt"  0 pho !  0 fyi !  0 nounval !
      CR ." You may enter .psi .en .aud to view memory engrams, or"
      CR ." ALIFE [ENTER] to erase all memories and run the AI again."
      0 lopsi !  0 hipsi ! \ 15jan2008 In case user restarts AI.
      QUIT  \ Stop the program.
    THEN

    pho @ 32 = IF  \ Upon SPACE retroactively adjust end of word.
   \  0 lastword !  \ 3apr2007 Space-bar means "lastword" is false.
      1 ordo +!     \ 3apr2007 indrement "ordo" word-counter.

      psi @ urpsi !  \ 10jan2008 Prep to psi-damp previous crest.
      0 upnext !  \ 24jan2008 Reset "upnext" after each input-word.

      t @  spt !       \ Update space time to the current time.
      t @  1 - tult !  \ The last previous time is "t-ultimate".
      0  tult @  4 aud{ !  \ Store a zero in the continuation-slot.
      psi @  0 >  IF    \ If audRecog & audSTM provide positive psi,
        onset @ aud !   \ use the onset-time as the recall-vector
        0 onset !       \ and blank out the onset-time.

     \  CR ." Audition debugging; psi = " psi @ . \ 4dec2007

        psi @  tult @  5 aud{ !  \  Store the psi-tag "psi".
        psi @ hipsi !     \ For re-activation by acme-trigger.
\ CR ."   After-io pre-assig Aud: psi lopsi hipsi " psi @ . lopsi @ . hipsi @ .

        OLDCONCEPT      \ Create a new node of an old concept;

\ CR ."   After-io post-oldC Aud: psi lopsi hipsi " psi @ . lopsi @ . hipsi @ .
\       fyi 1 > IF      \ 14jan2008
\ CR ."    Aud. post-oldC: psi lopsi hipsi " psi @ . lopsi @ . hipsi @ .
\       THEN            \ 14jan2008 End of fyi-test

        0 psi !         \ Zero out the psi-tag for safety;
        0 aud !         \ Zero out the recall-vector for safety;
      ELSE            \ If there is no psi-tag "psi";
          len @ 0 > IF  \ if the incoming word has a positive length,
            onset @ aud ! \ store the onset from AUDITION as "aud" tag;
            hipsi @ lopsi !  \ 14jan2008 Prepare for a new "hipsi".
            NEWCONCEPT    \ to create a new node of a new concept;
            psi @ hipsi !    \ 14jan2008 New concept is new "hipsi".
       \  CR ." Audition newConcept nen = " nen @ . \ 4dec2007

\       fyi 1 > IF      \ 14jan2008
\ CR ."    Aud. post-newC: psi lopsi hipsi " psi @ . lopsi @ . hipsi @ .
\       THEN            \ 14jan2008 End of fyi-test


\       fyi 0 > IF      \ 21jan2008
\ CR ."    Aud. post-newC: psi lopsi hipsi " psi @ . lopsi @ . hipsi @ .
\       THEN            \ 21jan2008 End of fyi-test

            nen @  tult @  5  aud{ !  \ Store new concept psi-tag.
          THEN          \ end of test for a positive-length word;
      THEN              \ end of test for the presence of a move-tag;
      audDamp           \ Zero out the auditory engrams.
      0 len !           \ Zero out the length variable.
      0 aud !           \ Zero out the auditory "aud" recall-tag.
      eot @ 13 = IF     \ CR-13 resets a primitive parsing flag;
        5 bias !        \ prepare to parse the next word as a noun;
      THEN              \ end of test for carriage-return "13" CR.
      0 psi !           \ for both old and new concepts
    THEN  \ end of retroactive import of "psi" from audSTM
    1 beg !  \ Set the "beginning" flag to "1" for true.
    1 ctu !  \ Provisionally set "continuation" flag to true.
    spt @ 1 + onset !  \ Onset comes next after a "space" time.
    \ The following code has a speed-up alternative below it:
      t @  onset @  = IF  1 beg !  ELSE  0 beg !  THEN
    \ t @  onset @  =       beg !  ( JAF suggestion )
    \ 27jul2002 Reverting to 32 from 31 as a test:
    pho @ 32 > IF     \ If 32-space or alphabetic character...
      1 len +!
      audSTM  \ Store character in Short Term Memory.
    THEN
    eot @ 13 = IF  \ If end-of-text is a carriage-return "13"
      5 bias !     \ 26jul2002 To help the Parser module.
      \ 25jul2002 Eliminate "quiet", set "pov" instead?
      1 quiet !    \ set "quiet" flag to "1" (true) status;
    THEN
    eot @  0 > IF  \ If CR-13 has raised eot to 13,
      eot @ 14 = IF  \ 26jul2002 After one increment
        1 quiet !    \ 25jul2002 Just in case it is needed.
        0 eot !      \ 26jul2002 Reset for safety.
        0 pho !      \ 25jul2002 Reset for safety.
        LEAVE   \ 25jul2002 Return to the calling module.
      THEN  \ 25jul2002 End of post-final-iteration test.
      14 eot !  \ 25jul2002 Make final iteration, then leave.
    THEN  \  25jul2002 End of test.
    0 pho !  \ Prevent "pho" from reduplicating.
  LOOP       \ End of checking for human input or AI reentry.

  hipsi @ kbpsi !  \ 20jan2008 Prepare to switch out hipsi upon rentry.


\ CR ." AUD: psi & topic = " psi @ . topic @ .   \ 27jan2008 A test.
\ CR ." AUD: psi topic redux " psi @ . topic @ . redux @ . \ 27jan2008 Test.
\ CR ." AUD: EEG = " EEG @ . ." psi = " psi @ .  \ 27jan2008 Test
\ ." topic = " topic @ . ." redux = " redux @ .  \ 27jan2008 Test

;  \  End of AUDITION; return to SENSORIUM or to SPEECH.


\ SENSORIUM handles the input of sensory perception.
:  SENSORIUM ( sensory input channels )  \ 7jan2008
 \ psiDecay   \ 8oct2005 Favor user input over unresolved activations.
 \ psiDecay  \ 13jan2008 Commenting out the above line to aid "detours".
 ( SMELL  -- normal sensory stub for later implementation )
 ( VISION -- normal sensory stub for seed AI expansion )
 ( TOUCH  -- normal haptics stub for cybernetic organisms )
 ( TASTE  -- normal sensory stub for cyborg alife )
 ( SYNAESTHESIA -- an option in a multisensory AI )
     fyi @ 2 = IF  \ 30nov 2007 In Tutorial mode only...
 ."   Sensorium calls Audition." CR
     THEN
   AUDITION  ( for entry or reentry of phonemic ASCII )
 ( COMPASS  -- exotic sensory stub for use in robots )
 ( GEIGER   -- exotic: Geiger counter )
 ( GPS      -- exotic: Global Positioning System )
 ( INFRARED -- exotic )
 ( RADAR    -- exotic: RAdio Detection And Ranging )
 ( SONAR    -- exotic: SOund Navigation And Ranging )
 ( VSA      -- exotic: Voice Stress Analyzer lie detector )
 ( Wi-Fi    -- exotic: 802.11 wireless fidelity )
;  \  Return to ALIFE or to the reentry process.


\ enBoot (the English language bootstrap) holds the bootstrap
\ sequences for thinking in English.  Instead of enBoot there
\ could be deBoot for thinking in German (deutsch), or jaBoot
\ for thinking in Japanese, or several such bootstraps together.
\ POS: 1=adj 2=adv 3=conj 4=interj 5=noun 6=prep 7=pron 8=verb
:  enBoot ( English Bootstrap of initial concepts ) \  1feb2008
  0 t !  t @  spt !  ." (clearing memory...)"
  CR ." There is no warranty for what this software does."

    1 t ! 89 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ Y
    2 t ! 69 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ E
    3 t ! 83 pho ! 35 pov !  0 beg ! 0 ctu ! 32 psi ! audSTM \ S
 32 psi !  0 act !  0 jux !  0 pre ! 4 pos !  0 seq ! 32 enx ! INSTANTIATE
 32 nen !  0 act ! 32 fex !          4 pos ! 32 fin !  1 aud ! enVocab

    5 t ! 73 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ I
    6 t ! 70 pho ! 35 pov !  0 beg ! 0 ctu ! 20 psi ! audSTM \ F
 20 psi !  0 act !  0 jux !  0 pre ! 3 pos !  0 seq ! 20 enx ! INSTANTIATE
 20 nen !  0 act ! 20 fex !          3 pos ! 20 fin !  5 aud ! enVocab

    8 t ! 84 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ T
    9 t ! 72 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ H
   10 t ! 69 pho ! 35 pov !  0 beg ! 0 ctu !  7 psi ! audSTM \ E
  7 psi !  0 act !  0 jux !  0 pre ! 1 pos !  0 seq !  7 enx ! INSTANTIATE
  7 nen !  0 act !  7 fex !          1 pos !  7 fin !  8 aud ! enVocab

   12 t ! 84 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ T
   13 t ! 82 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ R
   14 t ! 85 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ U
   15 t ! 84 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ T
   16 t ! 72 pho ! 35 pov !  0 beg ! 0 ctu ! 68 psi ! audSTM \ H
 68 psi !  1 num !  0 jux !  0 pre ! 5 pos ! 66 seq ! 68 enx ! INSTANTIATE
 68 nen !  0 act ! 68 fex !          5 pos ! 68 fin ! 12 aud ! enVocab

   18 t ! 73 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ I
   19 t ! 83 pho ! 35 pov !  0 beg ! 0 ctu ! 66 psi ! audSTM \ S
 66 psi !  8 act !  0 jux ! 68 pre ! 8 pos !  0 seq ! 66 enx ! INSTANTIATE
 66 nen !  0 act ! 66 fex !          8 pos ! 66 fin ! 18 aud ! enVocab

   21 t ! 84 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ T
   22 t ! 72 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ H
   23 t ! 65 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ A
   24 t ! 84 pho ! 35 pov !  0 beg ! 0 ctu ! 22 psi ! audSTM \ T
 22 psi !  0 act !  0 jux !  0 pre ! 3 pos !  0 seq ! 22 enx ! INSTANTIATE
 22 nen !  0 act ! 22 fex !          3 pos ! 22 fin ! 21 aud ! enVocab

   26 t ! 65 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ A
   27 t ! 76 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ L
   28 t ! 76 pho ! 35 pov !  0 beg ! 0 ctu !  2 psi ! audSTM \ L
  2 psi !  0 act !  0 jux !  0 pre ! 1 pos !  0 seq !  2 enx ! INSTANTIATE
  2 nen !  0 act !  2 fex !          1 pos !  2 fin ! 26 aud ! enVocab

   30 t ! 82 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ R
   31 t ! 79 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ O
   32 t ! 66 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ B
   33 t ! 79 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ O
   34 t ! 84 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ T
   35 t ! 83 pho ! 35 pov !  0 beg ! 0 ctu ! 39 psi ! audSTM \ S
 39 psi !  2 num !  0 jux !  0 pre ! 5 pos !  0 seq ! 39 enx ! INSTANTIATE
 39 nen !  0 act ! 39 fex !          5 pos ! 39 fin ! 30 aud ! enVocab

   37 t ! 65 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ A
   38 t ! 82 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ R
   39 t ! 69 pho ! 35 pov !  0 beg ! 0 ctu ! 67 psi ! audSTM \ E
 67 psi !  8 act !  0 jux ! 39 pre ! 8 pos ! 38 seq ! 67 enx ! INSTANTIATE
 67 nen !  0 act ! 67 fex !          8 pos ! 67 fin ! 37 aud ! enVocab

   41 t ! 80 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ P
   42 t ! 69 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ E
   43 t ! 82 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ R
   44 t ! 83 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ S
   45 t ! 79 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ O
   46 t ! 78 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ N
   47 t ! 83 pho ! 35 pov !  0 beg ! 0 ctu ! 38 psi ! audSTM \ S
 38 psi !  2 num !  0 jux !  0 pre ! 5 pos !  0 seq ! 38 enx ! INSTANTIATE
 38 nen !  0 act ! 38 fex !          5 pos ! 38 fin ! 41 aud ! enVocab

   49 t ! 66 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ B
   50 t ! 69 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ E
   51 t ! 67 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ C
   52 t ! 65 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ A
   53 t ! 85 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ U
   54 t ! 83 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ S
   55 t ! 69 pho ! 35 pov !  0 beg ! 0 ctu ! 18 psi ! audSTM \ E
 18 psi !  0 act !  0 jux !  0 pre ! 3 pos !  0 seq ! 18 enx ! INSTANTIATE
 18 nen !  0 act ! 18 fex !          3 pos ! 18 fin ! 49 aud ! enVocab

   57 t ! 87 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ W
   58 t ! 69 pho ! 35 pov !  0 beg ! 0 ctu ! 53 psi ! audSTM \ E
 53 psi !  0 act !  0 jux !  0 pre ! 5 pos !  0 seq ! 53 enx ! INSTANTIATE
 53 nen !  0 act ! 53 fex !          5 pos ! 56 fin ! 57 aud ! enVocab

   60 t ! 84 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ T
   61 t ! 72 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ H
   62 t ! 73 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ I
   63 t ! 78 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ N
   64 t ! 75 pho ! 35 pov !  0 beg ! 0 ctu ! 63 psi ! audSTM \ K
 63 psi !  0 act !  0 jux !  0 pre ! 8 pos !  0 seq ! 63 enx ! INSTANTIATE
 63 nen !  0 act ! 63 fex !          8 pos ! 63 fin ! 60 aud ! enVocab

   66 t ! 84 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ T
   67 t ! 72 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ H
   68 t ! 69 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ E
   69 t ! 78 pho ! 35 pov !  0 beg ! 0 ctu ! 13 psi ! audSTM \ N
 13 psi !  0 act !  0 jux !  0 pre ! 2 pos !  0 seq ! 13 enx ! INSTANTIATE
 13 nen !  0 act ! 13 fex !          2 pos ! 13 fin ! 66 aud ! enVocab

   71 t ! 78 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ N
   72 t ! 79 pho ! 35 pov !  0 beg ! 0 ctu ! 27 psi ! audSTM \ O
 27 psi !  0 act !  0 jux !  0 pre ! 4 pos !  0 seq ! 27 enx ! INSTANTIATE
 27 nen !  0 act ! 27 fex !          4 pos ! 27 fin ! 71 aud ! enVocab

   74 t ! 89 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ Y
   75 t ! 79 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ O
   76 t ! 85 pho ! 35 pov !  0 beg ! 0 ctu ! 56 psi ! audSTM \ U
 56 psi !  0 act !  0 jux !  0 pre ! 5 pos !  0 seq ! 56 enx ! INSTANTIATE
 56 nen !  0 act ! 56 fex !          5 pos ! 50 fin ! 74 aud ! enVocab

   78 t ! 65 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ A
   79 t ! 78 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ N
   80 t ! 68 pho ! 35 pov !  0 beg ! 0 ctu ! 17 psi ! audSTM \ D
 17 psi !  0 act !  0 jux !  0 pre ! 3 pos !  0 seq ! 17 enx ! INSTANTIATE
 17 nen !  0 act ! 17 fex !          3 pos ! 17 fin ! 78 aud ! enVocab

   82 t ! 73 pho ! 35 pov !  1 beg ! 0 ctu ! 50 psi ! audSTM \ I
 50 psi !  0 act !  0 jux !  0 pre ! 5 pos ! 61 seq ! 50 enx ! INSTANTIATE
 50 nen !  0 act ! 50 fex !          5 pos ! 56 fin ! 82 aud ! enVocab

   84 t ! 68 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ D
   85 t ! 79 pho ! 35 pov !  0 beg ! 0 ctu ! 59 psi ! audSTM \ O
 59 psi !  0 act !  0 jux !  0 pre ! 8 pos !  0 seq ! 59 enx ! INSTANTIATE
 59 nen !  0 act ! 59 fex !          8 pos ! 59 fin ! 84 aud ! enVocab

   87 t ! 78 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ N
   88 t ! 79 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ O
   89 t ! 84 pho ! 35 pov !  0 beg ! 0 ctu ! 12 psi ! audSTM \ T
 12 psi !  0 act !  0 jux !  0 pre ! 2 pos !  0 seq ! 12 enx ! INSTANTIATE
 12 nen !  0 act ! 12 fex !          2 pos ! 12 fin ! 87 aud ! enVocab

   91 t ! 75 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ K
   92 t ! 78 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ N
   93 t ! 79 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ O
   94 t ! 87 pho ! 35 pov !  0 beg ! 0 ctu ! 61 psi ! audSTM \ W
 61 psi !  0 act !  0 jux ! 50 pre ! 8 pos ! 16 seq ! 61 enx ! INSTANTIATE
 61 nen !  0 act ! 61 fex !          8 pos ! 61 fin ! 91 aud ! enVocab

   96 t ! 87 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ W
   97 t ! 72 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ H
   98 t ! 89 pho ! 35 pov !  0 beg ! 0 ctu ! 16 psi ! audSTM \ Y
 16 psi !  0 act !  0 jux !  0 pre ! 2 pos !  0 seq ! 16 enx ! INSTANTIATE
 16 nen !  0 act ! 16 fex !          2 pos ! 16 fin ! 96 aud ! enVocab

  100 t ! 83 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ S
  101 t ! 79 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ O
  102 t ! 77 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ M
  103 t ! 69 pho ! 35 pov !  0 beg ! 0 ctu ! 69 psi ! audSTM \ E
 69 psi !  0 act !  0 jux !  0 pre ! 1 pos !  0 seq ! 69 enx ! INSTANTIATE
 69 nen !  0 act ! 69 fex !          1 pos ! 69 fin ! 100 aud ! enVocab

  105 t ! 80 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ P
  106 t ! 69 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ E
  107 t ! 79 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ O
  108 t ! 80 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ P
  109 t ! 76 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ L
  110 t ! 69 pho ! 35 pov !  0 beg ! 0 ctu ! 37 psi ! audSTM \ E
 37 psi !  2 num !  0 jux !  0 pre ! 5 pos ! 70 seq ! 37 enx ! INSTANTIATE
 37 nen !  0 act ! 37 fex !          5 pos ! 37 fin ! 105 aud ! enVocab

  112 t ! 72 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ H
  113 t ! 65 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ A
  114 t ! 86 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ V
  115 t ! 69 pho ! 35 pov !  0 beg ! 0 ctu ! 70 psi ! audSTM \ E
 70 psi !  0 act !  0 jux ! 37 pre ! 8 pos ! 71 seq ! 70 enx ! INSTANTIATE
 70 nen !  0 act ! 70 fex !          8 pos ! 70 fin ! 112 aud ! enVocab

  117 t ! 65 pho ! 35 pov !  1 beg ! 0 ctu !  1 psi ! audSTM \ A
  1 psi !  0 act !  0 jux !  0 pre ! 1 pos !  0 seq !  1 enx ! INSTANTIATE
  1 nen !  0 act !  1 fex !          1 pos !  1 fin ! 117 aud ! enVocab

  119 t ! 70 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ F
  120 t ! 69 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ E
  121 t ! 65 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ A
  122 t ! 82 pho ! 35 pov !  0 beg ! 0 ctu ! 71 psi ! audSTM \ R
 71 psi !  1 num !  0 jux !  0 pre ! 5 pos !  0 seq ! 71 enx ! INSTANTIATE
 71 nen !  0 act ! 71 fex !          5 pos ! 71 fin ! 119 aud ! enVocab

  124 t ! 79 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ O
  125 t ! 70 pho ! 35 pov !  0 beg ! 0 ctu ! 45 psi ! audSTM \ F
 45 psi !  0 act !  0 jux !  0 pre ! 6 pos !  0 seq ! 45 enx ! INSTANTIATE
 45 nen !  0 act ! 45 fex !          6 pos ! 45 fin ! 124 aud ! enVocab

  127 t ! 87 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ W
  128 t ! 72 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ H
  129 t ! 79 pho ! 35 pov !  0 beg ! 0 ctu ! 55 psi ! audSTM \ O
 55 psi !  0 act !  0 jux !  0 pre ! 5 pos !  0 seq ! 55 enx ! INSTANTIATE
 55 nen !  0 act ! 55 fex !          5 pos ! 55 fin ! 127 aud ! enVocab

  131 t ! 73 pho ! 35 pov !  1 beg ! 0 ctu ! 50 psi ! audSTM \ I
 50 psi !  0 act !  0 jux !  0 pre ! 5 pos ! 57 seq ! 50 enx ! INSTANTIATE
 50 nen !  0 act ! 50 fex !          5 pos ! 56 fin ! 131 aud ! enVocab

  133 t ! 65 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ A
  134 t ! 77 pho ! 35 pov !  0 beg ! 0 ctu ! 57 psi ! audSTM \ M
 57 psi !  8 act !  0 jux !  0 pre ! 8 pos !  0 seq ! 57 enx ! INSTANTIATE
 57 nen !  0 act ! 57 fex !          8 pos ! 57 fin ! 133 aud ! enVocab

  136 t ! 79 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ O
  137 t ! 82 pho ! 35 pov !  0 beg ! 0 ctu ! 21 psi ! audSTM \ R
 21 psi !  0 act !  0 jux !  0 pre ! 3 pos !  0 seq ! 21 enx ! INSTANTIATE
 21 nen !  0 act ! 21 fex !          3 pos ! 21 fin ! 136 aud ! enVocab

  139 t ! 87 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ W
  140 t ! 72 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ H
  141 t ! 65 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ A
  142 t ! 84 pho ! 35 pov !  0 beg ! 0 ctu ! 54 psi ! audSTM \ T
 54 psi !  0 act !  0 jux !  0 pre ! 5 pos !  0 seq ! 54 enx ! INSTANTIATE
 54 nen !  0 act ! 54 fex !          5 pos ! 54 fin ! 139 aud ! enVocab

  144 t ! 84 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ T
  145 t ! 72 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ H
  146 t ! 69 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ E
  147 t ! 89 pho ! 35 pov !  0 beg ! 0 ctu ! 52 psi ! audSTM \ Y
 52 psi !  0 act !  0 jux !  0 pre ! 5 pos ! 62 seq ! 52 enx ! INSTANTIATE
 52 nen !  0 act ! 52 fex !          5 pos ! 52 fin ! 144 aud ! enVocab

  149 t ! 83 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ S
  150 t ! 69 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ E
  151 t ! 69 pho ! 35 pov !  0 beg ! 0 ctu ! 62 psi ! audSTM \ E
 62 psi !  0 act !  0 jux ! 52 pre ! 8 pos !  0 seq ! 62 enx ! INSTANTIATE
 62 nen !  0 act ! 62 fex !          8 pos ! 62 fin ! 149 aud ! enVocab

  153 t ! 73 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ I
  154 t ! 78 pho ! 35 pov !  0 beg ! 0 ctu ! 44 psi ! audSTM \ N
 44 psi !  0 act !  0 jux !  0 pre ! 6 pos !  0 seq ! 44 enx ! INSTANTIATE
 44 nen !  0 act ! 44 fex !          6 pos ! 44 fin ! 153 aud ! enVocab

  156 t ! 77 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ M
  157 t ! 69 pho ! 35 pov !  0 beg ! 0 ctu ! 65 psi ! audSTM \ E
 50 psi !  0 act !  0 jux !  0 pre ! 5 pos !  0 seq ! 65 enx ! INSTANTIATE
 65 nen !  0 act ! 50 fex !          5 pos ! 56 fin ! 156 aud ! enVocab

 \  KIDS MAKE ROBOTS
 \                ROBOTS NEED ME
 \                             I HELP KIDS

  159 t ! 75 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ K
  160 t ! 73 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ I
  161 t ! 68 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ D
  162 t ! 83 pho ! 35 pov !  0 beg ! 0 ctu ! 72 psi ! audSTM \ S
 72 psi !  2 num !  0 jux !  0 pre ! 5 pos ! 73 seq ! 72 enx ! INSTANTIATE
 72 nen !  0 act ! 72 fex !          5 pos ! 72 fin ! 159 aud ! enVocab

  164 t ! 77 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ M
  165 t ! 65 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ A
  166 t ! 75 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ K
  167 t ! 69 pho ! 35 pov !  0 beg ! 0 ctu ! 73 psi ! audSTM \ E
 73 psi !  0 act !  0 jux ! 72 pre ! 8 pos ! 39 seq ! 73 enx ! INSTANTIATE
 73 nen !  0 act ! 73 fex !          8 pos ! 73 fin ! 164 aud ! enVocab

  169 t ! 82 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ R
  170 t ! 79 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ O
  171 t ! 66 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ B
  172 t ! 79 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ O
  173 t ! 84 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ T
  174 t ! 83 pho ! 35 pov !  0 beg ! 0 ctu ! 39 psi ! audSTM \ S
 39 psi !  2 num !  0 jux ! 73 pre ! 5 pos !  0 seq ! 39 enx ! INSTANTIATE
 39 nen !  0 act ! 39 fex !          5 pos ! 39 fin ! 169 aud ! enVocab

  176 t ! 82 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ R
  177 t ! 79 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ O
  178 t ! 66 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ B
  179 t ! 79 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ O
  180 t ! 84 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ T
  181 t ! 83 pho ! 35 pov !  0 beg ! 0 ctu ! 39 psi ! audSTM \ S
 39 psi !  2 num !  0 jux !  0 pre ! 5 pos ! 74 seq ! 39 enx ! INSTANTIATE
 39 nen !  0 act ! 39 fex !          5 pos ! 39 fin ! 176 aud ! enVocab

  183 t ! 78 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ N
  184 t ! 69 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ E
  185 t ! 69 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ E
  186 t ! 68 pho ! 35 pov !  0 beg ! 0 ctu ! 74 psi ! audSTM \ D
 74 psi !  0 act !  0 jux ! 39 pre ! 8 pos ! 50 seq ! 74 enx ! INSTANTIATE
 74 nen !  0 act ! 74 fex !          8 pos ! 74 fin ! 183 aud ! enVocab

  188 t ! 77 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ M
  189 t ! 69 pho ! 35 pov !  0 beg ! 0 ctu ! 65 psi ! audSTM \ E
 50 psi !  0 act !  0 jux ! 74 pre ! 5 pos !  0 seq ! 65 enx ! INSTANTIATE
 65 nen !  0 act ! 50 fex !          5 pos ! 56 fin ! 188 aud ! enVocab

  191 t ! 73 pho ! 35 pov !  1 beg ! 0 ctu ! 50 psi ! audSTM \ I
 50 psi !  0 act !  0 jux !  0 pre ! 5 pos ! 75 seq ! 50 enx ! INSTANTIATE
 50 nen !  0 act ! 50 fex !          5 pos ! 56 fin ! 191 aud ! enVocab

  193 t ! 72 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ H
  194 t ! 69 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ E
  195 t ! 76 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ L
  196 t ! 80 pho ! 35 pov !  0 beg ! 0 ctu ! 75 psi ! audSTM \ P
 75 psi !  0 act !  0 jux ! 50 pre ! 8 pos ! 72 seq ! 75 enx ! INSTANTIATE
 75 nen !  0 act ! 75 fex !          8 pos ! 75 fin ! 193 aud ! enVocab


  198 t ! 75 pho ! 35 pov !  1 beg ! 1 ctu !  0 psi ! audSTM \ K
  199 t ! 73 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ I
  200 t ! 68 pho ! 35 pov !  0 beg ! 1 ctu !  0 psi ! audSTM \ D
  201 t ! 83 pho ! 35 pov !  0 beg ! 0 ctu ! 72 psi ! audSTM \ S
 72 psi !  2 num !  0 jux ! 75 pre ! 5 pos !  0 seq ! 72 enx ! INSTANTIATE
 72 nen !  0 act ! 72 fex !          5 pos ! 72 fin ! 198 aud ! enVocab

  1 t +!
    t @  vault !  \ Retain size of enBoot for Rejuvenate.
    t @  tov !    \ 23jan2006: No display of enBoot sequence at start-up.
\   t @  tov !    \  6oct2005 Try to prevent crashes involving "nesting."
  1 t +!          \ For a space before user input.
    t @ nlt !  ( nlt may be basis for DAMP functions )
            \ Assign concepts only above the pre-set first 64.
\ 71 nen !  \ Or higher if any more special words are included.
  \ 3dec2007 Next dozen lines from JavaScript AI may help in debugging.
  \ Any additional words for any bootstrap may be included:
  \ Concept #65 is quasi-noun "me".
  \ Concept #66 is verb "is".
  \ Concept #67 is verb "are".
  \ Concept #68 is noun "truth".
  \ Concept #69 is adjective "some".
  \ Concept #70 is verb "have".
  \ Concept #71 is noun "fear".
  \ Concept #72 is noun "kids". Added on 18sep2006.
  \ Concept #73 is verb "make". Added on 18sep2006.
  \ Concept #74 is verb "need". Added on 18sep2006.
  \ Concept #75 is verb "help". Added on 18sep2006.
  72 urpsi !  \ 10jan2008 As if "KIDS" were the cresting concept.
  75 nen !  \ 2apr2007 After add-on ontology from JavaScript AI.
  5 bias !  \ Expect first to parse a noun=5.
  0 num !  \ 27aug2008 Trying to prevent carry-overs.
  0 pho !  \ Reset to prevent reduplication.
  0 pre !  0 seq !  \  Prevent carry-overs.
;  \ End of enBoot; return to the main ALIFE loop.



\ kbTraversal keeps the AI from becoming too dull.
:  kbTraversal ( reactivate KB concepts )  \  3sep2008

    35 pov !  \  3sep2008 Make sure pov is "internal".

    psiDecay  \  3sep2008 Suppress currently active concepts.
    psiDecay  \  3sep2008 Suppress currently active concepts.
    psiDecay  \  3sep2008 Suppress currently active concepts.

    kbtv @ 4 > IF  1 kbtv !  THEN  \  3sep2008 Cycle through values.

    CR ." KB-traversal: With kbtv at " kbtv @ .   \  3sep2008

    kbtv @ 1 = IF  \  3sep2008
      39 psi !     \  3sep2008 Psi concept #39 for "ROBOTS" in enBoot.
      ." activating concept of ROBOTS" CR
      62 nounval ! \  3sep2008 High enough for slosh-over?
      nounAct      \  3sep2008 Activate the indicated concept.
    THEN  \  3sep2008

    kbtv @ 2 = IF  \  3sep2008
      37 psi !     \  3sep2008 Psi concept #37 for "PEOPLE" in enBoot.
      ." activating concept of PEOPLE" CR
      62 nounval ! \  3sep2008 High enough for slosh-over?
      nounAct      \  3sep2008 Use the concept in a sentence of thought.
    THEN  \  3sep2008

    kbtv @ 3 = IF  \  3sep2008
      56 psi !     \  3sep2008 Psi concept #56 for "YOU" in enBoot.
      ." activating concept of YOU" CR
      62 nounval ! \  3sep2008 High enough for slosh-over?
      nounAct      \  3sep2008 Use the concept in a sentence of thought.
    THEN  \  3sep2008

    kbtv @ 4 = IF  \  3sep2008
      68 psi !     \  3sep2008 Psi concept #68 for "TRUTH" in enBoot.
      ." activating concept of TRUTH" CR
      62 nounval ! \  3sep2008 High enough for slosh-over?
      nounAct      \  3sep2008 Use the concept in a sentence of thought.
    THEN  \  3sep2008

    42 pov !  \  3sep2008 Set pov to "external" to await input.

;  \  3sep2008 End of kbTraversal; return to Rejuvenate.


\  REJUVENATE aims for the following entelechy goals.
\  Enable KB-traversal by saving old memories from oblivion.
\  Erase oldest memories to make room for new memory engrams.
\  REJUVENATE is called by SECURITY when the "cns" is almost full,
\  and makes the seed AI potentially immortal by erasing the
\  oldest memories to free up "cns" space for new memories.
:  REJUVENATE ( recycle oldest memory spaces ) \  2sep2008
  fyi @ 2 = IF  \ 29jan2008 Check the display-mode.
    7 EMIT      \  2sep2008 Make a sound; remove if too noisy.
    CLS         \ 29jan2008 Clear-Screen to show transit of engrams.
  THEN          \ 29jan2008 End of fyi-check.
  0 edge !  \ When found, edge-of-thought becomes "1".
  CR 1 rjc +!  \ Increment the Rejuvenation-counter "rjc".
  ." Please wait as memories migrate in rejuvenation cycle #" rjc @ . CR
  t @ 2 +  coda @ vault @ +  DO  \ Loop after "vault" and "coda"
    \  Create a "junior-time" index offset by "coda":
    I  jrt !  \  First obtain the value of the current "Index".
    jrt @  coda @ -  jrt !  \ Then reduce the value by "coda" units.
    edge @ 1 = IF
      I 0 psi{ @  jrt @  0 psi{ !  0 I 0 psi{ !  \ Move the engram from
      I 1 psi{ @  jrt @  1 psi{ !  0 I 1 psi{ !  \ its senior time-spot
      I 2 psi{ @  jrt @  2 psi{ !  0 I 2 psi{ !  \ down to the "jrt"
      I 3 psi{ @  jrt @  3 psi{ !  0 I 3 psi{ !  \ "junior-time" spot
      I 4 psi{ @  jrt @  4 psi{ !  0 I 4 psi{ !  \ and erase the
      I 5 psi{ @  jrt @  5 psi{ !  0 I 5 psi{ !  \ original engram.
      I 6 psi{ @  jrt @  6 psi{ !  0 I 6 psi{ !
      I 7 psi{ @  jrt @  7 psi{ !  0 I 7 psi{ !  \ 24aug2008 with "num"

    THEN
    edge @  1 =  IF
    \ en5 @  1 <  IF  0 en5 !  THEN  \ Avoid negative "aud" flags.
      en6 @  1 <  IF  0 en6 !  THEN  \ 27aug2008 Avoid negative "aud"
      I 0 en{ @  jrt @  0 en{ !  0 I 0 en{ !  \ After moving, erase.
      I 1 en{ @  jrt @  1 en{ !  0 I 1 en{ !
      I 2 en{ @  jrt @  2 en{ !  0 I 2 en{ !
      I 3 en{ @  jrt @  3 en{ !  0 I 3 en{ !
      I 4 en{ @  jrt @  4 en{ !  0 I 4 en{ !
      I 5 en{ @  jrt @  5 en{ !  0 I 5 en{ ! \ 27aug2008
      I 6 en{ @  en6 !   \ 27aug2008 Fetch "aud" value to store as "en6"

          en6 @  vault @ < IF \ 27aug2008 If "aud" points into bootstrap
          en6 @  jrt @  6 en{ !  0 I 6 en{ !  THEN \ 27aug2008 unchanged
          en6 @  coda @  vault @ +  > IF \ 27aug2008 If "aud" is big enough
          en6 @  coda @ -  jrt @ 6 en{ ! \ 27aug2008 store "aud" after vault.
          THEN               0 I 6 en{ !  \ 27aug2008 Erase original engram.
    THEN
    edge @  1 = IF  \ Although at first "edge" will be "0"
      I 0 aud{ @  jrt @  0 aud{ !   \ shift backwards
      I 1 aud{ @  jrt @  1 aud{ !   \ shift backwards
      I 2 aud{ @  jrt @  2 aud{ !   \ shift backwards
      I 3 aud{ @  jrt @  3 aud{ !   \ shift backwards
      I 4 aud{ @  jrt @  4 aud{ !   \ shift backwards
      I 5 aud{ @  jrt @  5 aud{ !   \ shift backwards
        fyi @ 1 > IF  \ In Tutorial or Diagnostic mode...
                  jrt @  0 aud{ @ EMIT  \ show the transfer
        THEN \ End of test to prevent recap during Normal display.
    THEN
    \ Searching for ASCII "123" bracket "{" inserted by SVO module:
    edge @ 0 = IF  \ As long as edge-of-thought has NOT been reached
     32 jrt @ 0 aud{ !  \ Use a SPACE not a zero.
      0 jrt @ 1 aud{ !
            I 2 aud{ @ 123 = IF  1 edge !  THEN
      0 jrt @ 2 aud{ !
      0 jrt @ 3 aud{ !
      0 jrt @ 4 aud{ !
      0 jrt @ 5 aud{ !
      0 jrt @ 0 en{ !  \ Store blank engrams after the enBoot vault
      0 jrt @ 1 en{ !  \ until the "edge" bracket "{" is found that
      0 jrt @ 2 en{ !  \ indicates the edge of a complete thought
      0 jrt @ 3 en{ !  \ in auditory, semantic & conceptual memory
      0 jrt @ 4 en{ !  \ about to be moved "coda" moments backwards
      0 jrt @ 5 en{ !  \ and be redeposited in "junior time" memory.
      0 jrt @ 6 en{ !  \ 27aug2008 After adding "num" to flag-panel.
      0 jrt @ 0 psi{ ! \ "psi"
      0 jrt @ 1 psi{ ! \ "act"
      0 jrt @ 2 psi{ ! \ "num" 27aug2008
      0 jrt @ 3 psi{ ! \ "jux" 27aug2008
      0 jrt @ 4 psi{ ! \ "pre" 27aug2008
      0 jrt @ 5 psi{ ! \ "pos" 27aug2008
      0 jrt @ 6 psi{ ! \ "seq" 27aug2008
      0 jrt @ 7 psi{ ! \ "enx" 27aug2008
    THEN
  LOOP
  jrt @  t !        \ Final value of "junior time" becomes time "t".
  cns @    t @  DO  \ Blank out all rejuvenated time.
   32  I 0  aud{ !  \ Use a SPACE not a zero.
    0  I 1  aud{ !
    0  I 2  aud{ !
    0  I 3  aud{ !
    0  I 4  aud{ !
    0  I 5  aud{ !
    0  I 0   en{ !  \ Blank out the "rejuvenated" most recent
    0  I 1   en{ !  \ areas of the "enVocab" English vocabulary
    0  I 2   en{ !  \ array, from the downsized time "t" point
    0  I 3   en{ !  \ up to the highest "cns" value of time "t".
    0  I 4   en{ !
    0  I 5   en{ !
    0  I 6   en{ !  \ 27aug2008
    0  I 0  psi{ !  \ Blank out psi from "t" to end of "cns".
    0  I 1  psi{ !  \ "act"
    0  I 2  psi{ !  \ "num" 24aug2008 forces other tags down.
    0  I 3  psi{ !  \ "jux" 22jul2002 forces other tags down.
    0  I 4  psi{ !  \ "pre"
    0  I 5  psi{ !  \ "pos"
    0  I 6  psi{ !  \ "seq"
    0  I 7  psi{ !  \ "enx"
  LOOP  \ End of blank-out of recovered recent memory space.
  t @ 20 - tov !  \ 29apr2005 Prevent truncation in .echo function.
  CR CR ." End of Rejuvenation #" rjc @ . \  2sep2008 Adding a CR
\ ." in Mind.Forth Artificial Intelligence for schools and labs" CR
  ." in the first real artificial intelligence -- Mind.Forth AI" CR
  ." Tab key cycles through Normal, Transcript, Tutorial, Diagnostic."
  CR
  fyi @ 2 = IF   \ 29jan2008 Check the display mode.
    1000 rsvp !  \ 29jan2008 Give user more time to read screen.
  THEN           \ 29jan2008 End of fyi-test.

  1 kbtv +!  \  3sep2008 Increment -- until user input resets.
  kbtv @ 0 > IF  \  3sep2008 Skip if there has been user input.
    CR ." kbtv is positive -- calling KB-traversal" CR  \  3sep2008
    kbTraversal  \  3sep2008

  THEN  \  3sep2008  End of test of kbtv trigger variable.

; \ End of Rejuvenate; return to the Security mind-module.



\  REIFY is called by nounPhrase or verbPhrase to flush abstract
\  Psi concepts into the real names of English language reality.
\  18jan2008 version assumes Psi and En(glish) share matching "t".
:  REIFY ( express abstract concepts as real words ) \  1sep2008
  0 act !

\ midway @  t   @  DO   \ 27aug2008 Search from current time "t".
\          I   2 psi{ @  num !  \ 27aug2008 Get all num flags.
\ -1  +LOOP   \ 27aug2008 end of backwards sweep through Psi array

  midway @  t   @  DO      \ 4apr2007 Search from current time "t".
          I   1 psi{ @  0 > IF   \ If any psi has a positive "act"
          I   1 psi{ @  lexact ! \  4apr2007 A test.
          I   2 psi{ @  num !    \ 27aug2008 Passing to nounPhrase.
          I   7 psi{ @  enx !    \ 24aug2008 transfer-to-English flag.

          I   0  en{ @  enx @ = IF  \ 18jan2007 Making sure for time point.
 lexact @ I   1  en{ !  \ 18jan2008 Transfer the activation.
    num @ I   2  en{ !  \ 27aug2008 Transfer the num(ber) value.
            0 lexact !    \ 18jan2008 Reset for safety.
           THEN
         THEN         \ end of check for positive "enx" in Psi
    0 enx !      \ Reset the transfer-to-English flag.
    0 act !      \ Reset the act(ivation) level.
    0 lexact !   \ 4apr2007 Reset the lexical activation variable.
  -1  +LOOP      \ end of backwards sweep through Psi array
  0 act !        \ Reset the act(ivation) level.
;  \ End of REIFY; return to nounPhrase or verbPhrase.



\  SPEECH is for output of single words, not entire sentences.
:  SPEECH ( output of a word as text or sound ) \ 15jan2008
  fyi @ 2 = IF CR THEN  \ 18jun2006 For clarity in Tutorial display.
  0 audstop !   ( 1jan2008 Initially false value of flag )
  0 pho !       ( 1jan2008 Lest pho already be at 32 )
  aud @ onset !  ( the onset of a word is its recall-vector )
  aud @ t2s !    \ 1jan2008 Initial pre-increment text-to-speech.
  40  1  DO   \  Perform this loop up to forty times.
    t2s @  0  aud{ @ pho !  pho @ EMIT  ( say or display "pho" )
    pho @ 32 = IF 1 audstop ! THEN \ 1jan2008 One last call to Audition
    35 pov !  ( internal point-of-view ASCII 35 "#" like mindgrid )
    AUDITION    ( for reentry of a thought back into the mind )
    audstop @ 1 = IF LEAVE THEN  \ 1jan2008 Having spoken one word.
    t2s @  1+  t2s !  \ 1jan2008 Increment "t2s" for string of engrams.
    t2s @  4 aud{ @ 0 = IF  32 pho !  THEN ( 1jan2008 If end of word )

    \ 26jul2002 Diagnostic test code:
    match @ 1 = IF \ If oldConcept deals with a match,
      0 match !    \ first reset match to zero;
      LEAVE        \ leave SPEECH and go back to syntax,
    THEN           \ for generation of rest of sentence.

  LOOP             \ End of loop of up to 40 engram-fetches.
  0 match !       \ In case not otherwise reset.
  0 obstat !  \ 15jan2008 Restore ability of Audition to psi-damp words.
;  \ End of Speech; return to nounPhrase, verbPhrase, etc.


\ EGO is a function for increasing the activation of
\ the concept of self as a way of starting a self-centered
\ chain of thought when other activations have died down.
:  EGO ( revive ego after brain-dead flatline ) \ 22jan2008
  fyi @ 1 > IF CR CR
    ."   Calling EGO (AI4U Chapter 20); inert = " inert @ .
  THEN
\ 50 psi !      \ enBoot concept #50 = "self" or "ego".
   0 recon !    \ Pre-empt asking of questions.
\ CR ." EGO: Boosting self; [ENTER] say something [ENTER]" CR
 \  midway @   t @ 1+  DO  \  Search back to midway.
\   midway @   t @  DO  \ 7apr2007 Avoid "array boundary problem"
\     I 0 psi{ @ psi @ = IF  \  If concept "psi" is found...
\      24 I  1 psi{  !  \ 1apr2007 Using value from JS AI Mind
\                 12 spike !  ( 1apr2007 from JSAI Mind )
\         I  3 psi{ @  pre !  (           for use in SPREADACT )
\         I  4 psi{ @  pre !  ( 24aug2008 for use in SPREADACT )
\         I  5 psi{ @  seq !  (           for use in SPREADACT )
\         I  6 psi{ @  seq !  ( 24aug2008 for use in SPREADACT )
\         I           zone !  ( for use in SPREADACT )
\       SPREADACT             ( To engender thought about self )
\       0  pre !              \ blank out "pre" for safety
\       0  seq !              \ blank out "seq" for safety
\       LEAVE       \ Store one instance, then leave the loop.
\     THEN                    \ end of test for "psi" concept
\   -1  +LOOP                 \ end of backwards loop
  0 psi !      \ Reset for safety.
  0 inert !    \ Reset "inert" to build up again in Think.
  0 recon !    \ Thwart asking of questions about self.
\ EEG @ 1 - EEG !   \ 15oct2005 Decrement EEG towards zero.
  \ ." EEG = " EEG @ . ." and vault = " vault @ . CR
  EEG @ 0 = IF    \ If EEG has flatlined at a value of zero...

  fyi @ 2 = IF
    CR ." Repetitious thought detected; EGO module steps in." \ 22jan2008
  THEN

    vault @ rv !  \ Prepare an auditory-memory recall-vector (rv).
      rv @ 0 > IF \ Avoid crashes if rv is negative.
        BEGIN     \ Cluster of code borrowed from spreadAct
          rv @ 0 aud{ @ EMIT  1 rv +!
          rv @ 0 aud{ @ 32 >  \ Using a blank SPACE-bar.
        UNTIL      \ Stop when a non-blank character is found.
        \ ." EEG = " EEG @ . ." and rv = " rv @ . CR
        CR ." EGO Module recalls the oldest memory -- "
        rv @ aud !  \ Memory-parameter for Speech module.
        SPEECH      \ Recall and say the oldest memory.
      THEN
      0 rv !  \ Zero out the auditory associative tag

 \ CR ."  EGO: psi = " psi @ .  \ 27jan2008 Check for oldest concept.

 \ CR ."  EGO: psi topic redux "  \ 27jan2008
 \ psi @ . topic @ . redux @ . CR \ 27jan2008

     fyi @ 2 > IF   \ 27jan2008 For troubleshooting
       CR ."  EGO: EEG = " EEG @ .  ." psi = " psi @ .  \ 27jan2008
       ." topic = " topic @ .  ." redux = " redux @ .   \ 27jan2008
     THEN           \ 27jan2008 End of fyi-test.

     redux @ psi !  \ 27jan2008 For sending into nounAct.
     40 nounval !   \ 27jan2008 High enough for slosh-over?
     nounAct        \ 27jan2008 Use oldest concepts in a sentence.
     0 redux !      \ 27jan2008 Reset.

    \ psi @ topic !   \ 24jan2008
    \ 1 recon !       \ 24jan2008 Try to force a question.

  THEN  \ End of test to see if EEG has flatlined at zero.
  EEG @ 0 = IF 3 EEG ! THEN  \ 15oct2005 Reset EEG after use.
;  \  End of EGO self-assertion; return to SECURITY.


\ The Article module aims for the following entelechy goals.
\ [ ] It shall insert "THE" before something just mentioned.
\ [ ] It shall substitute "AN" for "A" when warranted.
\ [ ] It shall decide properly between the use of "A" and "THE".
\ Article is the first expansion of the Forthmind after its
\ emergence from debugging as a True AI in January of 2008.
:  Article ( select "a" or "the" before a noun )  \  1sep2008
  nphrnum @  1 = IF    \ 27aug2008 If noun is singular...
    midway @  t @  DO  \ 27aug2208 Look backwards for 1=A.
      I       0 en{ @  1 = IF  \ 27aug2008 If #1 "A" is found,
        I     6 en{ @  aud !  \ 27aug2008 Recall-vector for "A".
        LEAVE  \ 27aug2008 Use the most recent engram of "A".
      THEN  \ 27aug2008 End of search for #1 "A".
    -1 +LOOP  \ 27aug2008 End of loop finding the word "A".
    SPEECH  \ 27aug2008 Speak or display the word "A".
  THEN    \ 27aug2008 End of test for a singular noun.
  nphrnum @  2 = IF    \ 27aug2008 If noun is plural...
    midway @  t @  DO  \ Look backwards for 7=the.
      I       0 en{ @  7 = IF  \ If #7 "the" is found,
      \ I     5 en{ @  aud !  \ Recall-vector for "the".
        I     6 en{ @  aud !  \ Recall-vector for "the".
        LEAVE  \ Use the most recent engram of "the".
      THEN  \ End of search for #7 "the".
    -1 +LOOP  \ End of loop finding the word "the".
    SPEECH  \ Speak or display the word "the".
    0 nphrnum !  \ 27aug2008 Lest it interfere.
  THEN    \ 27aug2008 End of test for a plural noun.
;  \ 25aug2008 End of Article; return to nounPhrase.


\  nounPhrase is called by SVO or verbPhrase.
:  nounPhrase ( select part of a thought )  \  1sep2008
  0 reject !  \ 2apr2007 Reset in case pre-set to "1" (true).
  REIFY    ( to move abstract Psi concepts to enVocab reality )
  0 act !
  0 aud !  \ Start with a zero auditory recall-tag.
  0 motjuste !
  5 opt !  \ Look for option five (a noun).
  35 pov ! \ 3sep2008 Set "pov" to "internal".
  0 psi !  \ Start with a zero Psi concept tag.
  fyi @ 1 > IF CR  \ 18jun2006 New wording for Tutorial clarity...
    ."   nounPhrase preview of further associated concepts -- "
    CR  ."     " \ 9nov2005 Show word and what it associates to.
  THEN  \ End of test for Tutorial or Diagnostic display mode.
  midway @  t @  DO  \ 7apr2007 Hunting for "array boundary problem"
    I     4 en{ @  5 =  IF   \ 27aug2008 Look only for noun-phrases.
      fyi @ 2 > IF  \ 7apr2007 Reverting from a diagnostic test.
        I 1 en{ @ 0 > IF
          CR ."     candidate activation = " I 1 en{ @ . ."  "
          I 6 en{ @ unk !  \ 27aug2008 Temporary use of "unk"
          BEGIN
          unk @ 0 aud{ @ EMIT  1 unk +!
          unk @ 0 aud{ @ 32 =  \ Using a blank SPACE-bar.
          UNTIL
          ."                         "  \ Space to set apart chosen noun.
        THEN    \ End of test for positive (non-zero) activations.
      THEN      \ End of test for Tutorial mode.
      I   1 en{ @  act @ > IF  \ If en1 is higher than "act"
        I 0 en{ @  motjuste !  \ get psi-tag of the noun;
        I 2 en{ @  nphrnum !  \ 27aug2008 Get the num(ber) value.
              \ 0  num !  \ 27aug2008 Reset "num" after capture.

        I 4 en{ @  nphrpos ! \ 29aug2008 For branching in ENGLISH.

        I 6 en{ @  aud !  \ 27aug2008 get auditory recall-vector.
        I 6 en{ @  audjuste !  \ 27aug2008 For delivery into SPEECH
        fyi @ 2 > IF  \ In diagnostic mode...
          CR ."     nounPhrase: aud = "
          aud @ . \ aud recall-vector is...
          aud @ rv !  \ make aud the recall-vector "rv"
          ." urging psi concept #" motjuste @ . ."  "  \ 5aug2005 psi #?
          BEGIN       \ Start displaying the word-engram.
            rv @ 0 aud{ @ EMIT  1 rv +!
            rv @ 0 aud{ @ 32 =  \ Using a blank SPACE-bar.
          UNTIL        \ Stop when a blank space is found.
          ."  "
        0 rv !  \ Zero out the auditory associative tag.
        THEN    \ End of test for Diagnostic mode.
        I 1 en{ @  act !  \ To test for a higher en1.
        fyi @ 2 > IF   \ Diagnostic mode
          ."  activation = " act @ . CR ."   "  \ 13jan2008
        THEN
        ELSE   \ an error-trap (?) is needed here.
      THEN     \ end of test for en1 highest above zero.
    THEN       \ end of if-clause checking for nouns.
  -1 +LOOP     \ end of loop searching for most active noun.
  enDamp       \ to de-activate English concepts
  motjuste @  0 = IF  \ 1apr2007 From JSAI
    \ 31aug2005 Following code for "what" is borrowed from WhatAuxSDo:
    \ Call interrogative pronoun "what":
    midway @  t @  DO  \ Look backwards for 54=what.
      I       0 en{ @  54 = IF  \ If #54 "what" is found,
        54 motjuste !  \ "nen" concept #54 for "what".
      \ I     5 en{ @  aud !  \ Recall-vector for "what".
        I     6 en{ @  aud !  \ 27aug2008 Recall-vector for "what".
        LEAVE  \ Use the most recent engram of "what".
      THEN  \ End of search for #54 "what".
    -1 +LOOP  \ End of loop finding the word "what".

    SPEECH    \ Speak or display the word "what"
    1000 rsvp !  \ 31aug2005 Give human user time to enter response..
  \ LEAVE  \ Do not say the low-activation noun.
    EXIT   \ http://groups.yahoo.com/group/win32forth/message/11414
  THEN     \ 31aug2005 End of test for activation threshold.

\ motjuste @ psi !  \  1jun2006 For use in Activate module.
  act @ nounval !   \  7jun2006 So nounAct activates all nodes equally.

\ nounval @ 1 - nounval !  \ 22jan2008 test.
\ nounval @ 2 - nounval !  \ 22jan2008 test.
  nounval @ 3 - nounval !  \ 22jan2008 test.

\ act @ 5 < IF      \ 2apr2007 Paired with a low "residuum" in psiDamp
  act @ 2 < IF      \ 4apr2007 Testing with even lower values.
    motjuste @ 0 > IF  \ Do not let concept #0 abort thinking.
      1 reject !    \ 2apr2007 Flag for termination of higher module.
    \ 1 inert +!    \ 2apr2007 So that Security may soon call Ego.
    THEN               \ End of check for a positive concept-number.
  THEN              \ 2apr2007 End of test for nouns to be aborted.

\ motjuste @ lopsi @ = NOT IF  \ 14jan2008 If new psi different from hipsi
\   hipsi @  lopsi ! \ 14jan2008 Prepare to psi-damp old cresting-noun.
\   lopsi @ urpsi !  \ 14jan2008 Prepare to send urpsi into psiDamp.
\   66 caller !      \ 14jan2008 nounPhrase identified by AI4U page number.
\   psiDamp          \ 14jan2008 Damp the old crest just before new crest.
\ THEN  \ 14jan2008 End of test to avoid psi-damping the same noun.

  motjuste @  hipsi !  \ 14jan2008 Tag the currently cresting noun...
  \ ...so that it may be converted to lopsi when the next word crests.
\ fyi @ 1 > IF    \ 14jan2008 Select what display mode to show in...
\   CR ."    nounPhr: lopsi @ hipsi = " lopsi @ . hipsi @ . \ 14jan2008
\ THEN            \ 14jan2008 End of fyi-test.

\ CR ." nPhr: motjuste & nphrnum = " motjuste @ . nphrnum @ . \ 27aug08

  Article  \ 25aug2008 A brand-new mind-module.

  motjuste @ psi !  \ 25aug2008 Moved down below the call to Article
  nounAct           \  7jun2006 To impart a winning activation equally.
  0 nounval !       \  7jun2006 Safety measure after use of jolt.

\ Article  \ 25aug2008 A brand-new mind-module.

  audjuste @ aud !  \ 25aug2008 Prevent Article SPEECH interference.
\ ." nPh into Speech psi & aud = " psi @ . ." & " aud @ . \ 25aug2008 Test

  SPEECH  \ 2apr2007 from JSAI: Display or speak the selected noun-phrase.
  32 EMIT            \  Say a SPACE-BAR 32.

  fyi @ 2 > IF CR  \ 31jul2005 Seeing what calls Activate.
  ."   from nounPhrase "
  THEN
  motjuste @ topic !   \  6jan2008 For "detour" into a question.
  0 act !              \ Reset for safety.
  0 psi  !             \ 26jul2002 Reset for safety.
;  \ End of nounPhrase; return to SVO or verbPhrase.


\  verbPhrase is called from the subject-verb-object SVO
\  syntax module to find and express a verb +/- object.
:  verbPhrase ( select part of a thought ) \  1sep2008
  REIFY       \ move abstract Psi concepts to enVocab reality
  0 act !     \ precaution even though zeroed in REIFY
  0 aud !     \ Start with a zero auditory recall-tag.
  0 detour !  \ 19dec2007 Reset this abort-flag at the outset.
  0 motjuste !
  8 opt !  \ Look for option eight (a verb).
  0 psi !  \ Start with a zero Psi associative tag.
\ 0 vbpsi !  \ 22jan2008 Start with a zero verbpsi for thotnum.

  adverbact 32 > IF  \ 29aug2008 Idea for inserting adverbs.
    \ adVerb           \ 29aug2008 Module does not exist yet.
  THEN  \ 29aug2008 End of idea for insertion of adverbs.

  fyi @ 1 > IF CR  \ 18jun2006 New wording for Tutorial clarity.
 ."   verbPhrase preview with slosh-over indicated by + --"
    CR
 ."   Noun & verb activation must slosh over onto logical direct objects."
    CR  ."    " \ 9nov2005 Show word and what it associates to.
  THEN
  midway @ t  @ DO  \ Search backwards through enVocab
    I      4 en{ @  8 = IF  \ 27aug2008 only look at predicate/verbs
    fyi @ 3 = IF  ." vP" THEN  \ 15sep2005 and 13oct2005 test
      fyi @ 2 > IF      \ 24sep2005 Check the display-flag status.
        I 1 en{ @ 0 > IF
           CR ."     cand. act = " I 1 en{ @ . ."  "
           ." w. psi seq #"
           I 6 psi{ @ seq ! seq @ . ."  "  \ 24aug2008 W. psi "seq" #...
           I 6  en{ @ unk  !  \ 27aug2008 Temporary use of "unk"
           BEGIN
           unk @ 0 aud{ @ EMIT  1 unk +!
           unk @ 0 aud{ @ 32 =  \ Using a blank SPACE-bar.
           UNTIL
           ."  w. nodal dir. obj. "  \ 4sep2005 focussing on slosh-over
           midway @ t @ DO  \ Look beyond verb for the "seq" concept
             I   0  psi{ @   seq @  =  IF  \ If match of "seq" is found,
               I 1  psi{ @ . ." = act "  \ Correct node of psi?
               I 7  psi{ @   psi7 !    \ 24aug2008 Get the enx as psi7
               LEAVE                   \ Stop looking after one find.
             THEN       \  End of check for the "seq" concept
           -1  +LOOP    \  End of backwards search for "seq" concept
           midway @ t @ DO  \ Use enx to get the aud recall-vector
             I   0  en{ @    psi7 @ = IF  \ 27aug2008
               I 6  en{ @  rv ! \ 27aug2008 Store auditory recall-vector.
               LEAVE    \ Use only the most recent auditory engram.
             THEN
           -1  +LOOP    \ End of backwards search for "psi6" vocab item.
           rv @ 0 > IF  \ Avoid crashes if rv is negative.
             BEGIN
               rv @ 0 aud{ @ EMIT  1 rv +!
               rv @ 0 aud{ @ 32 =  \ Using a blank SPACE-bar.
             UNTIL        \ Stop when a blank space is found.
           THEN
           ."  spike = " spike @ .    \ 4sep2005 from spreadAct?
           0 rv !    \ Zero out the auditory associative tag.
           ."     "  \ Space to set apart chosen verb.
        THEN    \ End of test for positive (non-zero) activations.
      THEN      \ End of test of display-flag status.
      I    1 en{ @  act @ > IF  ( if en1 is higher )
        I  0 en{ @  motjuste !  ( store psi-tag of verb )
        I  6 en{ @  aud !  ( 27aug2008 auditory recall-vector )

        fyi @ 2 > IF        \ 9nov2005 Diagnostic mode
          CR ." verbPhrase: aud = "
          aud @ . \ aud recall-vector is...
          aud @ rv !  \ make aud the recall-vector "rv"
          ." urging psi concept #" motjuste @ . ."  " \ 5aug2005 psi #?
          BEGIN       \ Start displaying the word-engram.
            rv @ 0 aud{ @ EMIT  1 rv +!
            rv @ 0 aud{ @ 32 =  \ Using a blank SPACE-bar.
          UNTIL        \ Stop when a blank space is found.
          ."  "
          0 rv !       \ Zero out the auditory associative tag.
        THEN           \ End of test for Diagnostic mode.

        I  1 en{ @  act !  ( to test for a higher en1 )

       \  fyi @ 2 = IF CR   \ 19&27dec2007 Prelim to using "detour"
       \    ."  verbPhr: act = " act @ . ."   " CR
       \  THEN  \ 19&27dec2007 End of diagnostic test.

          fyi @ 3 = IF CR   \ Diagnostic mode
            ."  verbPhr: act = " act @ . ."   "
          THEN
        ELSE  \ An error-trap (?) is needed here.
      THEN  \ end of test for en1 highest above zero.
    THEN    \ end of test for opt=8 verbs
  -1 +LOOP  \ end of loop cycling back through English lexicon
   act @  verbval !   \ 3apr2007 For transferring val(ue) to verbAct.
   0 psi !            \ A precaution lest psi transit SPEECH.

   \ 22jan2008 verb-psi for calculating "thotnum"
   motjuste @ 0 > IF motjuste @ vbpsi ! THEN  \ 22jan2008

  fyi @ 2 > IF   \ Test for origin of YES-bug.
    CR ." verbPhrase: motjuste = " motjuste @ . ." going into SPEECH."
    CR ." verbPhrase: aud = " aud @ . ." going into SPEECH."
  THEN           \ End of test for origin of YES-bug.

  motjuste @ 0 = IF  \  3jan2008  If no candidate-verb is found...
    1 detour !   \  3jan2008 Set the detour flag to 1-as-true.
    fyi @ 1 > IF  \  6jan2008 Display in both Tutorial and Diagnostic.
      CR ."   verbPhr: detouring when no candidate-verb is found. "
      CR ."   verbPhr: detour value is at " detour @ .  \  3jan2008
    THEN          \  3jan2008 End of test for Tutorial mode
  \ LEAVE   \  3jan2008 Go back up to any calling module. e.g., SVO.
  \ LEAVE   \  3jan2008 Ting's manual says LEAVE is for DO-loops.
  THEN  \  3jan2008 End of test for no candidate verb found.

  motjuste @ 0 > IF  \ 15sep2005 Prevent aud-0 of spurious "YES".
\ motjuste @ psi !   \ 10jun2006 For use in verbAct module.
\ verbAct            \  7jun2006 For slosh-over of subj+verb onto object.
  \ act @ 18 < IF  \ 13jan2008 Lower so that crest-noun finds a verb.
    act @ 20 < IF  \ 16jan2008 To detour from low-activation verbs.

      1 detour !   \ 27dec2007 Set the detour flag to 1-as-true.
      fyi @ 1 > IF  \  6jan2008 Display in Tutorial and in Diagnostic.
    CR ."       verbPhr: detour because verb-activation is only " act @ .
      THEN          \ 27dec2007 End of test for Tutorial mode
    THEN      \ 27dec2007 End of test for verb with activation too low.

    detour @ 0 = IF  \  3jan2008 Speak verb only if detour is false.

\   motjuste @ lopsi @ = NOT IF  \ 14jan2008 If new psi different from hipsi
\   hipsi @  lopsi ! \ 14jan2008 Prepare to psi-damp old cresting word.
\   lopsi @ urpsi !  \ 14jan2008 Prepare to send urpsi into psiDamp.
\   62 caller !      \ 14jan2008 verbPhrase identified by AI4U page number.
\   psiDamp          \ 14jan2008 Damp the old crest just before new crest.

    62 caller !      \ 22jan2008 verbPhrase identified by AI4U page number.
    psiDamp          \ 22jan2008 Suppress background activations.

\   THEN  \ 14jan2008 End of test to avoid psi-damping the same word.
    motjuste @  hipsi !  \ 14jan2008 Tag the currently cresting word...
    \ ...so that it may be converted to lopsi when the next word crests.
    fyi @ 2 > IF    \ 14jan2008 Select what display mode to show in...
      CR ."    nounPhr: lopsi @ hipsi = " lopsi @ . hipsi @ . \ 14jan2008
    THEN            \ 14jan2008 End of fyi-test.

    motjuste @ psi ! \ 11jan2008 For use in verbAct module.
    verbAct          \ 11jan2008 For slosh-over of subj+verb onto object.
      SPEECH         \  To say or display the verb
    THEN             \  3jan2008 End of "detour" test.
  THEN               \ 15sep2005 End of test for motjuste = 0.

 detour @ 0 = IF     \  3jan2008 Only finish vPhr if "detour" is false.

  10 act !  \ 3apr2007 From JSAI: Some activation is necessary.
  fyi @ 2 > IF CR    \ Clean up the Tutorial display.
  ."   in verbPhrase after SPEECH output of verb"
  THEN
  fyi @ 2 > IF CR    \ Seeing what calls psiDamp
  ."   from verbPhrase after speaking of verb, psiDamping #" motjuste @ .
  THEN
  motjuste @  urpsi !  \ For use in psiDamp.
  22 residuum !  \ Trying to let spike win, over residual activation.
  62 caller !    \ 13jan2008 verbPhrase identified by AI4U page number.
\ psiDamp        \ 29apr2005 Necessary for chain of thought.
\ psiDamp        \ 14jan2008 Commenting out and using lopsi & hipsi.
  0 caller !     \ 13jan2008 Reset caller-ID for safety.
   2 residuum !  \ 28aug2005 Restore minimal psiDamp value.
  enDamp     \ to de-activate English concepts
  32 EMIT        \ Insert a SPACE.
  15 residuum !  \ Give direct objects higher residuum than subjects.
  1 dirobj ! \ 14sep2005 Declare seeking of a direct object.

  fyi @ 2 = IF  \ 30nov2007 For greater clarity in Tutorial mode.
  CR ."         verbPhrase calls nounPhrase for object of sentence." CR
\ CR ."         vPhr: detour = " detour @ .  \  3jan2008 test.
  THEN

\ 0 dopsi !      \ 22jan2008 Clear old value before new value.
  nounPhrase     \ To express direct object of verb,

  \ 22jan2008 direct-object psi for "thotnum"
  motjuste @ 0 > IF motjuste @ dopsi ! THEN  \ 22jan2008

  0 dirobj !     \ 14sep2005 No longer seeking a direct object.
  2 residuum !   \ 28aug2005 Restore minimal psiDamp value.

 THEN  \  3jan2008 End of test that skips code if "detour" is true.

  fyi @ 2 > IF     \  6jan2008 Test for high fyi value.
  CR ."   vPhr end: detour = " detour @ .  \  3jan2008
  THEN             \  6jan2008 End of test.
;  \  End of verbPhrase; return to the SVO syntax module.


\ Conjoin selects a hopefully appropriate conjunction and
\ allows the AI to answer a "why" question with a "because"
\ statement, under the assumption here that the thinking of
\ the AI will tend to display a modicum of explanatory logic.
:  Conjoin  \ ATM 16aug2002; or your ID & date.
  questype @  16 =  IF  \ If the question-type is "why" ...
    18     conj !       \ use the conjunction "because";
  ELSE  17 conj !       \ otherwise use "and".
  THEN                  \ "questype" from oldConcept.
  \ Code adapted "mutatis mutandis" from auxVerb "do" search:
  midway @  t @  DO  \ Look backwards for "conj".
    I       0 en{ @  conj @ = IF  \ If conjunction is found,
      conj @  motjuste !  \ "nen" concept for conjunction;
      I     5 en{ @  aud !  \ Recall-vector for conjunction.
      LEAVE  \ Only find one instance of the conjunction.
    THEN  \ End of search for conjunction.
  -1 +LOOP  \ End of loop finding the appropriate conjunction.
  SPEECH    \ Speak or display the chosen conjunction.
  0 questype !  \ Reset "questype" after any use.
;  \ End of Conjoin; return to the SVO module.


\ The Predicate module aims for the following entelechy goals.
\ [ ] If no predicate nominative is known, detour into a question.
\ [ ] If no transitive verb is most active, default to a verb of being.
\ [ ] If no direct object is found, detour into asking a question.
\ [ ] If a transitive verb is most active, try to find a direct object.
\ [X] Find whatever verb is most active after a noun-phrase.
\ 29aug2008 Predicate is initially a clone of verbPhrase
\ and then radically modified to serve certain purposes.
:  Predicate ( supervise verb syntax ) \  1sep2008
  REIFY       \ move abstract Psi concepts to enVocab reality
  0 act !     \ precaution even though zeroed in REIFY
  0 aud !     \ Start with a zero auditory recall-tag.
  0 detour !  \ 19dec2007 Reset this abort-flag at the outset.
  0 motjuste !
  8 opt !  \ Look for option eight (a verb).
  0 psi !  \ Start with a zero Psi associative tag.

  adverbact 32 > IF  \ 29aug2008 Idea for inserting adverbs.
    \ adVerb           \ 29aug2008 Module does not exist yet.
  THEN  \ 29aug2008 End of idea for insertion of adverbs.

  fyi @ 1 > IF CR  \ 18jun2006 New wording for Tutorial clarity.
\ ."   verbPhrase preview with slosh-over indicated by + --"
 ."   Predicate preview with slosh-over indicated by + --" \ 29aug2008
    CR
 ."   Noun & verb activation must slosh over onto logical direct objects."
    CR  ."    " \ 9nov2005 Show word and what it associates to.
  THEN
  midway @ t  @ DO  \ Search backwards through enVocab
    I      4 en{ @  8 = IF  \ 27aug2008 only look at predicate/verbs
      fyi @ 3 = IF  ." Predicate" THEN  \ 29aug2008 change
      fyi @ 2 > IF      \ 24sep2005 Check the display-flag status.
        I 1 en{ @ 0 > IF
           CR ."     cand. act = " I 1 en{ @ . ."  "
           ." w. psi seq #"
           I 6 psi{ @ seq ! seq @ . ."  "  \ 24aug2008 W. psi "seq" #...
           I 6  en{ @ unk  !  \ 27aug2008 Temporary use of "unk"
           BEGIN
           unk @ 0 aud{ @ EMIT  1 unk +!
           unk @ 0 aud{ @ 32 =  \ Using a blank SPACE-bar.
           UNTIL
           ."  w. nodal dir. obj. "  \ 4sep2005 focussing on slosh-over
           midway @ t @ DO  \ Look beyond verb for the "seq" concept
             I   0  psi{ @   seq @  =  IF  \ If match of "seq" is found,
               I 1  psi{ @ . ." = act "  \ Correct node of psi?
               I 7  psi{ @   psi7 !    \ 24aug2008 Get the enx as psi7
               LEAVE                   \ Stop looking after one find.
             THEN       \  End of check for the "seq" concept
           -1  +LOOP    \  End of backwards search for "seq" concept
           midway @ t @ DO  \ Use enx to get the aud recall-vector
             I   0  en{ @    psi7 @ = IF  \ 27aug2008
               I 6  en{ @  rv ! \ 27aug2008 Store auditory recall-vector.
               LEAVE    \ Use only the most recent auditory engram.
             THEN
         \ -1  +LOOP    \ End of backwards search for "psi6" vocab item.
           -1  +LOOP    \  1sep2008 End of search for "psi7" vocab item.
           rv @ 0 > IF  \ Avoid crashes if rv is negative.
             BEGIN
               rv @ 0 aud{ @ EMIT  1 rv +!
               rv @ 0 aud{ @ 32 =  \ Using a blank SPACE-bar.
             UNTIL        \ Stop when a blank space is found.

           THEN
           ."  spike = " spike @ .    \ 4sep2005 from spreadAct?
           0 rv !    \ Zero out the auditory associative tag.
           ."     "  \ Space to set apart chosen verb.
        THEN    \ End of test for positive (non-zero) activations.
      THEN      \ End of test of display-flag status.
      I    1 en{ @  act @ > IF  ( if en1 is higher )

        I  0 en{ @  motjuste !  ( store psi-tag of verb )
        I  4 en{ @  predpos ! ( 29aug2008 grab winning part of speech )
        I  6 en{ @  aud !  ( 27aug2008 auditory recall-vector )

        fyi @ 2 > IF        \ 9nov2005 Diagnostic mode
          CR ." Predicate: aud = "  \ 29aug2008
          aud @ . \ aud recall-vector is...
          aud @ rv !  \ make aud the recall-vector "rv"
          ." urging psi concept #" motjuste @ . ."  " \ 5aug2005 psi #?
          BEGIN       \ Start displaying the word-engram.
            rv @ 0 aud{ @ EMIT  1 rv +!
            rv @ 0 aud{ @ 32 =  \ Using a blank SPACE-bar.
          UNTIL        \ Stop when a blank space is found.
          ."  "
          0 rv !       \ Zero out the auditory associative tag.
        THEN           \ End of test for Diagnostic mode.

        I  1 en{ @  act !  ( to test for a higher en1 )

          fyi @ 3 = IF CR   \ Diagnostic mode
            ."  Predicate: act = " act @ . ."   "  \ 29aug2008
          THEN
        ELSE  \ An error-trap (?) is needed here.
      THEN  \ end of test for en1 highest above zero.
    THEN    \ end of test for opt=8 verbs
  -1 +LOOP  \ end of loop cycling back through English lexicon
   act @  verbval !   \ 3apr2007 For transferring val(ue) to verbAct.
   0 psi !            \ A precaution lest psi transit SPEECH.

   \ 22jan2008 verb-psi for calculating "thotnum"
   motjuste @ 0 > IF motjuste @ vbpsi ! THEN  \ 22jan2008

  fyi @ 2 > IF   \ Test for origin of YES-bug.
    CR ."  Predicate: motjuste = " motjuste @ . ." going into SPEECH."
    CR ."  Predicate: aud = " aud @ . ." going into SPEECH." \ 29aug2008
  THEN           \ End of test for origin of YES-bug.

  motjuste @ 0 = IF  \  3jan2008  If no candidate-verb is found...
    1 detour !   \  3jan2008 Set the detour flag to 1-as-true.
    fyi @ 1 > IF  \  6jan2008 Display in both Tutorial and Diagnostic.
      CR ."   Predicate: detouring when no candidate-verb is found. "
      CR ."   Predicate: detour value is at " detour @ .  \  29aug2008
    THEN          \  3jan2008 End of test for Tutorial mode
  \ LEAVE   \  3jan2008 Go back up to any calling module. e.g., SVO.
  \ LEAVE   \  3jan2008 Ting's manual says LEAVE is for DO-loops.
  THEN  \  3jan2008 End of test for no candidate verb found.

  motjuste @ 0 > IF  \ 15sep2005 Prevent aud-0 of spurious "YES".
\ motjuste @ psi !   \ 10jun2006 For use in verbAct module.
\ verbAct            \  7jun2006 For slosh-over of subj+verb onto object.
  \ act @ 18 < IF  \ 13jan2008 Lower so that crest-noun finds a verb.
    act @ 20 < IF  \ 16jan2008 To detour from low-activation verbs.

      1 detour !   \ 27dec2007 Set the detour flag to 1-as-true.
      1 recon !    \  1sep2008 So that ENGLISH will call ASK.

      fyi @ 1 > IF  \  6jan2008 Display in Tutorial and in Diagnostic.
    CR ."     Predicate: detour because verb-activation is only " act @ .
      THEN          \ 27dec2007 End of test for Tutorial mode
    THEN      \ 27dec2007 End of test for verb with activation too low.

    detour @ 0 = IF  \  3jan2008 Speak verb only if detour is false.

\   motjuste @ lopsi @ = NOT IF  \ 14jan2008 If new psi different from hipsi
\   hipsi @  lopsi ! \ 14jan2008 Prepare to psi-damp old cresting word.
\   lopsi @ urpsi !  \ 14jan2008 Prepare to send urpsi into psiDamp.
\   62 caller !      \ 14jan2008 verbPhrase identified by AI4U page number.
\   psiDamp          \ 14jan2008 Damp the old crest just before new crest.

    62 caller !      \ 22jan2008 verbPhrase identified by AI4U page number.
    psiDamp          \ 22jan2008 Suppress background activations.

\   THEN  \ 14jan2008 End of test to avoid psi-damping the same word.
    motjuste @  hipsi !  \ 14jan2008 Tag the currently cresting word...
    \ ...so that it may be converted to lopsi when the next word crests.
    fyi @ 2 > IF    \ 14jan2008 Select what display mode to show in...
      CR ."  Predicate: lopsi @ hipsi = " lopsi @ . hipsi @ . \ 29aug2008
    THEN            \ 14jan2008 End of fyi-test.

    motjuste @ psi ! \ 11jan2008 For use in verbAct module.
    verbAct          \ 11jan2008 For slosh-over of subj+verb onto object.
      SPEECH         \  To say or display the verb
    THEN             \  3jan2008 End of "detour" test.
  THEN               \ 15sep2005 End of test for motjuste = 0.

 detour @ 0 = IF     \  3jan2008 Only finish vPhr if "detour" is false.

  10 act !  \ 3apr2007 From JSAI: Some activation is necessary.
  fyi @ 2 > IF CR    \ Clean up the Tutorial display.
  ."   in Predicate after SPEECH output of verb" \ 29aug2008
  THEN
  fyi @ 2 > IF CR    \ Seeing what calls psiDamp
  ."   from Predicate after speaking of verb, psiDamping #" motjuste @ .
  THEN
  motjuste @  urpsi !  \ For use in psiDamp.
  22 residuum !  \ Trying to let spike win, over residual activation.
  62 caller !    \ 13jan2008 verbPhrase identified by AI4U page number.
\ psiDamp        \ 29apr2005 Necessary for chain of thought.
\ psiDamp        \ 14jan2008 Commenting out and using lopsi & hipsi.
  0 caller !     \ 13jan2008 Reset caller-ID for safety.
   2 residuum !  \ 28aug2005 Restore minimal psiDamp value.
  enDamp     \ to de-activate English concepts
  32 EMIT        \ Insert a SPACE.
  15 residuum !  \ Give direct objects higher residuum than subjects.
  1 dirobj ! \ 14sep2005 Declare seeking of a direct object.

  fyi @ 2 = IF  \ 30nov2007 For greater clarity in Tutorial mode.
  CR ."          Predicate calls nounPhrase for object of sentence." CR

  THEN

\ 0 dopsi !      \ 22jan2008 Clear old value before new value.
  nounPhrase     \ To express direct object of verb,

  \ 22jan2008 direct-object psi for "thotnum"
  motjuste @ 0 > IF motjuste @ dopsi ! THEN  \ 22jan2008

  0 dirobj !     \ 14sep2005 No longer seeking a direct object.
  2 residuum !   \ 28aug2005 Restore minimal psiDamp value.

 THEN  \  3jan2008 End of test that skips code if "detour" is true.

  fyi @ 2 > IF     \  6jan2008 Test for high fyi value.
  CR ."   Predicate end: detour = " detour @ .  \  3jan2008
  THEN             \  6jan2008 End of test.
;  \  1sep2008 End of Predicate; return to ENGLISH.


\  SVO is called by ENGLISH;is negated by negSVO;
\  and is one structure for an English sentence.
:  SVO ( Subject + Verb + Object ) \ 27jan2008

  \ 20jan2008 Let final hipsi in one sentence be hipsi in next sentence.
  \ 20jan2008 Following line may belong in English or in Think.
\ 0 lopsi !  \ 20jan2008 Do not psi-damp a high-activation concept.
  \ 21jan2008 Above line being commented out because lopsi may easily
  \ be carried over from a previous sentence.

  \  The AI fills in the next line by generating a thought:
  CR ." Robot: "
  \ We insert a "{" for the sake of Rejuvenate.
  123  t @  2 aud{ !  \  123 is ASCII bracket "{"
  1 subj !  \ 15sep2005 Set flag that noun will be SVO subject.
\ 0 supsi !   \ 22jan2008 Clear any old value before new value.
  fyi @ 2 = IF  \ 30nov2007 For greater clarity in Tutorial mode.
    CR ."       SVO calls nounPhrase for subject of sentence." CR
  THEN
  nounPhrase  ( finds "le mot juste" to be the subject )

\ reject @ 1 = IF   \ 2apr2007 If subject-selection is aborted...
\   0 reject !      \ 2apr2007 Reset flag as action is being taken.
\   7 EMIT  \ 2apr2007 A temporary diagnostic bell-sound; remove soon.
\   LEAVE           \ 2apr2007 Stop the module if activation is too low.
\ THEN              \ 2apr2007 End of test for "reject" abort-flag.

  \ 22jan2008 Trap the "supsi" for "thotnum".
  motjuste @ 0 > IF motjuste @ supsi ! THEN  \ 22jan2008

  motjuste @ decpsi1 @ = IF  \ 20apr2006 Check for repetition...
    motjuste @ decpsi2 !     \ so as to accelerate de-activations...
  THEN                       \ and thus avoid quasi-flatliners.
  motjuste @ decpsi1 !  \ 20apr2006 Keep track of just-thought concept.
  0 motjuste !  \ 20apr2006 Safety measure moved here from nounPhrase.
  0 subj !  \ 15sep2005 Reset flag after using it.
  \ AUDITION, will need to store retroactively in aud{
  \ the concept number of "motjuste" as a psi-tag "psi":
  motjuste @ psi !  ( send the found psi-tag into AUDITION ) 
  fyi @ 2 = IF  \ 30nov2007 For greater clarity in Tutorial mode.
  CR ."       SVO calls verbPhrase." CR
  THEN

\ CR ." SVO pre-vPhr -- detour value is at " detour @ .   \ 28dec2007 test
  fyi @ 2 > IF      \  6jan2008 Test for a high fyi value.
  CR ."   SVO pre-vPhr -- detour value is at " detour @ . \  3jan2008 test
  THEN              \  6jan2008 End of fyi test.

  verbPhrase  ( finds "le mot juste" for verb and for object )

\ CR ." SVO post-vPhr -- detour value is at " detour @ .  \ 28dec2007 test
  fyi @ 2 > IF      \  6jan2008 Test for a high fyi value.
  CR ."   SVO post-vPhr -- detour value is  " detour @ .  \  3jan2008 test
  THEN              \  6jan2008 End of test.

  detour @ 1 = IF   \ 28dec2007 If verb-selection is aborted...
    fyi @ 1 > IF    \ 13jan2008
    \ CR  ."   SVO: Detouring from inability to select a verb. "
    \ CR  ."   SVO detours into Ask module to form a question. "
      CR  ."     SVO sets recon-flag so a question will be asked "
    THEN        \ 13jan2008 End of fyi-test.
    1 recon !   \ 28dec2007 Necessary to transmit curiosity.
 \  Ask             \ 28dec2007 Must be called from a higher module.
  0 detour !    \  4jan2008 Reset "detour" after setting "recon" flag.
  THEN  \ 28dec2007 End of test for "detour" abort-flag.

  \  The following inactive insertion is merely for closure:
  125  t @ 1+  2 aud{ ! \ insert "}" without incrementing "t"
  13 pho !  ( ASCII 13 CR to trip a retroactive change )
  AUDITION  \ to receive the carriage-return CR 13
  5 bias !  \ 26jul2002 Expect next to parse a noun=5.
  0 reject ! \ 2apr2007 Reset for safety.

\ CR ."   SVO end: detour = " detour @ .  \  3jan2008 test
  fyi @ 2 > IF            \  6jan2008 Test for high fyi value.
  CR ."   SVO end: detour @ recon = " detour @ . recon @ . \  6jan2008
  THEN                    \ 6jan2008 End of test.

  0 thotnum !   \ 22jan2008

  1 thotcyc +!  \ 22jan2008 Increment thotcyc by one.

\ CR ."  SVO: supsi vbpsi dopsi = " supsi @ . vbpsi @ . dopsi @ . \ 22jan2008

  supsi @ 0 > IF
    supsi @ 1000000 * supsi !  \ 22jan2008 For first of three groups.
    supsi @ thotnum !  \ 22jan2008 Lay down first of three groups.
  THEN
  0 supsi !          \ 22jan2008 Reset supsi.

  vbpsi @ 0 > IF
    vbpsi @ 1000 * vbpsi !  \ 22jan2008 For second of SVO group.
    vbpsi @ thotnum +!    \ 22jan2008 Include second group of three.
  THEN
  0 vbpsi !             \ 22jan2008 Reset vbpsi.

  dopsi @ 0 > IF
    dopsi @ thotnum +!  \ 22jan2008 Include third group of SVO three.
  THEN
  0 dopsi !             \ 22jan2008 Reset dopsi.

  thotcyc @ 1 = IF thotnum @ thot1 ! THEN  \ 22jan2008 Capture thought #1.
  thotcyc @ 2 = IF thotnum @ thot2 ! THEN  \ 22jan2008 Capture thought #2.
  thotcyc @ 3 = IF
    thotnum @ thot3 !

    thot1 @ thot3 @ = IF
      7 EMIT    \ 22jan2008 SOund an ASCII bell.
      0 EEG !   \ 22jan2008 Declare a flatliner EEG.
    THEN   \ 22jan2008 End of test for match between thot1 and thot3.

    thot2 @ thot3 @ = IF
      7 EMIT    \ 22jan2008 Sound an ASCII bell.

      \ fyi @ 1 > IF   \ 27jan2008
          CR  ." REPETITIOUS THOUGHTS HAVE BEEN DETECTED. " \ 27jan2008
  CR  ." STARTING NEW PATHWAY IN MEANDERING CHAIN OF THOUGHT " \ 27jan2008
      \ THEN           \ 27jan2008 End of fyi-test.

      0 EEG !   \ 22jan2008 Declare a flatliner EEG.
    THEN   \ 22jan2008 End of test for match between thot2 and thot3.

  THEN  \ 22jan2008 Capture thought #3
  thotcyc @ 3 > IF 0 thotcyc ! THEN        \ 22jan2008 Return to start.
  fyi @ 2 > IF   \ 27jan2008 Show details only in proper display mode.
    CR ." SVO: thotcyc & thtnum = " thotcyc @ . thotnum @ .
    CR ." SVO: thot1 = " thot1 @ .  \ 22jan2008
    CR ." SVO: thot2 = " thot2 @ .  \ 22jan2008
    CR ." SVO: thot3 = " thot3 @ .  \ 22jan2008
  THEN           \ 22jan2008 End of fyo-test.
  EEG @ 0 = IF         \ 22jan2008
  \ verbClear          \ 22jan2008 so as not to corrupt chains of thought
    verbClip           \ 24jan2008 so as to leave a modicum of activation.
    EGO                \ 22jan2008 For appropriate action.
  \ EGO  \ 24jan2008 Call again to cause a higher activation.
  THEN                 \ 22jan2008
;  \ End of SVO; return to the ENGLISH module.


\ auxVerb provides part of a compound verb form.
:  auxVerb ( auviliary Verb ) \ atm 14oct2005; 27aug2008
  \ "can" -- one possible auxiliary verb.
  \ "dare" -- one possible auxiliary verb.
  \ "do" -- call a form of the auxiliary verb "do":
  midway @  t @  DO  \ Look backwards for 59=do.
    I       0 en{ @  59 = IF  \ If #59 "do" is found,
      59 motjuste !  \ "nen" concept #59 for "do".
    \ I     5 en{ @  aud !  \ Recall-vector for "do".
      I     6 en{ @  aud !  \ 27aug2008 Recall-vector for "do".
      LEAVE  \ Use the most recent engram of "do".
    THEN  \ End of search for #59 "do".
  -1 +LOOP  \ End of loop finding auxiliary verb "do".
  SPEECH    \ Speak or display the auxiliary verb "do".
  fyi @ 2 > IF CR  \ Diagnostic message,
  ."   from auxVerb after speaking of DO, psiDamping concept #59 DO"
  THEN
  59 urpsi !  \ For use in psiDamp to de-activate the "DO" concept.
  51 caller ! \ 13jan2008 auxVerb identified by AI4U page number
  psiDamp     \  6aug2005 As when verbPhrase has sent a verb to Speech.
  0 caller !  \ 13jan2008 Reset caller-ID for safety.
  \ enDamp    \ de-activate English concepts; 6aug2005: needed here?
  \ "may" -- one possible auxiliary verb.
  \ "must" -- one possible auxiliary verb.
  \ "shall" -- one possible auxiliary verb.
  \ "will" -- one possible auxiliary verb.
;  \ End of auxVerb; return to the negSVO module.


\ negSVO negates a subject-verb-object sentence.
:  negSVO ( negation of Subject + Verb + Object ) \  1sep2008
  fyi @ 1 > IF CR  \ 13oct2005 Leave room for Transcript mode.
    ."     Calling negSVO (AI4U Chapter 10)." CR
  THEN
  \ We insert a "{" for the sake of Rejuvenate.
  123  t @  2 aud{ !  \  123 is ASCII bracket "{"
  nounPhrase  \ for subject of negSVO sentence.
  32 pho !  \ Reset "pho" by blanking it out.
  auxVerb   \ Fetch a form of auxiliary verb "do".
  midway @  t @  DO  \ Look backwards for 12=not.
    I       0 en{ @  12 = IF  \ If #12 "not" is found,
      12 motjuste !  \ "nen" concept #12 for "not".
      I     6 en{ @  aud !  \ 27aug2008 Recall-vector for "not".
      LEAVE  \ Use the most recent engram of "not".
    THEN  \ End of search for #12 "not".
  -1 +LOOP  \ End of loop finding the word "not".
  SPEECH  \ Speak or display the word "not".
  verbPhrase  \ Find a verb +/- a direct object.
  \  The following inactive insertion is merely for closure:
  125 t @ 1+ 2 aud{ ! \ Insert "}" without incrementing "t".
  1   t +!  \ Advance time to separate reentered words.
  1 spt +!  \ Increment SPACE-time simultaneously.
  enDamp    \ Deactivate the English lexicon.
  audDamp   \ Protect audRecog?
\  13 pho !  ( ASCII 13 CR to trip a retroactive change )
\  AUDITION  \ to receive the carriage-return CR 13
  5 bias !  \ 29jul2002 Expect next to parse a noun=5.
;  \ End of negSVO; return to the English module.


\ whatAuxSDo is: what + Auxiliary + Subject + "Do"
\ where the Subject is a pre-selected "topic" concept.
:  whatAuxSDo ( what DO Subjects DO ) \  1sep2008
\  4jan2008 Calls to psiDecay may gradually be commented out.
  psiDecay   \ In the isolated module psiDecay has carte blanche.
  \ Call interrogative pronoun "what":
  midway @  t @  DO  \ Look backwards for 54=what.
    I       0 en{ @  54 = IF  \ If #54 "what" is found,
      54 motjuste !  \ "nen" concept #54 for "what".
      I     6 en{ @  aud !  \ 27aug2008 Recall-vector for "what".
      LEAVE  \ Use the most recent engram of "what".
    THEN  \ End of search for #54 "what".
  -1 +LOOP  \ End of loop finding the word "what".
  SPEECH    \ Speak or display the word "what".
  fyi @ 2 > IF CR  \ Diagnostic message,
  ."   from whatAuxSDo after speaking of WHAT, psiDamping concept #54"
  THEN
  54 urpsi !  \ For use in psiDamp to de-activate the "WHAT" concept.
  42 caller ! \ 13jan2008 whatAuxSDo identified by AI4U page number.
  psiDamp     \  6aug2005 As when verbPhrase has sent a verb to Speech.
  0 caller !  \ 13jan2008 Reset caller-ID for safety.
  \ Call a form of the auxiliary verb "do":
  auxVerb   \  7sep2005 Any of several auxiliary verbs. 
  0 motjuste !  \ 7sep2005 safety measure
  midway @  t @  DO  \ Look backwards for "topic".
    I       0 en{ @  topic @ = IF  \ If "topic" is found,
      topic @ motjuste !   \ mixing apples & oranges?
      I     6 en{ @ aud ! \ 27aug2008 Auditory recall-vector for "topic".
      LEAVE
    THEN     \ End of search for #"topic".
  -1 +LOOP  \ End of loop finding the lexical "topic" item.
  motjuste @ urpsi !   \ 7sep2005 for sake of psiDamp
  15 residuum !  \ whatAuxSDo module -- 14sep2005 nota bene.
  42 caller !    \ 13jan2008 whatAuxSDo identified by AI4U page number.
  psiDamp        \ Damp urpsi but leave residuum of activation.
  1 caller !     \ 13jan2008 Reset caller-ID for safety.
   2 residuum !
  SPEECH     \ Speak or display the lexical "topic".
  midway @  t @  DO  \ Look backwards for 59=do.
    I       0 en{ @  59 = IF  \ If #59 "do" is found,
      59 motjuste !  \ "nen" concept #59 for "do".
      I     6 en{ @  aud !  \ 27aug2008 Recall-vector for "do".
      LEAVE  \ Use the most recent engram of "do".
    THEN  \ End of search for #59 "do".
  -1 +LOOP  \ End of loop finding auxiliary verb "do".

  \ CR ." wtAuxSDo: aud = " aud @ .  \ 31dec2007 Test

  SPEECH    \ Speak or display the auxiliary verb "do".
  fyi @ 2 > IF CR  \ Diagnostic message,
  ."   from whatAuxSDo after speaking of DO, psiDamping concept #59 DO"
  THEN
  59 urpsi !  \ For use in psiDamp to de-activate the "DO" concept.
  42 caller ! \ 13jan2008 whatAuxSDo identified by AI4U page number.
  psiDamp     \  6aug2005 As when verbPhrase has sent a verb to Speech.
  0 caller !  \ 13jan2008 Reset caller-ID for safety.
  \ enDamp    \ de-activate English concepts; 6aug2005: needed here?
  psiDecay    \ Reduce unresolved activation on ignored concepts.
;  \ End of whatAuxSDo; return to ASK.


\ The whatIs module aims for the following entelechy goals.
\ [ ] Use "is" after both user-questions and self-questions.
\ [X] Ask "What is...?" instead of "What do...do?"
\ whatIs is a clone of whatAuxSDo with changes made
\ to ask "WHAT IS" rather than "WHAT DO... DO"
:  whatIs ( what IS Subjects ) \  1sep2008
\  4jan2008 Calls to psiDecay may gradually be commented out.
  psiDecay   \ In the isolated module psiDecay has carte blanche.
  \ Call interrogative pronoun "what":
  midway @  t @  DO  \ Look backwards for 54=what.
    I       0 en{ @  54 = IF  \ If #54 "what" is found,
      54 motjuste !  \ "nen" concept #54 for "what".
      I     6 en{ @  aud !  \ 27aug2008 Recall-vector for "what".
      LEAVE  \ Use the most recent engram of "what".
    THEN  \ End of search for #54 "what".
  -1 +LOOP  \ End of loop finding the word "what".
  SPEECH    \ Speak or display the word "what".
  fyi @ 2 > IF CR  \ Diagnostic message,
\ ."   from whatAuxSDo after speaking of WHAT, psiDamping concept #54"
  ."   from whatIs after speaking of WHAT, psiDamping concept #54"
  THEN
  54 urpsi !  \ For use in psiDamp to de-activate the "WHAT" concept.
  42 caller ! \ 13jan2008 whatAuxSDo identified by AI4U page number.
  psiDamp     \  6aug2005 As when verbPhrase has sent a verb to Speech.
  0 caller !  \ 13jan2008 Reset caller-ID for safety.
  \ Call a form of the auxiliary verb "do":
  \  auxVerb   \  7sep2005 Any of several auxiliary verbs.
  \  1sep2008 Fetch the verb of being "IS".
  midway @  t @  DO  \ Look backwards for 66=IS.
    I       0 en{ @  66 = IF  \ If #66 "IS" is found,
      66 motjuste !  \ "nen" concept #66 for "IS".
      I     6 en{ @  aud !  \  1sep2008 Recall-vector for "IS".
      LEAVE  \ Use the most recent engram of "IS".
    THEN  \ End of search for #66 "IS".
  -1 +LOOP  \ End of loop finding the word "IS".
  SPEECH    \ Speak or display the word "IS".
  fyi @ 2 > IF CR  \ Diagnostic message,
\ ."   from whatAuxSDo after speaking of WHAT, psiDamping concept #54"
  ."   from whatIs after speaking of IS, psiDamping concept #66"
  THEN
  66 urpsi !  \ For use in psiDamp to de-activate the "IS" concept.
\ 42 caller ! \ 13jan2008 whatAuxSDo identified by AI4U page number.
  psiDamp     \  6aug2005 As when verbPhrase has sent a verb to Speech.
  0 caller !  \ 13jan2008 Reset caller-ID for safety.
  0 motjuste !  \ 7sep2005 safety measure
  midway @  t @  DO  \ Look backwards for "topic".
    I       0 en{ @  topic @ = IF  \ If "topic" is found,
      topic @ motjuste !   \ mixing apples & oranges?
      I     6 en{ @ aud ! \ 27aug2008 Auditory recall-vector for "topic".
      LEAVE
    THEN     \ End of search for #"topic".
  -1 +LOOP  \ End of loop finding the lexical "topic" item.
  motjuste @ urpsi !   \ 7sep2005 for sake of psiDamp
  15 residuum !  \ whatAuxSDo module -- 14sep2005 nota bene.
\ 42 caller !    \ 13jan2008 whatAuxSDo identified by AI4U page number.
  psiDamp        \ Damp urpsi but leave residuum of activation.
  1 caller !     \ 13jan2008 Reset caller-ID for safety.
   2 residuum !
  SPEECH     \ Speak or display the lexical "topic".
\ midway @  t @  DO  \ Look backwards for 59=do.
\   I       0 en{ @  59 = IF  \ If #59 "do" is found,
\     59 motjuste !  \ "nen" concept #59 for "do".
\     I     6 en{ @  aud !  \ 27aug2008 Recall-vector for "do".
\     LEAVE  \ Use the most recent engram of "do".
\   THEN  \ End of search for #59 "do".
\ -1 +LOOP  \ End of loop finding auxiliary verb "do".
\ SPEECH    \ Speak or display the auxiliary verb "do".
\ fyi @ 2 > IF CR  \ Diagnostic message,
\ ."   from whatAuxSDo after speaking of DO, psiDamping concept #59 DO"
\ THEN
\ 59 urpsi !  \ For use in psiDamp to de-activate the "DO" concept.
\ 42 caller ! \ 13jan2008 whatAuxSDo identified by AI4U page number.
\ psiDamp     \  6aug2005 As when verbPhrase has sent a verb to Speech.
\ 0 caller !  \ 13jan2008 Reset caller-ID for safety.
  psiDecay    \ Reduce unresolved activation on ignored concepts.
;  \ 1sep2008 End of whatIs; return to ASK.



\ ASK enables the AI to ask a question, query a database,
\ search the Web with a search engine, or swallow ontologies.
:  ASK ( selector of question formats ) \ 11jan2008
   fyi @ 1 > IF       \ 13oct2005 Leave room for Transcript mode.
   \ CR ."     Ask-module calls whatAuxSDo to generate a question."
     CR ."     Ask-module calls a question-module."
   THEN
 \ whatAuxSDo  \  3jan2008 Let "detour" flag call questions.
   whatIs      \  1sep2008 Let "detour" flag call questions.
   0 recon !   \ 4aug2002 Reset the incentive to ask questions.
;  \ End of ASK; return to ENGLISH module.


\  ENGLISH is called by THINK and in turn calls SVO
\  or any other particular English syntax structure.
:  ENGLISH ( one of several possible languages ) \  1sep2008
\ t @ 1 -  tov !  \  Store current "t" as time-of-voice for ".echo".
  t @ tov ! \  1sep2008 Store current "t" as time-of-voice for ".echo".

  fyi @ 2 > IF      \  6jan2008 Test for a high fyi value.
  CR ."   ENGL start: detour @ recon = " detour @ . recon @ . CR
  THEN              \  6jan2008 End of test.

  recon @ 1 = IF    \ 11jan2008 If the flag is set to ask questions.
    fyi @ 2 = IF    \ 11jan2008 Display one choice among several.
      ."     English calls the Ask module. " CR
    THEN            \ 11jan2008 End of test for Tutorial mode
    ASK             \ Artificial curiosity and machine learning
    0 recon !       \ Reset recon-flag after asking a question.
    ELSE            \ If not asking a question, instead do...
    jux @  12 = IF  \ If negative 12=not adverb,
      negSVO        \ transformation of Chomsky;
      ELSE
      fyi @ 2 = IF  \ 30nov2007 For greater Tutorial mode clarity.
      \ ."     English calls SVO syntax structure. " CR
        ."     English starts to think a sentence. " CR \ 29aug2008
      THEN
    \ SVO           \ the "positive" S-V-O syntax.

      \  3sep2008 The AI fills in the next line by generating a thought:
      CR ." Robot: "   \ 3sep2008

      \ 2sep2008 from SVO:  insert a "{" for the sake of Rejuvenate.
      123  t @  2 aud{ ! \  2sep2008 from SVO: 123 is ASCII bracket "{"

      nounPhrase    \ 29aug2008 Making radical changes...

      nphrpos @ 5 = IF  \ 29aug2008 If nounPhrase finds a noun...
        Predicate   \ 29aug2008 Call the modified clone of verbPhrase
        0 nphrpos ! \ 29aug2008 Reset the nounphrase part-of-speech
      THEN  \ 29aug2008 End of test for a foregone conclusion.

    THEN            \ End of test for verb-negation.
  THEN              \ End of test for asking a question.

  recon @ 1 = IF    \  4jan2008 Call ASK and reset "recon".

    fyi @ 2 = IF    \ 11jan2008 Display one choice among several.
      ."   English calls Ask to increase the AI knowledge base."
    THEN            \ 11jan2008 End of test for Tutorial mode

  \ ASK             \  4jan2008 Which calls whatAuxSDo
    ASK             \  1sep2008 Which calls whatIs to emphasize being.
    0 recon !       \  4jan2008 Reset to zero.
    0 detour !      \  1sep2008 Reset to zero.    
  THEN              \  4jan2008 End of flag-check.

  fyi @ 2 > IF      \  6jan2008 Display above a certain fyi level.
  CR ."   ENGL end: detour @ recon = " detour @ . recon @ . CR  \  3jan2008
  THEN              \  6jan2008 End of fyi test.
;  \ 1sep2008 End of ENGLISH; return to the THINK module.


\  THINK is called by the main ALIFE function.
:  THINK ( express spreading activation as thought ) \ 24aug2008
  0 ordo !    \ 3apr2007 For output word-order tracking.
\ 1 inert +!  \ Augment "inert" by one for sake of EGO module.
  35 pov !  \ All thinking has "internal" point-of-view.
\ 10 act !  \ or whatever threshold is to be tested for.

\ 29aug2008 Following obsolete code is being commented out:
\  midway @  t @  DO  \ Examine recent Psi nodes.
\ \ I     4 psi{ @  5 = IF  \ 29aug2005 Look for nouns not verbs.
\   I     5 psi{ @  5 = IF  \ 24aug2008 Look for nouns not verbs.
\   \ I   1 psi{ @  3 > IF  \ 15oct2005 For chains of thought.
\     I   1 psi{ @  2 > IF  \  1apr2007 For chains of thought.
\       fyi @ 2 > IF CR     \ Diagnostic message...
\         ." Think module has found active concept; calling English."
\       THEN     \ 29aug2005 Perhaps identify the found noun.
\       fyi @ 2 = IF  \ 30nov2007 For clarity in Tutorial mode.
\         ."   Think calls English (or some other language in future)." CR
\       THEN
\       ENGLISH  \ 29aug2005 Making sure to call English.
\       LEAVE    \ 29aug2005 Finding one active noun is enough.
\     THEN  \ End of check for the activity of a found concept.
\   THEN      \ End of test for active opt=5 nouns.
\ -1  +LOOP  \ End of backwards loop seeking high activations.

  ENGLISH  \ 29aug2008 Let ENGLISH lead to most active concept.


  fyi @ 1 = IF CR THEN  \ Transcript-mode space between thoughts.
  0 recon ! \ Give ENGLISH one chance per cycle to call Ask.
  0 ordo !  \ 2apr2007 from JSAI; reset of the word-counter variable.
;  \ End of THINK; return to the main ALIFE loop.


\ MOTORIUM is a stub where you may insert robot motor code.
:  MOTORIUM ( stub for volitional control of actuators in robots )
   7 EMIT       \ The only power of the AI is to ring a bell.
 ( MOVE_FORWARD   ) \ See ACM SIGPLAN Notices 33(12):25-31 of
 ( MOVE_BACKWARDS ) \ December 1998 for a paper by Paul Frenger,
 ( STOP_MOTION    ) \ "Mind.Forth: Thoughts on Artificial
 ( TURN_LEFT      ) \ Intelligence and Forth" for discussion
 ( TURN_RIGHT     ) \ of the Mind.Forth MOTORIUM on page 26.
;  \ end of MOTORIUM stub; return to ALIFE when implemented.


\ HCI is the human-computer interface of the Robot AI Mind.
:  HCI ( Human-Computer Interaction )  \  1feb2008
  \ CLS  (  Clear the screen to prevent scrolling.  )
  fyi @ 0 = IF CLS CR CR CR CR CR CR CR
    t @ 300 < IF CR  \ Show warranty disclaimer during early start-up.
 ." There is no warranty with Mind.Forth AI Engine for robots."
    ELSE CR
    THEN    \ 14oct2005 End of test to stop showing disclaimer.
  THEN      \ 14oct2005 End of conditional test for Normal display mode.
  fyi @ 1 = NOT IF CR THEN  \ If not Transcript mode, then carriage return.
  ."  "  \  A kind of buffer before all the backspacing. 
  fyi @ 1 = NOT IF CR THEN  \ 13oct2005 Tighten up for Transcript mode.
  fyi @ 0 = IF    \ 12sep2005 For Normal display mode.
    CR ." Artificial intelligence -- alive and thinking since "
    bday @ .
        bmonth @  1 = IF ." January "   THEN
    bmonth @  2 = IF ." February "  THEN
    bmonth @  3 = IF ." March "     THEN
    bmonth @  4 = IF ." April "     THEN
    bmonth @  5 = IF ." May "       THEN
    bmonth @  6 = IF ." June "      THEN
    bmonth @  7 = IF ." July "      THEN
    bmonth @  8 = IF ." August "    THEN
    bmonth @  9 = IF ." September " THEN
    bmonth @ 10 = IF ." October "   THEN
    bmonth @ 11 = IF ." November "  THEN
    bmonth @ 12 = IF ." December "  THEN
    byear @ . 8 EMIT 46 EMIT CR  \ Backspace + period & carriage-return.
  THEN   \ 5sep2005 End of code to tell AI age in Normal mode.
\ fyi @ 2 = IF CR  \ Enhancing the Tutorial display.
\ ." HCI is the Human-Computer Interface. Study the Mind.Forth User Manual."
\   CR
\ THEN
  fyi @ 1 = NOT IF  \ 13oct2005 Omit display only during Transcript mode.
    ." TAB mode. ESCAPE exit. IQ = " IQ @ . \ IQ needs implementation.
    8 EMIT 46 EMIT  \ Backspace, then display a period.
    ."  Time = " t @ . 8 EMIT 46 EMIT  \ Backspace plus period.
    ."  Cyc = " rjc @ . ." since "  bday @ .
    bmonth @  1 = IF ." January "   THEN
    bmonth @  2 = IF ." February "  THEN
    bmonth @  3 = IF ." March "     THEN
    bmonth @  4 = IF ." April "     THEN
    bmonth @  5 = IF ." May "       THEN
    bmonth @  6 = IF ." June "      THEN
    bmonth @  7 = IF ." July "      THEN
    bmonth @  8 = IF ." August "    THEN
    bmonth @  9 = IF ." September " THEN
    bmonth @ 10 = IF ." October "   THEN
    bmonth @ 11 = IF ." November "  THEN
    bmonth @ 12 = IF ." December "  THEN
    byear @ . 8 EMIT 46 EMIT CR  \ Backspace + period & carriage-return.
 ." ENTER a positive or negative Subj-Verb-Obj unpunctuated sentence."
    CR  \ 29jan2008 For a gap before further display begins.
  THEN              \ End of test for Transcript display mode.
  fyi @ 0 = IF CR
 ." Normal display-mode is in effect. Press Tab key for other modes."
    CR
  THEN
\ fyi @ 2 = IF CR    \ 13oct2005 Leave room for Transcript display.
\ ." Tutorial mode is now in effect. Press Tab for other display modes."
\   CR
\ THEN
  fyi @ 3 = IF CR    \ 13oct2005 Leave room for Transcript display.
  ." Diagnostic messages - ignore during input before pressing ENTER."
  THEN
  \ 26jul2002 We set point-of-view "pov" to 42=external
  \ so that SENSORIUM calls AUDITION which calls LISTEN:
  42 pov !  \ 26jul2002 So that LISTEN will be called.
; \ End of HCI; return to the SECURITY safeguards module.


\ SECURITY is a module for potential safeguards as the seed AI
\ evolves into the artificial intelligence of transhumanism in
\ a cyborg or a supercomputer endowed with artificial life.
:  SECURITY ( safety for robots and human beings ) \ 29jan2008
  fyi @ 2 = IF CR  \ 30nov2007 For greater clarity in Tutorial mode.
  \ CR  \ 24jan2008 For a gap to indicate start of a new cycle.
  ."   Security module calls Human-Computer Interface (HCI)."
  THEN
  HCI  \ Call the human-computer interaction (HCI) module. 
  t @ cns @ 64 - > IF  \ When the CNS is almost full,

    fyi @ 2 = IF CR  \ 3dec2007 For greater clarity in Tutorial mode.
  \ CLS \ Comment out this Clear-Screen if prior data must be seen.
    ."   Security module calls Rejuvenate."
    THEN        \ 3dec2007 This message was previously in Rejuvenate.

    REJUVENATE  \ move bulk of memories backwards and
  THEN          \ forget the oldest unused memories.
  t   @  1024 > IF  \ Use midway only for larger AI Minds.
    t @  1024 -  midway !  ( for a range limit on searches )
    ELSE            \ If the CNS memory has a small capacity
  \ 0   midway !    \ currently search the entire CNS space.
    1   midway !    \ 7apr2007 Avoid any "array boundary problem."
  THEN   \ Future code may let the AI itself set the midway.
\ inert @ 33 > IF  \ 23dec2007 Call EGO more frequently to change topics.
\   EGO        \ Call EGO if THINK has not been calling ENGLISH.
\   0 inert !  \ ENGLISH shall zero out the "inert" variable.
\ THEN  \  Assert self.
  0 quiet !  \ Give Listen & Audition a chance for user input.
;  \ End of SECURITY; return to the main ALIFE loop.


\ ALIFE is the main program loop.that starts the AI running.
:  ALIFE ( artificial Life ) \ atm 2dec2007
  TIME&DATE byear ! bmonth ! bday ! bhour ! bminute ! bsecond !
  TABULARASA   \ Clears a "clean slate" of memory arrays.
  enBoot       \ English bootstrap loads initial concepts.
  BEGIN        \ Start the main program mind loop running.
    SECURITY   \ For human control and operation of the AI.
    fyi @ 2 = IF CR  \ 2dec2007 For clarity in Tutorial mode.
    ." Alife main loop calls the Sensorium mind-module." CR
    THEN          \ End of test for Tutorial display mode.
    SENSORIUM  \ Audition; other human-robot input senses.
  ( EMOTION )  \ Quasi-physiological influence upon thought.
    fyi @ 2 = IF CR  \ 30nov2007 For clarity in Tutorial mode.
    ." Alife main loop calls the Think mind-module." CR
    THEN          \ End of test for Tutorial display mode.
    THINK      \ Syntax and vocabulary of natural languages.
  ( VOLITION ) \ Contemplative selection of motor options.
  ( MOTORIUM ) \ Robotic activation of motor initiatives.
  AGAIN        \ Repeat if the AI has not met with misadventure.
; \ End of main module called when user types:  ALIFE [RETURN]. 
  