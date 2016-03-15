# haskomp

## To install
You need haskell and cabal, and be on linux with snd-seq (alsa)
> cabal install -j

## To run 
> ~/.cabal/bin/haskomp

## What you get

One input midi port in jack, where the program listens to notes and 3 controls.

One output midi port playing at 125 bpm a loop of 16 lines.

## Notes
Note on events by-pitch-select a phrase clone and open its valve proportional to its velocity (0 .. 1) of the transposed to key phrase. The selection is persistent for controls until next noteon or noteoff. 

Note off close the relative valve and set the selection to none.


## Controls

Ctrl 1 is checked for 0 value events to random sample a new set of events 

Ctrl 11 is sampled to control the phrase time shift (0 .. 1) of the last noteon key

Ctrl 12 is sampled to control the phrase time stretch (1 .. 2)

