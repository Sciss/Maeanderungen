# Mäanderungen

[![Build Status](https://travis-ci.org/Sciss/Maeanderungen.svg?branch=master)](https://travis-ci.org/Sciss/Maeanderungen)

## statement

Software for sound experiments. (C)opyright 2017&ndash;2018 by Hanns Holger Rutz. All rights reserved. This project is released under the
[GNU Affero General Public License](http://github.com/Sciss/Maeanderungen/blob/master/LICENSE) v3+ and comes with absolutely no warranties.
To contact the author, send an email to `contact at sciss.de`.

## building

Builds with sbt against Scala 2.12.

## context

This project was first used in our preliminary radio experiment __Raumaneignungen__ (Nayari Castillo, 
Reni Hofmüller, Miriam Raggam, Hanns Holger Rutz). In 2018, we plan to create a radio piece
_generator_, and the code for this will likely be found in this repository as well.
For 'Raumaneignungen', it contains code to translate photographs of cracks in the pavement
to synthetic sounds, which was used as one layer in the piece, as well as an algorithm to
distribute whispered text fragments.

'Raumaneignungen' was part of the Urban Challenges series, and it will eventually be available
from [this archive](https://cba.fro.at/series/urban-challenges).

The repository is now used as a basis for developing the _Mäanderungen_ radio project.
_Mäanderungen_ is a project by Nayarí Castillo, Reni Hofmüller, Hanns Holger Rutz, and Miriam Raggam.
It won the 3rd lime\_lab prize - laboratory for the development of experimental media and 
boundary crossing radio play - awarded by the Federal Chancellery of Austria (BKA).

## generator

- `Preparation`: expects a workspace with top-level folder `material`

Run args

    -w /data/projects/Maeanderungen/workspaces/Maeanderungen180921-octo-1.mllt -b /data/projects/Maeanderungen --min-dur 900 --max-dur 900 --backup --text-sound-ratio 1.2 --iterations 50 --num-channels 8 --no-prepare
