%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% File:             lilypond.ly
%%%
%%% Class Hierarchy:  none (no classes defined)
%%%
%%% Project:          slippery chicken (algorithmic composition)
%%%
%%% Purpose:          Definition of some markup commands for inclusion in
%%%                   Lilypond scores. 
%%%
%%% Author:           Michael Edwards: m@michael-edwards.org
%%%
%%% Creation date:    23rd November 2011
%%%
%%% $$ Last modified: 16:51:40 Wed Apr 18 2012 BST
%%%
%%% SVN ID: $Id: lilypond.ly 963 2010-04-08 20:58:32Z medward2 $
%%%
%%% ****
%%% Licence:          Copyright (c) 2010 Michael Edwards
%%%
%%%                   This file is part of slippery-chicken
%%%
%%%                   slippery-chicken is free software% you can redistribute it
%%%                   and/or modify it under the terms of the GNU General
%%%                   Public License as published by the Free Software
%%%                   Foundation% either version 2 of the License, or (at your
%%%                   option) any later version.
%%%
%%%                   slippery-chicken is distributed in the hope that it will
%%%                   be useful, but WITHOUT ANY WARRANTY% without even the
%%%                   implied warranty of MERCHANTABILITY or FITNESS FOR A
%%%                   PARTICULAR PURPOSE.  See the GNU General Public License
%%%                   for more details.
%%%
%%%                   You should have received a copy of the GNU General Public
%%%                   License along with slippery-chicken% if not, write to the
%%%                   Free Software Foundation, Inc., 59 Temple Place, Suite
%%%                   330, Boston, MA 02111-1307 USA
%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% although this is basically scheme, we're including this file in a lilypond
% file so the comment char is % not %
% The now standard Bartok pizzicato, somehow not included in lilypond.
#(define-markup-command (snappizz layout props) () 
 (interpret-markup layout props 
  (markup #:stencil  
   (ly:stencil-translate-axis  
    (ly:stencil-add  
     (make-circle-stencil 0.7 0.1 #f)  
     (ly:make-stencil  
      (list 'draw-line 0.1 0 0.1 0 1)  
      '(-0.1 . 0.1) '(0.1 . 1)))  
    0.7 X))))  
snapPizzicato = \markup \snappizz 

% N.B. all of the new commands which include PostScript files require that
% the graphics files are in same directory as the lilypond file you're
% processing.  When creating EPS files, make sure to embed fonts.

% For flute and other instruments, a small circle like a harmonic, but
% lightly filled, the implication being a very breathy note, but with some
% tone. 
#(define-markup-command (aeolianLight layout props) () 
 (interpret-markup layout props 
        (markup #:general-align Y DOWN  #:epsfile X 1.5 "aeolian-light.eps")))
aeolianLight = \markup \aeolianLight



% Similar to the above but a darkly filled circle, implying more tone.
#(define-markup-command (aeolianDark layout props) () 
 (interpret-markup layout props 
        (markup #:general-align Y DOWN  #:epsfile X 1.5 "aeolian-dark.eps")))
aeolianDark = \markup \aeolianDark

% an S with wavy lines to indicate singing whilst playing a wind instrument
#(define-markup-command (sing layout props) () 
 (interpret-markup layout props 
        (markup #:general-align Y DOWN  #:epsfile X 5 "sing.eps")))
sing = \markup \sing

% same as above but with an arrow pointing east
#(define-markup-command (singArr layout props) () 
 (interpret-markup layout props 
        (markup #:general-align Y DOWN  #:epsfile X 7 "sing-arrow.eps")))
singArr = \markup \singArr

% an M with wavy lines to indicate a multiphonic
#(define-markup-command (mphonic layout props) () 
 (interpret-markup layout props 
        (markup #:general-align Y DOWN  #:epsfile X 5 "multiphonic.eps")))
mphonic = \markup \mphonic

% variations
#(define-markup-command (mphonicCluster layout props) () 
 (interpret-markup layout props 
        (markup #:general-align Y DOWN  #:epsfile X 5
                "multiphonic-cluster.eps")))
mphonicCluster = \markup \mphonicCluster

#(define-markup-command (mphonicCons layout props) () 
 (interpret-markup layout props 
        (markup #:general-align Y DOWN  #:epsfile X 5
                "multiphonic-consonant.eps")))
mphonicCons = \markup \mphonicCons

#(define-markup-command (mphonicDiss layout props) () 
 (interpret-markup layout props 
        (markup #:general-align Y DOWN  #:epsfile X 5
                "multiphonic-dissonant.eps")))
mphonicDiss = \markup \mphonicDiss

% same as above but with an arrow pointing east
#(define-markup-command (mphonicArr layout props) () 
 (interpret-markup layout props 
        (markup #:general-align Y DOWN  #:epsfile X 7 "multiphonic-arrow.eps")))
mphonicArr = \markup \mphonicArr

% end of an arrow: bracket for closing techniques
#(define-markup-command (bracketEnd layout props) () 
 (interpret-markup layout props 
        (markup #:general-align Y DOWN  #:epsfile X 5 "bracket-end.eps")))
bracketEnd = \markup \bracketEnd

% one arrow up and one down: meant to indicate bowing up and down string
% instead of across 
#(define-markup-command (arrowUpDown layout props) () 
 (interpret-markup layout props 
        (markup #:general-align Y DOWN  #:epsfile X 2 "up-down.eps")))
arrowUpDown = \markup \arrowUpDown

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EOF lilypond.ly
