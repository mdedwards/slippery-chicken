                       ************************
                       *** SLIPPERY CHICKEN ***
                       ************************

This is slippery-chicken, a Common Lisp and CLOS package for
algorithmic composition.  It should run on Mac OSX 10.6 and above.
Documentation and other resources are available online at
http://www.michael-edwards.org/sc

This project bundles slippery chicken (including CM 2.6.0) with CLM-4
and CMN. Similar to (and liberally copied from ;) Rick Taube's old
CM.app, this app merely fires up your Emacs application with slime and
SBCL and loads all the packages. The first time it runs it might take
some time to compile everything but from then on it should be much
faster.

Prerequisites: 
- Emacs: either /Applications/Emacs.app
  (http://www.gnu.org/software/emacs/) or /Applications/Aquamacs.app
  (http://aquamacs.org/) I think Aquamacs behaves most like a native
  OSX text editor, so if you've never used Emacs before start with
  this.
- The GNU C compiler (gcc), which comes free with Apple's developer
  tools (https://developer.apple.com/xcode/). Install XCode via the
  App Store. Once you have installed it you'll need to download and
  install the XCode Command Line Tools from the Downloads Tab of
  XCode's preferences. You can then verify that gcc works by opening
  the terminal app and typing gcc -v (whereupon you should see version
  number information rather than "Command not found").
  - alternatively on OSX 10.9 and above you can install command line
    tools without XCode if you prefer; type the following in the
    terminal application: xcode-select --install
    http://osxdaily.com/2014/02/12/install-command-line-tools-mac-os-x/

Known issues: 

- If the app is in a folder whose name (or parents' names) has spaces
  it might just hang forever.
- SBCL spits out loads of (really unhelpful) warnings--just ignore
  them.
- If you're already an Emacs+slime user and have code in your .emacs
  file which loads slime or defines an -inferior-lisp- image, or a
  ~/.swank.lisp slime init file, or perhaps even a ~/.sbclrc init file
  for SBCL, then the app may just fail as it tries to handshake your
  init code with its own. But then it sounds like you know what
  you're doing anyway so get the code base from
  http://www.michael-edwards.org/sc/source.html and hand load it as
  per the installation instructions given there.

If there are any problems, please post the error messages to
http://groups.google.com/group/slippery-chicken

Michael Edwards
m@michael-edwards.org

_____________________________________________________________________

This file is part of slippery-chicken. slippery-chicken is free
software; you can redistribute it and/or modify it under the terms of
the GNU General Public License as published by the Free Software
Foundation; either version 3 of the License, or (at your option) any
later version.

slippery-chicken is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with slippery-chicken; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
USA
