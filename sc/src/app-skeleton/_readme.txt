************************
*** SLIPPERY CHICKEN ***
************************

This is the slippery-chicken app.  It should run on Mac OSX 10.6.x and 10.7.x

It bundles slippery chicken (including CM 2.6.0) with CLM-4 and CMN.  Similar
to (and liberally copied from ;) Rick Taube's old CM.app, it merely
fires up your Emacs application with slime and SBCL and loads all the
packages.  The first time it runs it might take some time to compile everything
but from then on it should be much faster.

Prerequisites: 
* Emacs (either /Applications/Emacs.app or /Applications/Aquamacs.app) 
* The GNU C compiler (gcc), which comes with Apple's developer tools

Known issues: 
* If the app is in a folder whose name (or parents' names) has
  spaces it might just hang forever.
* SBCL spits out loads or (really unhelpful) warnings--just ignore them.
* If you're already an Emacs+slime user and have code in your .emacs file which
  loads slime or defines an *inferior-lisp* image, or a ~/.swank.lisp slime
  init file, or perhaps even a ~/.sbclrc init file for SBCL, then the app may
  just fail as it tries to handshake your init code with its own.  But then it
  sounds like you know what you're doing anyway so get the code base from
  http://www.michael-edwards.org/sc/source.html and hand load it as per the
  installation instructions.

If there are any problems, please post the error messages to
http://groups.google.com/group/slippery-chicken 

Michael Edwards
m@michael-edwards.org
12th June 2012
