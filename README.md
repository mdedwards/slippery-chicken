# slippery-chicken-RTs
regression test routines for checking the functionality of the slippery-chicken algorithmic composition software

There are two main files that can be loaded into Lisp once it has been freshly started and slippery-chicken loaded: sc-test-suite.lsp and sc-test-full.lsp. Be sure not to run one after the other, in either direction, unless you've restarted Lisp.

The latter assumes that you've got the full documentation (downloaded from http://michael-edwards.org/sc/source.html) and that the doc and test-suite directories are at the same level as src i.e wherever you've got slippery chicken you'll have the following directories all in the same folder: src, bin, doc, test-suite (probably slippery-chicken.wiki also)

Kudos and thanks to Sean Reed for getting the bulk of the unit test infrastructure up and running back in 2011/12
