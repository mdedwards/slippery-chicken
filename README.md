# slippery-chicken-RTs
Regression test routines for checking the functionality of the slippery-chicken algorithmic composition software

There are two main files that can be loaded into Lisp once it has been freshly started and slippery-chicken loaded: sc-test-suite.lsp and sc-test-full.lsp. Be sure not to run one after the other, in either direction, unless you've restarted Lisp.

The latter assumes that you've got the full documentation (downloaded from http://michael-edwards.org/sc/source.html) and that the doc and test-suite directories are at the same level as src i.e wherever you've got slippery chicken you'll have the following directories all in the same folder: src, bin, doc, test-suite (probably slippery-chicken.wiki also). (The test-suite directory will be called slippery-chicken-RTs unless you change the directory name to test-suite after cloning from git, which I do.)

Loading either test file defines many test functions via the sc-deftest macro. This not only defines the function but ensures that it's called (tested) too. So if you want to add new tests, follow one of the many sc-deftest examples from sc-test-suite.lsp in order to add your own to that file, then save and load and all should be well.

Kudos and thanks to Sean Reed for getting the bulk of the unit test infrastructure up and running back in 2011/12
