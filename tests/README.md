# slippery-chicken-RTs
Regression test routines for checking the functionality of the slippery-chicken algorithmic composition software

Once you've cloned/downloaded this test-suite, the directory the files live in will be called 'slippery-chicken-RTs'. Please change this directory name to 'test-suite' otherwise the tests will fail.

There are two main files that can be loaded into Lisp once it has been freshly started and slippery-chicken loaded: sc-test-suite.lsp and sc-test-full.lsp. Be sure not to run one after the other, in either direction, unless you've restarted Lisp.

sc-test-suite.lsp is the workhorse. I run it regularly whilst developing. It's pretty quick and pretty thorough but doesn't try complete, complex pieces. That's where sc-test-full.lsp comes into play. This loads lots of legacy pieces with all kinds of complexities and dusty corners, as well as code from workshops, web pages, etc. It takes a lot longer to run, hence I don't do it as often.

sc-test-full.lsp assumes that you've got the full documentation (downloaded from http://michael-edwards.org/sc/source.html) and that the doc and test-suite directories are at the same level as src i.e. wherever you've got slippery chicken you'll have the following directories all in the same folder: src, bin, doc, test-suite (probably slippery-chicken.wiki also). 

Loading either test file defines many test functions via the sc-deftest macro. This not only creates the function but also ensures that it's called (tested) automatically. So if you want to add new tests, follow one of the many sc-deftest examples from sc-test-suite.lsp in order to add your own to that file, then save and load and all should be well.

If any of the tests fail, you'll get an error from Lisp and probably pop into the debugger so you can see where the problem lies. I also scan the output of the test file for any lines beginning with "FAIL: "

Kudos and thanks to Sean Reed for getting the bulk of the unit test infrastructure up and running back in 2011/12
