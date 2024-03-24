# Install slippery chicken

This document describes the installation process of slippery chicken using the
[ASDF](https://asdf.common-lisp.dev) build system, which is -- as of March 2024
-- the recommended method for this process. Other installation methods are
documented on this [wiki page](https://github.com/mdedwards/slippery-chicken/wiki/how-to-install-slippery-chicken-'by-hand')[^5].


- [Prerequisites and Preparations](#prerequisites-and-preparations)
  - [ASDF](#asdf)
  - [Quicklisp](#quicklisp-optional)
- [Dependencies](#dependencies)
  - [Common Music (required)](#common-music-required)
  - [Common Music Notation](#common-music-notation)
  - [Common Lisp Music](#common-lisp-music)
    - [C Compiler](#c-compiler)
  - [ffprobe](#ffprobe)
  - [LilyPond](#lilypond)
- [Install and Use](#install-and-use)
- [Customise Global Options](#customise-global-options)


## Prerequisites and Preparations

Although slippery chicken's dependency overhead is rather low and installing the
software via ASDF is quite straightforward, a few remarks on its prerequisites
are worth to be made. Generally, this document assumes that you have a working
Common Lisp implementation running on your system as well as that you have a
basic understanding of the Common Lisp language.

### ASDF

Most current Common Lisp implementations already include the ASDF-package by
default,[^1] thus it is very likely that you don't need to take care of setting
ASDF up before going ahead. You can find out whether ASDF is available on your
system by evaluating this form in your CL-REPL...

```lisp
(find :asdf *features*)
```

...which should return `:ASDF`.

In ASDF-parlance, modules or programs (e.g. slippery-chicken) are termed
*systems*. Each system contains a system definition file (the `.asd` file) which
provides information about dependencies, system components etc. In case you 
encounter ASDF-related errors, inspecting the `.asd` file of the respective 
system is always a good strategy. 

> [!IMPORTANT] 
> If you are not familiar with ASDF, it is worth mentioning that all systems
> need to be present in the ASDF-search-path/registry. A simple and
> straightforward way to do this is to place the respective files to ASDF's
> standard location (`~/common-lisp/~). You could also use symlinks, of
> course. For more detail
> cf. [https://asdf.common-lisp.dev/asdf.html#Configuring-ASDF-to-find-your-systems](https://asdf.common-lisp.dev/asdf.html#Configuring-ASDF-to-find-your-systems).


### Quicklisp (optional)

As some modules (i.e. dependencies) of slippery chicken themselves depend on
modules probably not present on your system and ASDF does not take care about
downloading them,[^2] you might want to use Quicklisp, a Common Lisp library
manager built upon ASDF, to dynamically (down-)load the respective
components. In order to do so, head over to
[https://www.quicklisp.org/](https://www.quicklisp.org/) and follow the
instructions given there.

> [!NOTE]
> Currently (March 2024), slippery chicken itself and some of its major
> dependencies (e.g. CM, see below) are not part of the Quicklisp-distribution
> and have to be downloaded manually (as described below).

>[!IMPORTANT]
> When using Quicklisp, the default search-path (cf. [ASDF](#asdf)) for local
> projects might change to `~/quicklisp/local-projects/~`. 


## Dependencies

One major dependency of slippery chicken is *Common Music* (CM). Without CM, 
slippery chicken will not work. Additionally, slippery chicken's functionality
will be greatly enhanced when further modules are available. A fully fledged 
slippery chicken requires the following modules:

- Common Music (required)
- Common Music Notation (CMN)
- Common Lisp Music (CLM)
- ffprobe
- LilyPond


### Common Music (required)

As mentioned before, Rick Taube's Common Music (CM) needs to be available for
slippery chicken to work. As slippery chicken is written in Common Lisp (CL), we
need a CL version of CM, which is provided by Orm Finnendahl. Just clone[^3]
[https://github.com/ormf/cm](https://github.com/ormf/cm) to your
ASDF-/quicklisp-directory (cf. [ASDF](#asdf)).


### Common Music Notation

Rick Taube's Common Music Notation (CMN) is capable of generating musical scores
from CL and required for example by slippery-chicken's `cmn-display`[^4]
method. If you want to use it, download the tarball archive from
[https://ccrma.stanford.edu/software/cmn/](https://ccrma.stanford.edu/software/cmn/)
and unpack(-tar) it to your ASDF-/quicklisp-directory (cf. [ASDF](#asdf)).

### Common Lisp Music

Bill Schottstaedt's Common Lisp Music (CLM) is used in slippery chicken to write
and (quickly) analyse soundfiles. In order to use it, download the tarball
archive from
[https://ccrma.stanford.edu/software/clm/](https://ccrma.stanford.edu/software/clm/)
and unpack(-tar) it to your ASDF-/quicklisp-directory (cf. [ASDF](#asdf)).

#### C Compiler

CLM requires a C compiler to be present on your system. On most UNIX systems
(including Apple's OSX) you'll already have the GNU C compiler. You can check
whether yours is ready to go by typing the following command in your terminal:

```
gcc -v
```

You should get some version details containing something like this:

```
Configured with: --prefix=/Library/Developer/CommandLineTools/usr --with-gxx-include-dir=/usr/include/c++/4.2.1
```

If you don't have a C compiler, installation methods vary from OS to OS:

- macOS
  - cf. this [wiki entry](https://github.com/mdedwards/slippery-chicken/wiki/how-to-install-slippery-chicken-'by-hand'#c-compiler)
- Linux
  - should have a C compiler installed; if not, friendly ask your preferred
    search engine
- Windows
  - this is a complicated story. 
  

### ffprobe

ffprobe is used by slippery chicken to retrieve data from media files
(e.g. video and audio files). When CLM is not installed, slippery chicken uses
ffprobe's (slightly slower, compared to CLM) capabilities for analysing
sndfiles. In order to install ffprobe, head to this site and install ffprobe
according to the directions given there:
[https://ffmpeg.org/download.html](https://ffmpeg.org/download.html).

You can, of course, use `apt` on Linux or `brew` on MacOS.

> [!NOTE] 
> ffprobe is not a Common Lisp module, thus all ASDF-/Quicklisp-related
> comments made above do not apply to this program.

### LilyPond

If you want to take advantage of rendering beautiful
[LilyPond](https://lilypond.org) scores (e.g. via slippery-chicken's
`lp-display`), you might be interested in installing this software to your
system. For further information, head here: [LilyPond](https://lilypond.org).

You can, of course, use `apt` on Linux or `brew` on MacOS.

> [!NOTE] 
> lilypond is not a Common Lisp module, thus all ASDF-/Quicklisp-related
> comments made above do not apply to this program.


## Install and Use

Installing slippery chicken, after fulfilling the prerequisites and (desired)
dependencies, is really easy. Just clone[^3]
[http://github.com/mdedwards/slippery-chicken](http://github.com/mdedwards/slippery-chicken)
to your ASDF-/quicklisp-directory (cf. [ASDF](#asdf)).

Then, you need to take care of loading the modules (cf. [ASDF](#asdf)), before
starting to work with slippery chicken, in appropriate order. It is recommended
to do this via the initfile of your Common Lisp implementation. 

If you are using the full version of slippery chicken
(cf. [Dependencies](#dependencies)), this exemplary (portion of an) initfile for
SBCL (`.sbclrc`) might provide some insight:

```lisp
;; ...
(asdf:load-system :cmn)
(asdf:load-system :cm)     ; this might require additional packages (e.g. 
                             alexandria to be loaded in advance)
;; when using quicklisp (cf. previous comment), this might facilitate things:
;; (ql:quickload :cm)
(asdf:load-system :clm)
(asdf:load-system :slippery-chicken)

```

Now, you should be able to work with slippery chicken.


## Customise Global Options

slippery chicken contains some global variables which could and should be
modified to the user's desired. For a detailed insight, have a look at
`src/globals.lsp`. Though, especially if you use lilypond, ffprobe, or csound,
it might be useful to the global variables to the actual path to the actual
binaries on your system. The following is an example for a "fully-fledged"
system installed via [homebrew[(https://brew.sh) (on macOS), appended to the
initfile (e.g. `.sbcl`) appended to the aforementioned. 

```lisp
(set-sc-config 'lilypond-command "/opt/homebrew/bin/lilypond")
(set-sc-config 'ffprobe-command "/opt/homebrew/bin/ffprobe")
(set-sc-config 'csound-command "/opt/homebrew/bin/csound")
```

---


[^1]:
    Cf. [https://asdf.common-lisp.dev/#implementations](https://asdf.common-lisp.dev/#implementations)

[^2]:
    Cf. [https://asdf.common-lisp.dev/#what_it_is_not](https://asdf.common-lisp.dev/#what_it_is_not)

[^3]: This is git-parlance:
    [https://git-scm.com/docs/git-clone](https://git-scm.com/docs/git-clone)

[^4]:
    Cf. [file:///Users/rubenphilipp/lisp/slippery-chicken/doc/manual/output.html#cmn](file:///Users/rubenphilipp/lisp/slippery-chicken/doc/manual/output.html#cmn)


[^5]: Written by Ruben Philipp (<me@rubenphilipp.com>).
