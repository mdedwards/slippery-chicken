# Installation Instructions

## Ultrashort: for the quicklisp afficionado (TL;DR)

For a minimum installation of slippery chicken, open your terminal, `cd` to your
Quicklisp local-projects directory, then:

```shell
# in your Quicklisp standard directory
git clone https://github.com/ormf/cm.git
git clone https://github.com/mdedwards/slippery-chicken.git
```

In your Lisp REPL or your Lisp initialisation file:

```lisp
(ql:quickload :slippery-chicken)
```

(If none of the above made any sense, or you want extra package functionality
such as CLM, then keep reading.)

---

This document describes the installation process of slippery chicken using
the [ASDF](https://asdf.common-lisp.dev) build system, which is – as of March
2024 – the recommended method for this process. 

You will find two versions of this description: A [short](#short) one for those
familiar with Common Lisp, ASDF and the like, and a [long](#long) version more
aimed at those who are new to the Lisp world or prefer to follow more verbose
instructions.

- [Short](#short)
  - [Install Dependencies](#install-dependencies)
  - [Install slippery-chicken](#install-slippery-chicken)
  - [Configure slippery-chicken](#configure-slippery-chicken)
- [Long](#long)
  - [Prerequisites and Preparations](#prerequisites-and-preparations)
    - [ASDF](#asdf)
    - [Quicklisp (recommended)](#quicklisp-recommended)
  - [Install Dependencies](#install-dependencies-1)
    - [Common Music (required)](#common-music-required-1)
    - [Common Music Notation (optional)](#common-music-notation-optional-1)
    - [Common Lisp Music (optional)](#common-lisp-music-optional-1)
      - [C Compiler](#c-compiler)
    - [ffprobe (optional)](#ffprobe-optional-1)
    - [LilyPond (optional)](#lilypond-optional-1)
  - [Install slippery-chicken](#install-slippery-chicken-1)
  - [Configure slippery-chicken](#configure-slippery-chicken-1)
  - [Customise Global Options](#customise-global-options)

## Short

Before going ahead, make sure you have a running Common Lisp implementation on
your system. We work with [SBCL](http://www.sbcl.org). Also make sure that 
[ASDF](https://asdf.common-lisp.dev) is installed.

We recommend working with [Quicklisp](https://www.quicklisp.org/) as it automatically
downloads packages it finds on its servers and thus makes the installation process
simpler.


### Install Dependencies

First of all, install slippery chicken's dependencies. These are:

- Common Music/CM (required)
- Common Music Notation/CMN (optional)
- Common Lisp Music/CLM (optional)
- ffprobe (optional)
- LilyPond (optional)

Without the optional dependencies, you will only be able to use a reduced
feature set, such as writing MIDI or Music-XML files.

> [!NOTE]
> The Common Lisp dependencies (CM, CMN and CLM) need to be available to the
> ASDF's/Quicklisp's search path. We suggest putting the code in one of the standard
> locations:
>
> **ASDF:** `~/common-lisp/` or `~/.local/share/common-lisp/source/`<br>
> **Quicklisp:** `~/quicklisp/local-projects/`

Let's begin:


#### Common Music (required)

Download the CM-sources from
[https://github.com/ormf/cm](https://github.com/ormf/cm) and put them in your
ASDF/Quicklisp directory, or if you're comfortable working with a (UNIX) terminal:

```shell
# in your ASDF/Quicklisp standard directory
git clone https://github.com/ormf/cm.git
```


#### Common Music Notation (optional)

Download the CMN-sources from
[https://ccrma.stanford.edu/software/cmn/](https://ccrma.stanford.edu/software/cmn/)
and unpack them to your ASDF/Quicklisp directory, or:

```shell
# in your ASDF/Quicklisp standard directory
curl https://ccrma.stanford.edu/software/cmn/cmn.tar.gz -o cmn.tar.gz
tar xf cmn.tar.gz
```


#### Common Lisp Music (optional)

Download the CLM-sources from
[https://ccrma.stanford.edu/software/clm/](https://ccrma.stanford.edu/software/clm/)
and unpack them to your ASDF/Quicklisp directory, or:

```shell
# in your ASDF/Quicklisp standard directory
curl https://ccrma.stanford.edu/software/clm/clm-5.tar.gz -o clm.tar.gz
tar xf clm.tar.gz
```

> [!IMPORTANT]
> CLM requires a C compiler to be present on your system. If you don't have a
> C compiler or are not sure, jump to [this](#c-compiler) section. 

#### ffprobe (optional)

You can use `apt` on Linux, `brew` on MacOS or `choco` on Windows: 

```shell
# Linux
apt get ffmpeg

# MacOS
brew install ffmpeg

# Windows
choco install ffmpeg
```

If none of these options are available to you, head to this site and install
ffprobe according to the directions given there:
[https://ffmpeg.org/download.html](https://ffmpeg.org/download.html)


#### LilyPond (optional)

You can use `apt` on Linux, `brew` on MacOS or `choco` on Windows: 

```shell
# Linux
apt get lilypond

# MacOS
brew install lilypond

# Windows
choco install lilypond
```

If none of these options are available, head to this site and install
LilyPond according to the directions given there:
[LilyPond](https://lilypond.org)


### Install slippery-chicken

Now you are ready to install slippery chicken. Download the sources from
[https://github.com/mdedwards/slippery-chicken](https://github.com/mdedwards/slippery-chicken) 
and put them in your ASDF/Quicklisp directory, or:

```shell
# in your ASDF/Quicklisp standard directory
git clone https://github.com/mdedwards/slippery-chicken.git
```

### Configure slippery-chicken

The last step is modifying the initialisation file of your Common Lisp implementation to
load slippery chicken and its dependencies. The name and location of your init
file depends on the implementation you are using. With SBCL, for example, it is
`~/.sbclrc` by default, whereas Clozure CL uses `~/ccl-init.lisp` or
`~/.ccl-init.lisp`.

Add the following line to the init file:

```lisp
(ql:quickload :cmn) ;; if you're using it
(ql:quickload :clm) ;; if you're using it
(ql:quickload :slippery-chicken)
```

If you are not using Quicklisp, replace the `ql:quickload` with
`asdf:load-system`. Please note that you might then need to manually install and
load additional packages which are required by slippery-chicken as well as to
load slippery chicken's dependencies before loading slippery-chicken
itself. [See below](#configure-slippery-chicken-1) for details.

Finally, you might want to customise some of slippery chicken's global
configuration variables[^4]. In case you use one of the optional dependencies,
ensure the following paths are set to the actual locations of the binaries on
your system, by appending the respective lines to your CL init file:

```lisp
# for example...
(set-sc-config 'lilypond-command "/opt/homebrew/bin/lilypond")
(set-sc-config 'ffprobe-command "/opt/homebrew/bin/ffprobe")
(set-sc-config 'csound-command "/opt/homebrew/bin/csound")
```

You should be able to use the slippery-chicken package in your Common Lisp REPL.


## Long

### Prerequisites and Preparations

Although slippery chicken's dependency overhead is rather low and installing the
software via ASDF is quite straightforward, a few remarks on its prerequisites
are worth being be made. Generally, this document assumes that you have a
working Common Lisp implementation running on your system as well as that you
have a basic understanding of the Common Lisp language.

#### ASDF

Most current Common Lisp implementations already include the ASDF-package by
default,[^1] thus it is very likely that you don't need to set up ASDF up before
going ahead. You can find out whether ASDF is available on your system by
evaluating this form in your Lisp interpreter:

```lisp
(find :asdf *features*)
```

This should return `:ASDF`.

In ASDF-parlance, modules or programs (e.g. slippery-chicken) are termed
*systems*. Each system contains a system definition file (the `.asd` file) which
provides information about dependencies, system components, etc. In case you 
encounter ASDF-related errors, inspecting the `.asd` file of the respective 
system is always a good strategy. 

> [!IMPORTANT] 
> If you are not familiar with ASDF, it is worth mentioning that all systems
> need to be present in the ASDF-search-path/registry. A simple and
> straightforward way to do this is to place the respective files in ASDF's
> standard location (`~/common-lisp/` or
> `~/.local/share/common-lisp/source/`). You could also use symbolic links, of
> course. For more detail
> cf. [https://asdf.common-lisp.dev/asdf.html#Configuring-ASDF-to-find-your-systems](https://asdf.common-lisp.dev/asdf.html#Configuring-ASDF-to-find-your-systems).


#### Quicklisp (recommended)

As some modules (i.e. dependencies) of slippery chicken themselves depend on
modules probably not present on your system and ASDF itself does not take care
to download them,[^2] you might want to use Quicklisp, a Common Lisp
library manager built upon ASDF to dynamically (down-)load the respective
components. In order to do so, head over to
[https://www.quicklisp.org/](https://www.quicklisp.org/) and follow the
instructions given there.

We recommend using Quicklisp as it facilitates handling nested dependencies and
thus minimises the effort needed to install and load slippery chicken.

> [!IMPORTANT]
> When using Quicklisp, the default search-path (cf. [ASDF](#asdf)) for local
> projects might change to `~/quicklisp/local-projects/`. 


### Install Dependencies

One major dependency of slippery chicken is *Common Music* (CM). Without CM, 
slippery chicken will not work. Additionally, slippery chicken's functionality
will be greatly enhanced when further modules are available. This is a list of
slippery chicken's dependencies:

- Common Music/CM (required)
- Common Music Notation/CMN (optional)
- Common Lisp Music/CLM (optional)
- ffprobe (optional)
- LilyPond (optional)

The following sub-sections describe the installation process for each
module. Except for *Common Music*, it is possible to skip one or more of the
following sub-sections and thus adapt the feature set of slippery chicken to
your needs.


#### Common Music (required)

As mentioned before, Rick Taube's Common Music (CM) needs to be available for
slippery chicken to work. As slippery chicken is written in Common Lisp (CL), we
need a CL version of CM, which is provided by Orm Finnendahl. 

Download the CM-sources from
[https://github.com/ormf/cm](https://github.com/ormf/cm) and put them in your
ASDF/Quicklisp directory, or use the terminal:

```shell
# in your ASDF/Quicklisp standard directory
git clone https://github.com/ormf/cm.git
```


#### Common Music Notation (optional)

Bill Schottstaedt's Common Music Notation (CMN) is capable of quickly generating
musical scores from CL and is required, for example, by slippery-chicken's
`cmn-display`[^3] method.

If you want to use CMN, download the tarball archive from
[https://ccrma.stanford.edu/software/cmn/](https://ccrma.stanford.edu/software/cmn/)
and unpack(-tar) it to your ASDF-/quicklisp directory. Alternatively, you can
also use the terminal:

```shell
# in your ASDF/Quicklisp standard directory
curl https://ccrma.stanford.edu/software/cmn/cmn.tar.gz -o cmn.tar.gz
tar xf cmn.tar.gz
```

#### Common Lisp Music (optional)

Bill Schottstaedt's Common Lisp Music (CLM) is used in slippery chicken to write
and (quickly) analyse soundfiles. 

In order to use CLM, download the tarball archive from
[https://ccrma.stanford.edu/software/clm/](https://ccrma.stanford.edu/software/clm/)
and unpack(-tar) it to your ASDF-/quicklisp directory. Alternatively, you can
also use the terminal:

```shell
# in your ASDF/Quicklisp standard directory
curl https://ccrma.stanford.edu/software/clm/clm-5.tar.gz -o clm.tar.gz
tar xf clm.tar.gz
```

##### C Compiler

CLM requires a C compiler to be present on your system. On most UNIX systems
(including Apple's macOS/OSX) you'll already have the GNU C compiler. You can
check whether yours is ready to go by typing the following command in your
terminal:

```
gcc -v
```

You should get some version details containing something like this:

```
Configured with: --prefix=/Library/Developer/CommandLineTools/usr --with-gxx-include-dir=/usr/include/c++/4.2.1
```

If you don't have a C compiler, installation methods vary from OS to OS:

- macOS
  - run `xcode-select --install` in the terminal or cf. this [wiki entry](https://github.com/mdedwards/slippery-chicken/wiki/how-to-install-slippery-chicken-'by-hand'#c-compiler) 
- Linux
  - Should have a C compiler installed; if not, ask your preferred
    search engine.
- Windows
  - CLM was not designed to run on Windows though Michael Edwards had success a
    long time ago on Windows 95/XP. See README.clm in the CLM distribution.
  

#### ffprobe (optional)

ffprobe (from the ffmpeg package) is used by slippery chicken to retrieve some
metadata from media files (e.g. video and, when not available via CLM, audio
files). Note that using ffprobe is significantly slower at this than CLM.

In order to install ffprobe, you can use `apt` on Linux, `brew` on
MacOS or `choco` on Windows. If none of these options are viable for you, head
to this site and install ffprobe according to the directions given there:
[https://ffmpeg.org/download.html](https://ffmpeg.org/download.html)

> [!NOTE] 
> ffprobe is not a Common Lisp module, thus all ASDF-/Quicklisp-related
> comments made above do not apply to this program.


#### LilyPond (optional)

If you want to directly render beautiful scores via
[LilyPond](https://lilypond.org) (e.g. via slippery-chicken's `lp-display`), you
might be interested in installing this software on your system.

In order to install LilyPond, you can use `apt` on Linux, `brew` on
MacOS or `choco` on Windows. If none of these options are viable for you, head
to this site and install LilyPond according to the directions given there:
[LilyPond](https://lilypond.org)

> [!NOTE] 
> lilypond is not a Common Lisp module, thus all ASDF-/Quicklisp-related
> comments made above do not apply to this program.


### Install slippery-chicken

Installing slippery chicken, after fulfilling the prerequisites and (required)
dependencies, is usually very easy. You can either download the sources from
[http://github.com/mdedwards/slippery-chicken](http://github.com/mdedwards/slippery-chicken) and put them in your ASDF/Quicklisp directory, or use the terminal:

```shell
# in your ASDF/Quicklisp standard directory
git clone https://github.com/mdedwards/slippery-chicken.git
```

### Configure slippery-chicken

Now that you have installed all the components that you would like to use, you
need to take care of loading the modules before starting to work with slippery
chicken. It is recommended to do this via the init file of your Common Lisp
implementation which will be loaded and evaluated when you start your Common
Lisp interpreter. The name and location of your init file depends on the
implementation. On SBCL, for example, it is `~/.sbclrc` by default, whereas
Clozure CL uses `~/ccl-init.lisp` or `~/.ccl-init.lisp`. Add the following line
to your init file:


```lisp
(ql:quickload :cmn) ;; if you're using it
(ql:quickload :clm) ;; if you're using it
(ql:quickload :slippery-chicken)
```

If you are not using Quicklisp, replace the `ql:quickload` with
`asdf:load-system`. Please note that you then need to manually load various
dependencies before loading slippery-chicken. The following could serve as an
starting point to load a full slippery chicken installation without Quicklisp:

```lisp
(asdf:load-system :cmn)
(asdf:load-system :alexandria) ;; required by Common Music (cm)
(asdf:load-system :cm)
(asdf:load-system :clm)
(asdf:load-system :cl-ppcre) ;; required by slippery-chicken
(asdf:load-system :slippery-chicken)
```


#### Customise Global Options

slippery chicken contains some global variables which could/should be
modified to suit the user's requirements. For a detailed insight, take a look at
`src/globals.lsp`.[^4]

If you want to use slippery chicken with external programs like LilyPond,
ffprobe, or Csound, you might need to tell slippery chicken where it can find
the binaries to execute. This is because although there are standard locations
for such files, the actual location might differ from operating system to
operating system or machine to machine; locations also depend on the
installation method. Below you will find an example that sets the variables for
the aforementioned programs to the binary location where they were
installed via Homebrew on a Mac. These lines could also be appended to the
Common Lisp init file, but must be added after the commands which load
slippery-chicken and its dependencies (see above).

```lisp
(set-sc-config 'lilypond-command "/opt/homebrew/bin/lilypond")
(set-sc-config 'ffprobe-command "/opt/homebrew/bin/ffprobe")
(set-sc-config 'csound-command "/opt/homebrew/bin/csound")
```

To find out where a particular program is, you can call e.g. `which ffprobe`
in most terminals.

That's it. You should now be able to start working with slippery-chicken. 

---


[^1]:
    Cf. [https://asdf.common-lisp.dev/#implementations](https://asdf.common-lisp.dev/#implementations)

[^2]:
    Cf. [https://asdf.common-lisp.dev/#what_it_is_not](https://asdf.common-lisp.dev/#what_it_is_not)


[^3]:
    Cf. [https://michael-edwards.org/sc/manual/output.html#cmn](https://michael-edwards.org/sc/manual/output.html#cmn)

[^4]: [https://michael-edwards.org/sc/robodoc/globals_lsp.html#robo230](https://michael-edwards.org/sc/robodoc/globals_lsp.html#robo230)
