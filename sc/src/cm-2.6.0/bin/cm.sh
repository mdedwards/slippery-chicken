#!/bin/sh -
#
# File:           cm.sh
#
SUMMARY="         run Common Music"
#
# Author:         Tobias Kunze Briseño
# E-Mail:         t AT fictive DOT com
# Org:            
#
# $Source: /cvsroot/commonmusic/cm/bin/cm.sh,v $
# $Date: 2004/05/15 15:29:28 $
# $Revision: 1.40 $

# Defaults

: ${CM_EDITOR:=}
: ${CM_RUNTIME:=}
: ${CM_RUNTIME_PREFS:=openmcl cmucl acl clisp sbcl guile}

: ${CM_OS:=}
: ${CM_ARCH:=}
: ${CM_RUNTIME_FLAVOR:=}
: ${CM_RUNTIME_VERSION:=}
: ${CM_ROOT_DIR:=}


# Description --------------------------------------------------------------
OPTIONS="
    -h           print this help
    -q           don't run cm, only echo platform as 'os-arch' string
    -n           dry run
    -v           be verbose
    -e editor    run under this editor (default: '${CM_EDITOR:-<none>}')
    -l lisp      run this Lisp/Scheme (default: '${CM_RUNTIME:-<unset>}') 
    -P prefs     preferred runtimes (default: '$CM_RUNTIME_PREFS')
    -O OS        OS in case autodetection fails
    -A arch      machine architecture in case autodetection fails
    -F flavor    Lisp flavor in case autodetection fails
    -V version   Lisp version in case autodetection fails
    -R cmroot    Common Music root directory in case autodetection fails
"
DESCRIPTION="
    This script starts Common Music (cm) either by loading it into a fresh
    Lisp or Scheme session or by launching an existing Lisp image that has
    been built with Common Music pre-loaded.  Additionally, this process can
    be run stand-alone in a terminal or as a subprocess of an
    Emacs-compatible editor such as xemacs(1), emacs(1), or gnuclient(1).

    Since the script is designed to autodetect all required parameters at
    runtime, it should in virtually all cases suffice to call it without any
    arguments.  This autodetection however may be customized or overridden
    via options and the following environment variables:

      CM_EDITOR
        Name or path or command of an Emacs-compatible editor under which to
        run cm, e.g. 'xemacs' or '/usr/bin/gnuclient' or '~/bin/xemacs -nw'.
        Same as -e option.

      CM_RUNTIME
        Name or path or command of a Lisp or Scheme system to execute,
        e.g. 'clisp' or '/usr/bin/openmcl' or '../foo/bin/lisp -cxLf'.  Same
        as -l option.

      CM_RUNTIME_PREFS
        List of CM_RUNTIME_FLAVORs (NOT CM_RUNTIMEs!) to try during
        autodetection, in order of preference.  Same as -r option.

      CM_OS
        Symbolic name of host OS, e.g. 'linux' or 'darwin'.  Same as -O
        option.

      CM_ARCH
        Symbolic name of host architecture, e.g. 'i386' or 'powerpc'.  Same
        as -A option.

      CM_RUNTIME_FLAVOR
        Symbolic name of the Lisp/Scheme flavor to run, e.g. 'clisp' or
        'cmucl'.  Useful in conjuction with CM_RUNTIME or the -l option if
        the flavor can't be derived from the command name or path.  Same as
        -F option.

      CM_RUNTIME_VERSION
        Version of the Lisp/Scheme to run.  Useful in conjuction
        with CM_RUNTIME or the -l option if the version can't be derived from
        the command name or path.  Same as -V option.

      CM_ROOT_DIR
        Absolute path of the Common Music root directory in case
        autodetection fails.  Same as -R option.

    For autodetection to work, it is important that the script be not moved
    from its location inside the Common Music directory tree.  A 'cm' (or
    similar) command in a standard binary location can be easily provided
    either by creating a symlink or by creating a wrapper shell script, e.g.

      # ln -s '/path/to/Common Music/bin/cm.sh' /usr/local/bin/cm

    or

      # cat <<EOD > /usr/local/bin/cm
      #!/bin/sh
      exec /path/to/cm/bin/cm.sh \"$@\"
      EOD
      # chmod 755 /usr/local/bin/cm

    Requirements: bash(1), cat(1), cut(1), cygpath(1) (cygwin only),
    echo(1), head(1), sed(1), sort(1), tr(1), uname(1), which(1); getopts
    and ls; a working installation of a Lisp or Scheme runtime; the '-repl'
    option if clisp is used (clisp 2.31 or higher).
"

# --------------------------------------------------------------------------


#
# Utils
# -----

imatch_head_token () {
  test `echo $1 | tr '[A-Z]' '[a-z]' | sed 's/[^a-z].*//;'` = $2
}
imatch_end_token () {
  test `echo $1 | tr '[A-Z]' '[a-z]' | sed 's/.*[^a-z]//;'` = $2
}

msg_i () { echo "[Notice ]  $*" >> /dev/stderr ;         }
msg_w () { echo "[Warning]  $*" >> /dev/stderr ;         }
msg_e () { echo "[Error  ]  $*" >> /dev/stderr ;         }
msg_f () { echo "[Fatal  ]  $*" >> /dev/stderr ;         }
msg_x () { echo "$*"            >> /dev/stderr ; exit 1; }

canonicalize_string () {
  echo $* | tr 'A-Z/ ' 'a-z_'
}

sanitize_path () {
  if test $CYGWIN_HACKS ; then
    cygpath -u -a "$1"
  else
    echo "$1"
  fi
}

CM_PLATFORM=
OSX_EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs

resolve_bin () {
  # OSX Emacs: check /Applications/Emacs.app first
  if test $CM_PLATFORM && 
     imatch_head_token $CM_PLATFORM darwin &&
     imatch_end_token $1 emacs && 
     test -x $OSX_EMACS ; then 
    echo $OSX_EMACS
  else
    # can't use return code b/c cygwin 'which' is broken
    EXE=`which "$1" 2>/dev/null`
    if test -z "$EXE" ; then
      test $2 && msg_w "'$1' not found in PATH ($PATH)."
      echo ""
    else
      which "$1"
    fi
  fi
}


#
# Cygwin Hacks
# ------------
# Catch-33.  Can't check for uname yet since OSX double-clicked apps won't
# find it, can't shield OSX from the test before options are parsed, and
# can't postpone CYGWIN_HACKS test until after options are parsed since
# cygwin's default /bin/sh does not know about getopts.  Hence the following
# abomination.

set_cygwin () {
  : ${WINPATH_PREFIX:=/cygdrive}
  export CYGWIN_HACKS=1
  export WINPATH_PREFIX

  # cygwin sh is not bash
  if test ! $BASH ; then
    BASH_EXE=`resolve_bin bash WARN`
    if [ ! $BASH_EXE ] ; then
      msg_f "Cygwin: 'sh' broken and can't find 'bash'!  Install bash first."
      msg_x "Aborting. "
    fi
  fi
}

if test ! $CYGWIN_HACKS ; then
  if test $CM_OS ; then
    if imatch_head_token "$CM_OS" cygwin ; then
      set_cygwin
      exec $BASH_EXE -- "$0" "$@"
    fi
  elif which uname >/dev/null 2>&1 ; then
    if imatch_head_token `uname -s` cygwin ; then
      set_cygwin
      exec $BASH_EXE -- "$0" "$@"
    fi
  elif which cygpath >/dev/null 2>&1 ; then
    # last resort: assume cygwin if no CM_OS and no uname but cygpath
    set_cygwin
    exec $BASH_EXE -- "$0" "$@"
  fi
fi



#
# Help
# ----

CMD=`basename "$0"`
USAGE="
  Usage: $CMD -h
         $CMD [-qnv] [-e editor] [-l lisp] [-P prefs] [-O OS] [-A arch]
               [-F lisp-flavor] [-V lisp-version] [-R cmroot]
"

print_help() {
  echo ""
  echo "  $CMD --"$SUMMARY
  echo "$USAGE"
  echo "  Options:"
  echo "$OPTIONS"
  echo "$DESCRIPTION"
}


#
# Variables
# ---------

QUERY_PLATFORM=
EXEC=exec
VERBOSE=

EDITOR_OPT=$CM_EDITOR
LISP_OPT=$CM_RUNTIME
LISP_PREFS=$CM_RUNTIME_PREFS
OS=$CM_OS
ARCH=$CM_ARCH
LISP_FLV=$CM_RUNTIME_FLAVOR
LISP_VRS=$CM_RUNTIME_VERSION

PAGER=`which less 2>/dev/null || which more 2>/dev/null || echo cat`

IMG_NAME=cm.img


#
# Options
# -------

while getopts hqnve:l:P:O:A:F:V:R: OPT
do
  case $OPT in
    h)  print_help | $PAGER
        exit 2
        ;;
    q)  QUERY_PLATFORM=1 ;;
    n)  EXEC=true ;;
    v)  VERBOSE=1 ;;
    e)  EDITOR_OPT=$OPTARG ;;
    l)  LISP_OPT=$OPTARG ;;
    P)  LISP_PREFS=$OPTARG ;;
    O)  OS=$OPTARG ;;
    A)  ARCH=$OPTARG ;;
    F)  LISP_FLV=$OPTARG ;;
    V)  LISP_VRS=$OPTARG ;;
    R)  CM_ROOT_DIR=$OPTARG ;;
    \?) echo "$USAGE"
        exit 1 ;;
  esac    
done

shift `expr $OPTIND - 1`


#
# Path Detection
# --------------

real_path () {
  if [[ "$1" == /* ]] ; then
    CHK="$1"
  elif [ -z "$1" ] ; then
    CHK="$CWD"
  else
    CHK="$CWD/$1"
  fi
  if [ -e "$CHK" ] ; then
    sanitize_path "$CHK"
  else 
    echo ''
  fi
}


# Resolve symbolic links
if test -L "$0" ; then
  ARGV0=`\ls -l "$0" | sed 's/.*-> //;'`
else
  ARGV0="$0"
fi

CWD=`pwd`

if [ "$CM_ROOT_DIR" ] ; then
  LOC="$CM_ROOT_DIR"
  export PATH="$LOC/bin:$PATH"
else
  PTU=`echo "$ARGV0" | sed 's:[^/]*$::;s:\(.\)/$:\1:;'`
  LOC=`real_path "$PTU"`
  if [ ! "$LOC" ] ; then
    msg_e "No such file or directory: '$1'"
    msg_f "Can't determine CM_ROOT_DIR!"
    msg_i "Re-run with -R option."
    msg_x "Aborting."
  else
    export PATH="$LOC:$PATH"
    LOC="$LOC/.."               # we are now in cm/bin, so get back out of it
  fi
fi

export CM_ROOT_DIR="$LOC"
export CM_ROOT="$LOC"           # backwards compat


#
# Platform Detection
# ------------------

if [ ! "$OS" -o ! "ARCH" ] ; then
  UNAME=`resolve_bin uname WARN`
  if [ ! "$UNAME" ] ; then
    msg_f "Can't run without either uname(1) or both of CM_OS and CM_ARCH set!"
    msg_i "Re-run with environment variables set or pass -O and -A options."
    msg_x "Aborting."
  fi
fi

if [ ! "$OS" ] ; then
  for flag in s o ; do
    if "$UNAME" -$flag >/dev/null 2>&1 ; then
      OS=`"$UNAME" -$flag 2>/dev/null`
      if [[ ! "$OS" || "$OS" == unknown ]] ; then OS= ; else break ; fi
    fi
  done
  if [ ! "$OS" ] ; then
    msg_e "Can't detect OS type."
    msg_i "Pass in -O option or set OS environment variable."
    msg_x "Aborting."
  fi
  OS=`canonicalize_string "$OS"`
fi

if [ ! "$ARCH" ] ; then
  for flag in p m i ; do
    if "$UNAME" -$flag >/dev/null 2>&1 ; then
      ARCH=`"$UNAME" -$flag 2>/dev/null`
      if [[ "$ARCH" = unknown ]] ; then ARCH= ; else break ; fi
    fi
  done
  if [ ! "$ARCH" ] ; then
    msg_e "Can't detect ARCH type."
    msg_i "Pass in -A option or set ARCH environment variable."
    msg_x "Aborting."
  fi
  ARCH=`canonicalize_string "$ARCH"`
fi

export CM_PLATFORM="$OS-$ARCH"

if [ $QUERY_PLATFORM ] ; then
  echo $CM_PLATFORM
  exit 0;
fi


#
# Lisp Detection
# --------------
# Determine which Lisp/Scheme to start:
#
#   { <lisp> { <initform> | <memdump> } | <image> }

LISP_EXE=
LISP_IMG=
LISP_DIA=LISP
LOAD=1

is_image () {
  [[ $1 == */$IMG_NAME ]]
}

find_lisp () {
  case $1 in
    cmucl) bin=lisp ;;
    *)     bin=$1 ;;
  esac
  resolve_bin $bin $3           # FIXME version ($2) is ignored for now
}

get_lisp_info () {
  if test $LISP_VRS ; then
    if test $LISP_FLV ; then
      echo ${LISP_FLV}_${LISP_VRS}
    else
      case "$1" in
        *clisp*|*CLISP*)                flv=clisp ;;
        *acl*|*ACL*)                    flv=acl ;;
        *lisp*|*LISP*|*cmucl*|*CMUCL*)  flv=cmucl ;;
        *openmcl*|*OPENMCL*|*dppccl*)   flv=openmcl ;;
        *sbcl*|*SBCL*)                  flv=sbcl ;;
        *guile*)                        flv=guile ;;
        *)
          msg_e "Can't determine flavor of '$1'."
          msg_i "Re-run with -F option."
          msg_x "Exiting."
          ;;
      esac
      echo ${flv}_$LISP_VRS
    fi
  else
    # This is ugly, wasteful, and requires maintenance :(
    vfd='[[:alpha:]]*[0-9][[:alnum:].]*'
    vre="s/^[^\"]*\"[^0-9]*\($vfd\(-$vfd\)*\).*/\1/p"
    case "${LISP_FLV:-$1}" in
      *clisp*|*CLISP*)
        flv=clisp
        vrs=`"$1" --version | head -1 | cut -d' ' -f3`
        min=`echo -e "$vrs\n2.31" | sort -n | head -1`
        if [ "$min" != 2.31 ] ; then
          msg_f "$1: version '$vrs' unsupported."
          msg_i "Need clisp with -repl option (version 2.31 or higher)."
          msg_x "Aborting."
        fi
        ;;
      *acl*|*ACL*)
        flv=acl
        vrs=`echo '(lisp-implementation-version)' | "$1" -batch 2>/dev/null \
	     | sed -n $vre`
        ;;
      *lisp*|*LISP*|*cmucl*|*CMUCL*)
        flv=cmucl
        vrs=`echo '(lisp-implementation-version)' | "$1" -quiet -batch \
	     | sed -n $vre`
        ;;
      *openmcl*|*OPENMCL*|*dppccl*)
        flv=openmcl
        vrs=`echo '(lisp-implementation-version)' | "$1" -b | sed -n $vre`
        ;;
      *sbcl*|*SBCL*)
        flv=sbcl
        vrs=`"$1" --version | cut -d' ' -f2`
        ;;
      *guile*)
        LISP_DIA=SCHEME
        flv=guile
        vrs=`"$1" --version | head -1 | cut -d' ' -f2`
        ;;
    esac
    if test $vrs ; then
      echo ${flv}_${vrs}
    else
      msg_e "Can't detect version of '$1'."
      msg_i "Re-run with -V option."
      msg_x "Exiting."
    fi
  fi
}

if [ "$LISP_OPT" ] ; then
  LEXE=`echo "$LISP_OPT" | sed 's: -.*$::'`
  LOPTS=`echo "$LISP_OPT" | sed 's:\( -.*\)*$:¦\1:;s:^.*¦ *::'`
  LISP_EXE=$LEXE
  if [[ "$LISP_EXE" == */* ]] ; then
    LISP_EXE=`real_path "$LISP_EXE"`
    if [ ! "$LISP_EXE" ] ; then
      msg_e "No such file or directory: '$LEXE'"
    fi
  else
    LISP_EXE=`resolve_bin "$LISP_EXE" WARN`
  fi
  if [ ! "$LISP_EXE" ] ; then
    msg_x "Aborting."
  elif [ ! -f "$LISP_EXE" -a ! -h "$LISP_EXE" ] ; then
    msg_e "Not a file or link: '$LISP_EXE'"
    msg_x "Aborting."
  fi
  
  if is_image "$LISP_EXE" ; then
    LOAD=
    LISP_INF=`echo "$LISP_EXE" | sed 's:^.*/\([^/]*\)/[^/]*$:\1:;'`
    if [ ! $LISP_INF ] ; then
      msg_e "Not in proper location: '$LISP_EXE'."
      msg_x "Aborting."
    fi
    LISP_FLV=`echo $LISP_INF | sed 's:\([^_]*\)_.*$:\1:;'`
    LISP_VRS=`echo $LISP_INF | sed 's:[^_]*_\([^_]*\)_.*:\1:;'`
    if [ -x "$LISP_EXE" ] ; then
      LISP_IMG=
    else
      LISP_IMG="$LISP_EXE"
      LISP_EXE=`find_lisp $LISP_FLV $LISP_VRS WARN`
      if [ ! "$LISP_EXE" ] ; then
        msg_e "Can't find a '$LISP_FLV' executable with version '$LISP_VRS'." 
        msg_x "Aborting."
      fi
    fi
  else
    if [ ! -x "$LISP_EXE" ] ; then
      msg_e "Not an executable: '$LISP_EXE'"
      msg_x "Aborting."
    fi
    LISP_INF=`get_lisp_info "$LISP_EXE"`
    if [ $? == 1 ] ; then exit 1 ; fi
    LISP_FLV=`echo $LISP_INF | sed 's:_.*::'`
    LISP_VRS=`echo $LISP_INF | sed 's:.*_::'`
    if [ -e "$LOC/bin/${LISP_INF}_$CM_PLATFORM/$IMG_NAME" ] ; then
      LISP_IMG="$LOC/bin/${LISP_INF}_$CM_PLATFORM/$IMG_NAME"
      LOAD=
    else
      LISP_IMG=
      LOAD=1
    fi
  fi
else
  if ls "$LOC"/bin/*_*_$CM_PLATFORM/$IMG_NAME >/dev/null 2>&1; then
    for pref in $LISP_PREFS ; do
      for img in "$LOC"/bin/${pref}_*_$CM_PLATFORM/$IMG_NAME ; do
        if [ "$img" != "$LOC/bin/${pref}_*_$CM_PLATFORM/$IMG_NAME" ] ; then
          LISP_INF=`echo "$img" | sed 's:^.*/\([^/]*\)/[^/]*$:\1:;'`
          LISP_FLV=`echo $LISP_INF | sed 's:\([^_]*\)_.*$:\1:;'`
          LISP_VRS=`echo $LISP_INF | sed 's:[^_]*_\([^_]*\)_.*:\1:;'`
          if [ -x "$img" ] ; then
            LISP_EXE="$img"
            LISP_IMG=
            LOAD=
            break
          else
            LISP_EXE=`find_lisp $LISP_FLV $LISP_VRS`
            if [ "$LISP_EXE" ] ; then
              LISP_IMG="$img"
              LOAD=
              break
            fi
          fi
          LISP_FLV=
          LISP_VRS=
        fi
      done
      if [ "$LISP_EXE" ] ; then break ; fi
    done
  fi

  if [ ! "$LISP_EXE" ] ; then
    for pref in $LISP_PREFS ; do
      LISP_EXE=`find_lisp $pref`
      if [ "$LISP_EXE" ] ; then
        LISP_INF=`get_lisp_info "$LISP_EXE"`
        if [ $? == 1 ] ; then exit 1 ; fi
        LISP_FLV=`echo $LISP_INF | sed 's:_.*::'`
        LISP_VRS=`echo $LISP_INF | sed 's:.*_::'`
        LOAD=1
        break
      fi
    done
  fi
fi

if [ ! "$LISP_EXE" ] ; then 
  msg_e "No executable found."
  msg_x "Aborting."
fi


#
# Lisp Command Line Assembly
# --------------------------
# Syntax:
# 
#   <lisp> <options> <initform>
#
# this is also the place to add implementation-specific provisions.

LISP_CMD=
LISP_INI=
LISP_LOA="$CM_ROOT/src/cm"

make_lisp_cmd () {
  LISP_EVL="(progn (load \"${LISP_LOA}.lisp\" :verbose nil) (cm))"
  case $LISP_FLV in
    clisp)
      LISP_CMD="'$LISP_EXE' -I -q -ansi $LOPTS"
      if [ $LOAD ] ; then
        LISP_CMD="$LISP_CMD -x '$LISP_EVL' -x t -repl"
      else
        test $LISP_INI && LISP_INI="-i $LISP_INI"
        LISP_CMD="$LISP_CMD -M '$LISP_IMG' $LISP_INI"
      fi
      ;;
    acl)
      LISP_CMD="'$LISP_EXE' $LOPTS"
      if [ $LOAD ] ; then
        LISP_CMD="$LISP_CMD -e '$LISP_EVL'"
      else
        test $LISP_INI && LISP_INI="-L $LISP_INI"
        LISP_CMD="$LISP_CMD -I '$LISP_IMG' $LISP_INI"
      fi
      ;;
    cmucl)
      LISP_CMD="'$LISP_EXE' $LOPTS"
      if [ $LOAD ] ; then
        LISP_CMD="$LISP_CMD -eval '$LISP_EVL'"
      else
        test $LISP_INI && LISP_INI="-init $LISP_INI"
        LISP_CMD="$LISP_CMD -core '$LISP_IMG' $LISP_INI"
      fi
      ;;
    openmcl)
      LISP_CMD="'$LISP_EXE' $LOPTS"
      if [ $LOAD ] ; then
        LISP_CMD="$LISP_CMD --eval '$LISP_EVL'"
      else
        test $LISP_INI && LISP_INI="--load $LISP_INI"
        LISP_CMD="$LISP_CMD --image-name '$LISP_IMG' $LISP_INI"
      fi
      ;;
    sbcl)
      LISP_CMD="'$LISP_EXE' --noinform $LOPTS"
      if [ $LOAD ] ; then
        LISP_CMD="$LISP_CMD --eval '$LISP_EVL'"
      else
        test $LISP_INI && LISP_INI="--userinit $LISP_INI"
        LISP_CMD="$LISP_CMD --core '$LISP_IMG' $LISP_INI"
      fi
      ;;
    guile)
      LISP_CMD="'$LISP_EXE' $LOPTS -l '${LISP_LOA}.scm' -e cm"
      ;;
    *)
      msg_e "Don't know how to call '$LISP_FLV' yet... =:("
      msg_x "Fatal."
      ;;
  esac
}


#
# Editor Detection & Command Line Assembly
# ----------------------------------------

EDITOR_EXE=
EDITOR_CMD=

under_editor () {
  test $EMACS
}

#
# Cygwin may need some pathnames translated into wintendo form.
# The various possibilities are
#
#   +--------------------+----------+---------------------+----------+
#   | Editor Type        | Elisp    | Runtime Type        | Load-CM/ |
#   +---------+----------+ Load     +----------+----------+ Image    |
#   | Unix    | Wintendo | Paths    | Unix     | Wintendo | Paths    |
#   +---------+----------+----------+----------+----------+----------+
#                                     /.../x                /.../x
#                                                /.../x     x:\...
#     /.../x               /.../x     /.../x                /.../x
#     /.../x               /.../x                /.../x     x:\...
#               /.../x     x:\...     x:\...                /.../x
#               /.../x     x:\...                x:\...     x:\...

is_wintendo_app () {
  [[ "$1" == $WINPATH_PREFIX/* ]]
}
wintendofy () {
  cygpath -m -a "$1"            # use "mixed" format
}

if [ "$EDITOR_OPT" ] ; then
  if under_editor && [[ "$EDITOR_OPT" != *gnuclient* ]] ; then
    msg_i "Already running under emacs.  Looking for gnuclient(1)."
    GNUCLIENT=`resolve_bin gnuclient WARN`
    if [ ! "$GNUCLIENT" ] ; then
      msg_i "Ignoring '$EDITOR_OPT'."
      EDITOR_OPT=
    else
      # FIXME_RFE: check for a running gnuserv first
      EDITOR_OPT="$GNUCLIENT"
    fi
  fi

  if [ "$EDITOR_OPT" ] ; then
    EEXE=`echo "$EDITOR_OPT" | sed 's: -.*$::'`
    EOPTS=`echo "$EDITOR_OPT" | sed 's:\( -.*\)*$:¦\1:;s:^.*¦ *::'`
    EDITOR_EXE=$EEXE
    if [[ "$EDITOR_EXE" == */* ]] ; then
      EDITOR_EXE=`real_path "$EDITOR_EXE"`
      if [ ! "$EDITOR_EXE" ] ; then
        msg_e "No such file or directory: '$EEXE'"
      fi
    else
      EDITOR_EXE=`resolve_bin "$EDITOR_EXE" WARN`
    fi
    if [ ! "$EDITOR_EXE" ] ; then
      msg_w "Command not found: '$EEXE'.  Ignoring."
    elif [ ! -f "$EDITOR_EXE" -a ! -h "$EDITOR_EXE" ] ; then
      msg_w "Not a file or link: '$EDITOR_EXE'.  Ignoring."
    else
      EL1="$LOC/etc/xemacs/listener.el"
      EL2="$LOC/etc/xemacs/cm.el"
      if test $CYGWIN_HACKS ; then
        if is_wintendo_app "$LISP_EXE" ; then
          if [ "$LISP_IMG" ] ; then
            LISP_IMG=`wintendofy "$LISP_IMG"`
          fi
          LISP_LOA=`wintendofy "$LISP_LOA"`
        fi
        if is_wintendo_app "$EDITOR_EXE" ; then
          EL1=`wintendofy "$EL1"`
          EL2=`wintendofy "$EL2"`
          LISP_EXE=`wintendofy "$LISP_EXE"`
        fi
      fi
      make_lisp_cmd
      LCM=`echo "$LISP_CMD" |tr -d "'" |sed 's:":\\\":g;'`
      INI="(progn (load \"$EL1\") (load \"$EL2\") (lisp-listener \"$LCM\"))"
      EDITOR_CMD="'$EDITOR_EXE' $EOPTS -eval '$INI'"
      if [[ "$EDITOR_EXE" == *client ]] ; then
        EDITOR_CMD="$EDITOR_CMD -batch"
      fi
    fi
  fi
fi

if [ ! "$LISP_CMD" ] ; then
  if test $CYGWIN_HACKS && is_wintendo_app "$LISP_EXE" ; then
    if [ "$LISP_IMG" ] ; then
      LISP_IMG=`wintendofy "$LISP_IMG"`
    fi
    LISP_LOA=`wintendofy "$LISP_LOA"`
  fi
  make_lisp_cmd
fi


#
# All Systems Go
# --------------
# Final command line syntax is:
# 
#   [ <editorcmd> | run_in_editor ] \
#     { LISP { LOAD [ DUMP ; RESTART_CM_IMAGE=> ] } | [ LISP ] CM_IMAGE }

if [ $VERBOSE ] ; then
  echo ""
  echo "Executable:  ${LISP_EXE:-<missing>}"
  echo "Image:       ${LISP_IMG:-<none>}"
  echo "Flavor:      ${LISP_FLV:-<n/a>}"
  echo "Version:     ${LISP_VRS:-<n/a>}"
  echo "Load:        $LOAD"
  echo -n "Command:     "
  if [ "$EDITOR_CMD" ] ; then
    echo "$EDITOR_CMD"
  else
    echo "$LISP_CMD"
  fi
  echo ""
fi

if [ "$EDITOR_CMD" ] ; then
  eval $EXEC $EDITOR_CMD
else
  eval $EXEC $LISP_CMD
fi

#
# EOF
