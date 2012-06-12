#!/bin/sh

CWD="`(cd \"\`dirname \\\"$0\\\"\`\"; echo $PWD)`"
TOP="`dirname \"$CWD\"`/Resources"
APP=`defaults read -app sc emacs 2>/dev/null`
EDITOR=
PROCESSOR=`uname -p`

export SC_ROOT_DIR="${TOP}/sc"
export SBCL_HOME="${TOP}/sbcl"
export SC_OS="darwin"
export SC_ARCH="intel"
export SC_RUNTIME="${TOP}/sbcl/bin/sbcl"
export SC_RUNTIME_CORE="${TOP}/sbcl/lib/sbcl/sbcl.core"
export SC_RUNTIME_INIT="${TOP}/sbcl-init.lsp"
export SC_RUNTIME_FLAVOR="sbcl"
export SC_RUNTIME_VERSION="1.0.0"

 
export DYLD_LIBRARY_PATH="${TOP}/lib"

CEMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"
AEMACS="/Applications/Aquamacs.app/Contents/MacOS/Aquamacs"

if [[ $APP == Aquamacs ]] ; then
    if test -x "$AEMACS" ; then
        EDITOR="$AEMACS"
    else
        EDITOR=
    fi
elif [[ $APP == Emacs ]] ; then
    if test -x "$CEMACS" ; then
        EDITOR="$CEMACS"
    else
        EDITOR=
    fi
elif test $APP ; then
    EDITOR=
elif test -x "$AEMACS" ; then
    EDITOR="$AEMACS"
elif test -x "$CEMACS" ; then
    EDITOR="$CEMACS"
else
    EDITOR=
fi

cd ${TOP}

if [ -x "$EDITOR" ] ; then
#     "$EDITOR" --eval "(setq inhibit-startup-message t)" --directory "${TOP}/slime" -l "${TOP}/cm/etc/xemacs/cm.el" -l "${TOP}/sal/sal-mode.el" --eval "(progn (enable-cm-commands) (cm \"${SC_ROOT_DIR}/bin/cm.sh -l ${SC_RUNTIME} -s sal\"))"
    "$EDITOR" --eval "(setq inhibit-startup-message t)" --directory "${TOP}/slime" --directory "${TOP}/sbcl/bin" --directory "${TOP}/sbcl/lib/sbcl" --directory "${TOP}" -l "${TOP}/sc.el" --eval "(slime)"
else
    open -a Terminal "${CWD}/noemacs.sh"
fi

# EOF
