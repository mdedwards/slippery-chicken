#!/bin/sh
echo ""
echo "*************************************************************"
echo "*************************************************************"
echo "*************************************************************"
echo ""
echo "No Emacs application found under /Applications"
echo ""
echo "Either install a recommended Emacs:"
echo ""
echo "    Aquamacs Emacs: http://aquamacs.org/"
echo "    Carbon Emacs: http://homepage.mac.com/zenitani/emacs-e.html"
echo ""
echo "or set slippery-chicken's emacs property by hand:"
echo ""
echo '    $ defaults write -app sc emacs <editor>'
echo ""
echo "*************************************************************"
echo "*************************************************************"
echo "*************************************************************"

#APPDIR=`dirname "$0"`
#exec "${APPDIR}/bin/dppccl" -I "${APPDIR}/bin/cm.img

# EOF


