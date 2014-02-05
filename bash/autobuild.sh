#!/bin/bash
#
# Author: Yang, Ying-chao@gmail.com, 06-29-2013
#

emerge -av $1
if [ $? -eq 0 ]; then
    printf "\nCompile %s finisihed\n\n" $1
    exit 0
fi

# Parse last entry of emerge.log, set env to gcc to compile it again.
# Sample of entries in emerge.log:
#1372456596:  === (1 of 1) Post-Build Cleaning (dev-qt/qtdeclarative-4.8.4::/usr/portage/dev-qt/qtdeclarative/qtdeclarative-4.8.4.ebuild)

package=`tail -n 1 /var/log/emerge.log | awk -F "/" '{print $7}'`
flaggie package +gcc && emerge --resume || \
    printf "\nFailed to compile package %s\n" package

