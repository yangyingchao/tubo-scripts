#!/bin/bash
#
# Author: Yang,Ying-chao <yangyingchao@gmail.com>, 2017-01-07
#

if [ $# -ne 1 ]; then
    echo "Usage: rwget URL"
else
    wget -c -r -np -k -L -p  $1
fi
