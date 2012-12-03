#!/bin/bash
######################################################################
## Copyright (C) 2011-2012, Yang, Ying-chao
##
## Author:        Yang, Ying-chao <yangyingchao@gmail.com>
##
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License
## as published by the Free Software Foundation; either version 2
## of the License, or (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
##
## Description: This simple script will try to copy remote folder to local
##   folder if necessary.
##
##
#####################################################################

#
# Author: Yang, Ying-chao@gmail.com, 09- 7-2012
#

function usage ()
{
    printf "Usage: source destination\n"
}


function guess_trans_type ()
{
    ttype=0
    if [ $# -ne 1 ]; then
        echo "Wrong number of arguments"
        return ttype
    fi
    
}

if [ $# -ne 2 ]; then
    usage
    exit 1
else
    SRC=$1
    DST=$2

    TransmissionType=$(guess_trans_type $SRC)
    
fi
