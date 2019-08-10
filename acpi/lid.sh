#!/bin/bash
#
# Author: Yang,Ying-chao <yingchao.yang@icloud.com>, 2019-08-10
#

device=$1
id=$2

case $device in
    LID)
        case $id in
            close)
            ;;
            *)
            ;;
        esac
    ;;
    *)
        logger "ACPI: device ${device} not handled."
    ;;
esac
