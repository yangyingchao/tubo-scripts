#!/bin/bash
#
# Author: Yang,Ying-chao <yingchao.yang@icloud.com>, 2019-08-10
#
logger "[ACPI] Fn+F8 pressed, WiFi rfkill state toggled"
rf=/sys/class/rfkill/rfkill0
case $(< $rf/state) in
    0) echo 1 >$rf/state;;
    1) echo 0 >$rf/state;;
esac
