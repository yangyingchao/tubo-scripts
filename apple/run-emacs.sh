#!/bin/bash
#
# Author: Yang,Ying-chao <yangyingchao@gmail.com>, 2016-06-22
#

EMACS=/opt/Applications/Gentoo/Emacs-24.app/Contents/MacOS/Emacs
EMACSCLIENT=/opt/usr/bin/emacsclient

export LC_CTYPE=zh_CN.UTF-8
export LC_ALL=

_is_emacs_daemon_started () {
    netstat -nl 2> /dev/null | awk '{print $NF}' | grep -q "emacs"
}

_is_emacs_window_exist () {
    _is_emacs_daemon_started && \ $EMACSCLIENT -e '(<= 2 (length (visible-frame-list)))' | grep -q -x t
}

kill_emacs () {
    if _is_emacs_daemon_started;
    then $EMACSCLIENT -e -n '(let ((desktop-save '\''if-exists)) (dolist (f (cdr-safe (reverse (frame-list)))) (delete-frame f t)) (kill-emacs "yyyyyy"))'
         if _is_emacs_daemon_started;
         then ps -u $UID -o pid,comm= | grep Emacs$ | cut -d' ' -f1 | xargs -n 1 kill
         fi
    fi
    return 0
}

start_emacs ()
{
    $EMACS &>/dev/null &
    return $?
}

main () {
    _is_emacs_daemon_started
    if [ $? -ne 0 ] ; then
        start_emacs
        if [ $? -eq 0 ]; then
            echo ' [sucess]'
        else
            echo ' [faild]'
            return 1
        fi
    fi

    if [ "$1" = "-t" ]; then
        $EMACSCLIENT -n "$@"
    elif [ -z "$1" ];  then
        if ! _is_emacs_window_exist;  then
            $EMACSCLIENT -n
        fi
        osascript -e 'tell application "Emacs" to activate'
        return 0
    else
        if ! _is_emacs_window_exist;  then
            $EMACSCLIENT -n
        fi
        if ! echo "$*" | grep -q -e '-n'; then
            osascript -e 'tell application "Emacs" to activate'
        fi
        $EMACSCLIENT -n "$@"
    fi
    osascript -e 'tell application "Emacs" to activate'
}

main "$@"
