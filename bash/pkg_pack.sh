#!/usr/bin/env bash

PKG_DB="/var/db/pkg"

prepair_libs() {
    # Searching files in this package ...
    ldd $1 | sed -r "s/.*?=>//g" | sed -r "s/\(.*?\)//g" \
        | sed -r "s/[ \t]+//g" | sed "1d" >> files.list
}

prepair_files() {
    echo "Searching libraries for "$1
    qlist $1  | grep -v "share/doc" > files.list
    cat files.list | grep "bin/" >> binaries.list
}

prepair_db() {
    echo "Searching database for "$1
    for ebuild_name in $(find $PKG_DB -name ${1##*/}"*.ebuild"); do
        echo "ebuild file: " $ebuild_name
        dir=$(dirname $ebuild_name)
        find $dir -name "*" >> files.list
    done
}

do_packup() {
    pkg_name=$(echo ${1##*/})".cpio"
    cat files.list | sort | uniq | cpio -oL > $pkg_name
}

packup_single() {

    # Calling prepair_files ...
    prepair_files $1

    # Prepairing libs
    for ff in $(cat  binaries.list);do
        prepair_libs $ff
    done

    prepair_db $1

    # Pack it up!
    do_packup $1
}

if [ "$#" = 0 ]; then
    echo "USAGE: ./pkg_pickup path_of_excutable_files"
    exit 1
fi

rm -rf files.list binaries.list

for fn in $@; do
    echo "Packing package: "$fn
    packup_single $fn
done

echo "Finished."