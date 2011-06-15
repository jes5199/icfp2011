#!/bin/sh
dpkg --list | cut -c5-52 | sed -e 's/ *$//' | ./eat5.sh | sort > current-packages.txt
echo 'THE FOLLOWING PACKAGES ARE INSTALLED BUT NOT PRESENT ON THE JUDGES MACHINE:'
diff icfp-package-list.txt current-packages.txt | grep '^>'
rm current-packages.txt
