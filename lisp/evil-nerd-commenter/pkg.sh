#!/bin/bash
name=evil-nerd-commenter
version=3.1.3
pkg=$name-$version
mkdir $pkg
cp *.el $pkg
cat << EOF > $pkg/$name-pkg.el
(define-package "$name" "$version"
                "whatever")
EOF
if [[ `uname -s` == *Darwin* ]]; then
   COPYFILE_DISABLE="" tar cvf $pkg.tar $pkg/
else
   tar cvf $pkg.tar $pkg/
fi
rm -rf $pkg/
