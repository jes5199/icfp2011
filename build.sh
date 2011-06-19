./test.sh

rm -rf build

mkdir -p build/src
echo "#!/bin/bash" > build/install
echo "true" >> build/install
chmod a+x build/install

cp brain/braindriver build/run

src_dirs="simulator"

for D in $src_dirs ; do
  mkdir -p build/src/$D
done

find $src_dirs -name '*.hs' -exec 'cp' '{}' 'build/src/{}' ';'

tar -zcf build.tar.gz -C build/ .
