./test.sh

mkdir -p build/src
touch build/install
chmod a+x build/install

cp brain/braindriver build/run

src_dirs="brain simulator"

for D in $src_dirs ; do
  mkdir -p build/src/$D
done

find $src_dirs -name '*.hs' -exec 'cp' '{}' 'build/src/{}' ';'

tar -zcf build.tar.gz -C build/ .
