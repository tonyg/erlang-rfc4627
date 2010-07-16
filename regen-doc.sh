rm -rf doc
git clone -b master . tmp
make -C tmp DOC_DIR=`pwd`/doc `pwd`/doc/index.html
rm -rf tmp
