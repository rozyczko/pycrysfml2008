echo "Building PyCrysFML08 with ifort"

export CI_PROJECT_DIR=`pwd`
export CRYSFML08_REPO=`pwd`/crysfml08_repo
export CRYSFML08_DIST=`pwd`/crysfml08_dist

export INSTALLATION_DIR=`pwd`/pycrysfml08_dist
export CRYSFML08_INCLUDE_DIR=${CRYSFML08_DIST}/include
export CRYSFML08_LIB_DIR=${CRYSFML08_DIST}/lib

git clone https://code.ill.fr/rodriguez-carvajal/CrysFML2008 ${CRYSFML08_REPO}
cd ${CRYSFML08_REPO}
mkdir build
cd build
cmake -D ARCH32=OFF -D PYTHON_API=OFF -D CMAKE_BUILD_TYPE=Debug -D CMAKE_Fortran_COMPILER=ifort -D CMAKE_INSTALL_PREFIX=${CRYSFML08_DIST} ..
cmake --build .
make install

cd $CRYSFML08_DIST/lib
ln -s libcrysfml.a libcrysfml08.a

cd $CI_PROJECT_DIR/scripts/linux
./make_ifort_pycrysfml08.sh

status=$?
if [ $status -ne 0 ]; then
	echo "Failure/Error during compilation"
	exit $status
fi
