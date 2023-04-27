echo "Building PyCrysFML08 with ifort"

export CI_PROJECT_DIR=`pwd`
export CRYSFML08=`pwd`/crysfml08
export CRYSFML08_INSTALL=${CI_PROJECT_DIR}/CFML08_Install

git clone https://code.ill.fr/rodriguez-carvajal/CrysFML2008 crysfml08
cd crysfml08
mkdir build
cd build
cmake -D ARCH32=OFF -D PYTHON_API=OFF -D CMAKE_BUILD_TYPE=Debug -D CMAKE_Fortran_COMPILER=ifort -D CMAKE_INSTALL_PREFIX=${CRYSFML08_INSTALL} -D CRYSFML_PREFIX=LibC ..
cmake --build .
cmake --install

cd $CRYSFML08_INSTALL/libC
ln -s libcrysfml.a libcrysfml08.a

cd $CI_PROJECT_DIR/scripts/linux
./make_ifort_pycrysfml08.sh

status=$?
if [ $status -ne 0 ]; then
	echo "Failure/Error during compilation"
	exit $status
fi
