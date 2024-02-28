echo "Building PyCrysFML08 with ifort"

export CI_PROJECT_DIR=`pwd`
export CRYSFML08_REPO=`pwd`/crysfml08_repo
export CRYSFML08_DIST=`pwd`/crysfml08_dist

export INSTALLATION_DIR=`pwd`/pycrysfml08_dist
export CRYSFML08_INCLUDE_DIR=${CRYSFML08_DIST}/include
export CRYSFML08_LIB_DIR=${CRYSFML08_DIST}/lib

export PYTHONPATH=${INSTALLATION_DIR}
export PATH=${INSTALLATION_DIR}:${PATH}

git clone --branch powder_mod_fix https://code.ill.fr/rodriguez-carvajal/CrysFML2008 ${CRYSFML08_REPO}
cd ${CRYSFML08_REPO}
mkdir build
cd build
cmake -D ARCH32=OFF -D PYTHON_API=OFF -D CMAKE_BUILD_TYPE=Debug -D CMAKE_Fortran_COMPILER=ifort -D CMAKE_INSTALL_PREFIX=${CRYSFML08_DIST} ..
cmake --build .
make install

cd $CRYSFML08_DIST/lib
ln -s libcrysfml.a libcrysfml08.a


#PyCRYSFML08 
if [ ! -d $INSTALLATION_DIR ]; then
    mkdir $INSTALLATION_DIR
fi

cd $CI_PROJECT_DIR/scripts/linux
./make_ifort_pycrysfml08.sh

mv $CI_PROJECT_DIR/pycrysfml08/__init__.py $INSTALLATION_DIR/pycrysfml08/.
rmdir $CI_PROJECT_DIR/pycrysfml08
cd $INSTALLATION_DIR/pycrysfml08
mkdir Databases
cp $CRYSFML08_REPO/Src/Databases/magnetic_data.txt Databases/.


status=$?
if [ $status -ne 0 ]; then
	echo "Failure/Error during compilation"
	exit $status
fi

cd $CI_PROJECT_DIR
python3 -m pytest tests/ -vv