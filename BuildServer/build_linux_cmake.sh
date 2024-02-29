#
# Default Options
#
compiler="ifort"
#
# Arguments
#
for arg in "$@"
do
   case "$arg" in
      "ifort")
         compiler=$arg
         ;;
      "gfortran")
         compiler=$arg
         ;;
      "ifx")
         compiler=$arg
         ;;
   esac
done

echo "Building PyCrysFML08 with " $compiler

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

if [ $compiler = "ifx" ]; then
	. /opt/intel/oneapi/setvars.sh 
	echo $PATH
	which ifx
	compiler="/opt/intel/oneapi/compiler/2024.0/bin/ifx"
	cmake -D CMAKE_POSITION_INDEPENDENT_CODE=ON -D ARCH32=OFF -D PYTHON_API=OFF -D CMAKE_BUILD_TYPE=Debug -D CMAKE_Fortran_COMPILER=$compiler -D CMAKE_INSTALL_PREFIX=${CRYSFML08_DIST} ..
else
	cmake -D CMAKE_POSITION_INDEPENDENT_CODE=ON -D ARCH32=OFF -D PYTHON_API=OFF -D CMAKE_BUILD_TYPE=Debug -D CMAKE_Fortran_COMPILER=$compiler -D CMAKE_INSTALL_PREFIX=${CRYSFML08_DIST} ..
fi
cmake --build .
make install

cd $CRYSFML08_DIST/lib
ls -l
if [ -s libcrysfml.a ]; then
	ln -s libcrysfml.a libcrysfml08.a
else
	ln -s libCrysFML08.a libcrysfml08.a
fi

#PyCRYSFML08 
if [ ! -d $INSTALLATION_DIR ]; then
    mkdir $INSTALLATION_DIR
fi

cd $CI_PROJECT_DIR
export CRYSFML08_INSTALL=${CRYSFML08_DIST}
mkdir build
cd build
cmake -D CMAKE_POSITION_INDEPENDENT_CODE=ON -D CMAKE_BUILD_TYPE=Debug -D CMAKE_Fortran_COMPILER=$compiler -D CMAKE_INSTALL_PREFIX=${INSTALLATION_DIR} ..
cmake --build .
make install

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