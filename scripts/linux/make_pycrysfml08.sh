# -------------------------------------------------------------
# PyCrysFML08
# -------------------------------------------------------------
# This file is part of PyCrysFML08
#
# The PyCrysFML08 is distributed under LGPL. In agreement with the
# Intergovernmental Convention of the ILL, this software cannot be used
# in military applications.
#
# PyCrysFML08 is based on Elias Rabel work for Forpy, see <https://github.com/ylikx/forpy>.
#
# Copyright (C) 2020-2022  Institut Laue-Langevin (ILL), Grenoble, FRANCE
#
# Authors: Nebil A. Katcho (ILL)
#          Juan Rodriguez-Carvajal (ILL)
#
#
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 3.0 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, see <http://www.gnu.org/licenses/>.
#
# -------------------------------------------------------------

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

if [ $compiler = "ifort" ]; then
    FFLAGS="-fPIC -fpp"
elif [ $compiler = "ifx" ]; then
    FFLAGS="-fPIC -fpp"
elif [ $compiler = "gfortran" ]; then
    FFLAGS="-fPIC -cpp -O0 -std=f2008 -ffree-line-length-0 -fdec-math -fall-intrinsics"
fi

#INSTALLATION_DIR=""
#CRYSFML08_INCLUDE_DIR=""
#CRYSFML08_LIB_DIR=""
INSTALLATION_DIR=$INSTALLATION_DIR/pycrysfml08

if [ ! -d $INSTALLATION_DIR ]; then
    mkdir $INSTALLATION_DIR
fi

echo Building CFML_Atoms
echo "$compiler $FFLAGS -c ../../src/py_cfml_atoms.F90 -I$CRYSFML08_INCLUDE_DIR"
$compiler $FFLAGS -c ../../src/py_cfml_atoms.F90 -I$CRYSFML08_INCLUDE_DIR
echo "$compiler -shared -o py_cfml_atoms.so py_cfml_atoms.o -L $CRYSFML08_LIB_DIR -l CrysFML08"
$compiler -shared -o py_cfml_atoms.so py_cfml_atoms.o -L $CRYSFML08_LIB_DIR -l CrysFML08 -L/usr/local/Cellar/python@3.8/3.8.3/Frameworks/Python.framework/Versions/3.8/lib/ -lpython3.8
mv py_cfml_atoms.so $INSTALLATION_DIR

echo Building CFML_DiffPatt
$compiler $FFLAGS -c ../../src/py_cfml_diffpatt.F90 -I$CRYSFML08_INCLUDE_DIR
$compiler -shared -o py_cfml_diffpatt.so py_cfml_diffpatt.o -L $CRYSFML08_LIB_DIR -l CrysFML08
mv py_cfml_diffpatt.so $INSTALLATION_DIR

echo Building CFML_gSpaceGroups
$compiler $FFLAGS -c ../../src/py_cfml_gspacegroups.F90 -I$CRYSFML08_INCLUDE_DIR
$compiler -shared -o py_cfml_gspacegroups.so py_cfml_gspacegroups.o -L $CRYSFML08_LIB_DIR -l CrysFML08
mv py_cfml_gspacegroups.so $INSTALLATION_DIR

echo Building CFML_IOForm
$compiler $FFLAGS -c ../../src/py_cfml_ioform.F90 -I$CRYSFML08_INCLUDE_DIR
$compiler -shared -o py_cfml_ioform.so py_cfml_ioform.o -L $CRYSFML08_LIB_DIR -l CrysFML08
mv py_cfml_ioform.so $INSTALLATION_DIR

echo Building CFML_Metrics
$compiler $FFLAGS -c ../../src/py_cfml_metrics.F90 -I$CRYSFML08_INCLUDE_DIR
$compiler -shared -o py_cfml_metrics.so py_cfml_metrics.o -L $CRYSFML08_LIB_DIR -l CrysFML08
mv py_cfml_metrics.so $INSTALLATION_DIR

echo Building CFML_Profiles
$compiler $FFLAGS -c ../../src/py_cfml_profiles.F90 -I$CRYSFML08_INCLUDE_DIR
$compiler -shared -o py_cfml_profiles.so py_cfml_profiles.o -L $CRYSFML08_LIB_DIR -l CrysFML08
mv py_cfml_profiles.so $INSTALLATION_DIR

echo Building CFML_Reflections
$compiler $FFLAGS -c ../../src/py_cfml_reflections.F90 -I$CRYSFML08_INCLUDE_DIR
$compiler -shared -o py_cfml_reflections.so py_cfml_reflections.o -L $CRYSFML08_LIB_DIR -l CrysFML08
mv py_cfml_reflections.so $INSTALLATION_DIR

echo Building CFML_Structure_Factors
$compiler $FFLAGS -c ../../src/py_cfml_structure_factors.F90 -I$CRYSFML08_INCLUDE_DIR
$compiler -shared -o py_cfml_structure_factors.so py_cfml_structure_factors.o -L $CRYSFML08_LIB_DIR -l CrysFML08
mv py_cfml_structure_factors.so $INSTALLATION_DIR

echo Building CFML_Sxtal_Geom
$compiler $FFLAGS -c ../../src/py_cfml_sxtal_geom.F90 -I$CRYSFML08_INCLUDE_DIR
$compiler -shared -o py_cfml_sxtal_geom.so py_cfml_sxtal_geom.o -L $CRYSFML08_LIB_DIR -l CrysFML08
mv py_cfml_sxtal_geom.so $INSTALLATION_DIR

rm *.o *.mod
