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

#INSTALLATION_DIR=""
#CRYSFML08_INCLUDE_DIR=""
#CRYSFML08_LIB_DIR=""
#INSTALLATION_DIR=$INSTALLATION_DIR/pycrysfml08

if [ ! -d $INSTALLATION_DIR ]; then
    mkdir $INSTALLATION_DIR
fi

echo Path
echo `pwd`

echo Building CFML_Atoms
ifort -fPIC -fpp -c ../../src/py_cfml_atoms.f90 -I$CRYSFML08_INCLUDE_DIR
ifort -shared -o py_cfml_atoms.so py_cfml_atoms.o -L $CRYSFML08_LIB_DIR -l CrysFML08
mv py_cfml_atoms.so $INSTALLATION_DIR

echo Building CFML_DiffPatt
ifort -fPIC -fpp -c ../../src/py_cfml_diffpatt.f90 -I$CRYSFML08_INCLUDE_DIR
ifort -shared -o py_cfml_diffpatt.so py_cfml_diffpatt.o -L $CRYSFML08_LIB_DIR -l CrysFML08
mv py_cfml_diffpatt.so $INSTALLATION_DIR

echo Building CFML_gSpaceGroups
ifort -fPIC -fpp -c ../../src/py_cfml_gspacegroups.f90 -I$CRYSFML08_INCLUDE_DIR
ifort -shared -o py_cfml_gspacegroups.so py_cfml_gspacegroups.o -L $CRYSFML08_LIB_DIR -l CrysFML08
mv py_cfml_gspacegroups.so $INSTALLATION_DIR

echo Building CFML_IOForm
ifort -fPIC -fpp -c ../../src/py_cfml_ioform.f90 -I$CRYSFML08_INCLUDE_DIR
ifort -shared -o py_cfml_ioform.so py_cfml_ioform.o -L $CRYSFML08_LIB_DIR -l CrysFML08
mv py_cfml_ioform.so $INSTALLATION_DIR

echo Building CFML_Metrics
ifort -fPIC -fpp -c ../../src/py_cfml_metrics.f90 -I$CRYSFML08_INCLUDE_DIR
ifort -shared -o py_cfml_metrics.so py_cfml_metrics.o -L $CRYSFML08_LIB_DIR -l CrysFML08
mv py_cfml_metrics.so $INSTALLATION_DIR

echo Building CFML_Profiles
ifort -fPIC -fpp -c ../../src/py_cfml_profiles.f90 -I$CRYSFML08_INCLUDE_DIR
ifort -shared -o py_cfml_profiles.so py_cfml_profiles.o -L $CRYSFML08_LIB_DIR -l CrysFML08
mv py_cfml_profiles.so $INSTALLATION_DIR

echo Building CFML_Reflections
ifort -fPIC -fpp -c ../../src/py_cfml_reflections.f90 -I$CRYSFML08_INCLUDE_DIR
ifort -shared -o py_cfml_reflections.so py_cfml_reflections.o -L $CRYSFML08_LIB_DIR -l CrysFML08
mv py_cfml_reflections.so $INSTALLATION_DIR

echo Building CFML_Structure_Factors
ifort -fPIC -fpp -c ../../src/py_cfml_structure_factors.f90 -I$CRYSFML08_INCLUDE_DIR
ifort -shared -o py_cfml_structure_factors.so py_cfml_structure_factors.o -L $CRYSFML08_LIB_DIR -l CrysFML08
mv py_cfml_structure_factors.so $INSTALLATION_DIR

echo Building CFML_Sxtal_Geom
ifort -fPIC -fpp -c ../../src/py_cfml_sxtal_geom.f90 -I$CRYSFML08_INCLUDE_DIR
ifort -shared -o py_cfml_sxtal_geom.so py_cfml_sxtal_geom.o -L $CRYSFML08_LIB_DIR -l CrysFML08
mv py_cfml_sxtal_geom.so $INSTALLATION_DIR

rm *.o *.mod
