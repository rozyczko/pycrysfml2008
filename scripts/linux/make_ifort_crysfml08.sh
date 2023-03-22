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

echo Compiling forpy_mod.F90
ifort -fPIC -fpp -c $FORPY/forpy_mod.F90
echo Compiling wraps_cfml_atoms.f90
ifort -fPIC -fpp -c ../../src/wraps_cfml_atoms.f90 -I$CRYSFML08_INSTALL/include
echo Compiling wraps_cfml_metrics.f90
ifort -fPIC -fpp -c ../../src/wraps_cfml_metrics.f90 -I$CRYSFML08_INSTALL/include
echo Compiling py_cfml_sxtal_geom.f90
ifort -fPIC -fpp -c ../../src/py_cfml_sxtal_geom.f90 -I$CRYSFML08_INSTALL/include
echo Compiling py_extension_cfml_messages.f90
ifort -fPIC -fpp -c ../../src/py_extension_cfml_messages.f90 -I$CRYSFML08_INSTALL/include
echo Compiling py_extension_cfml_ioform.f90
ifort -fPIC -fpp -c ../../src/py_extension_cfml_ioform.f90 -I$CRYSFML08_INSTALL/include
echo Compiling py_extension_cfml_sxtal_geom
ifort -fPIC -fpp -c ../../src/py_extension_cfml_sxtal_geom.f90 -I$CRYSFML08_INSTALL/include
echo Compiling py_extension_cfml_diffpatt
ifort -fPIC -fpp -c ../../src/py_extension_cfml_diffpatt.f90 -I$CRYSFML08_INSTALL/include
echo Compiling py_extension_cfml_export_vtk
ifort -fPIC -fpp -c ../../src/py_extension_cfml_export_vtk.f90 -I$CRYSFML08_INSTALL/include
echo Compiling py_extension_cfml_reflections
ifort -fPIC -fpp -c ../../src/py_extension_cfml_reflections.f90 -I$CRYSFML08_INSTALL/include
echo Compiling api_init
ifort -fPIC -fpp -c ../../src/api_init.f90 -I$CRYSFML08_INSTALL/include

echo Linking
ifort -shared -o pycrysfml08.so *.o -L $CRYSFML08_INSTALL/lib -l CrysFML08

if [ ! -d ../../lib ]; then
    mkdir ../../lib
fi
mv pycrysfml08.so ../../lib
rm *.o *.mod
