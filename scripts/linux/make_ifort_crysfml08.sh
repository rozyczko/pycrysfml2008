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

echo Building CFML_Sxtal_Geom
ifort -fPIC -fpp -c ../../src/py_cfml_sxtal_geom.f90 -I$CRYSFML08_INSTALL/libC
ifort -shared -o py_cfml_sxtal_geom.so py_cfml_sxtal_geom.o -L $CRYSFML08_INSTALL/libC -l CrysFML08
mv py_cfml_sxtal_geom.so ../../pycrysfml08/

echo Building CFML_DiffPatt
ifort -fPIC -fpp -c ../../src/py_extension_cfml_diffpatt.f90 -I$CRYSFML08_INSTALL/libC
ifort -fPIC -fpp -c ../../src/py_cfml_diffpatt.f90 -I$CRYSFML08_INSTALL/libC
ifort -shared -o py_cfml_diffpatt.so py_extension_cfml_diffpatt.o py_cfml_diffpatt.o -L $CRYSFML08_INSTALL/libC -l CrysFML08
mv py_cfml_diffpatt.so ../../pycrysfml08/

echo Building CFML_Reflections
ifort -fPIC -fpp -c ../../src/py_extension_cfml_reflections.f90 -I$CRYSFML08_INSTALL/libC
ifort -fPIC -fpp -c ../../src/py_cfml_reflections.f90 -I$CRYSFML08_INSTALL/libC
ifort -shared -o py_cfml_diffpatt.so py_extension_cfml_reflections.o py_cfml_reflections.o -L $CRYSFML08_INSTALL/libC -l CrysFML08
mv py_cfml_reflections.so ../../pycrysfml08/

echo Building CFML_VTK
ifort -fPIC -fpp -c ../../src/py_cfml_vtk.f90 -I$CRYSFML08_INSTALL/libC
ifort -shared -o py_cfml_vtk.so py_cfml_vtk.o -L $CRYSFML08_INSTALL/libC -l CrysFML08
mv py_cfml_vtk.so ../../pycrysfml08/

rm *.o *.mod
