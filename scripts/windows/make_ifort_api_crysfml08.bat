@echo off
rem -------------------------------------------------------------
rem PyCrysFML08
rem -------------------------------------------------------------
rem This file is part of PyCrysFML08
rem
rem The PyCrysFML08 is distributed under LGPL. In agreement with the
rem Intergovernmental Convention of the ILL, this software cannot be used
rem in military applications.
rem
rem PyCrysFML08 is based on Elias Rabel work for Forpy, see <https://github.com/ylikx/forpy>.
rem
rem Copyright (C) 2020-2022  Institut Laue-Langevin (ILL), Grenoble, FRANCE
rem
rem Authors: Nebil A. Katcho (ILL)
rem          Juan Rodriguez-Carvajal (ILL)
rem
rem
rem
rem This library is free software; you can redistribute it and/or
rem modify it under the terms of the GNU Lesser General Public
rem License as published by the Free Software Foundation; either
rem version 3.0 of the License, or (at your option) any later version.
rem
rem This library is distributed in the hope that it will be useful,
rem but WITHOUT ANY WARRANTY; without even the implied warranty of
rem MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
rem Lesser General Public License for more details.
rem
rem You should have received a copy of the GNU Lesser General Public
rem License along with this library; if not, see <http://www.gnu.org/licenses/>.
rem
rem -------------------------------------------------------------

ifort /c /fpp %FORPY%\forpy_mod.f90
ifort /c /fpp ..\..\src\wraps_cfml_atoms.f90 /I%CRYSFML08_INSTALL%\include
ifort /c /fpp ..\..\src\wraps_cfml_metrics.f90 /I%CRYSFML08_INSTALL%\include
ifort /c /fpp ..\..\src\py_cfml_sxtal_geom.f90 /I%CRYSFML08_INSTALL%\include
ifort /c /fpp ..\..\src\py_extension_cfml_messages.f90 /I%CRYSFML08_INSTALL%\include
ifort /c /fpp ..\..\src\py_extension_cfml_ioform.f90 /I%CRYSFML08_INSTALL%\include
ifort /c /fpp ..\..\src\py_extension_cfml_sxtal_geom.f90 /I%CRYSFML08_INSTALL%\include
ifort /c /fpp ..\..\src\py_extension_cfml_diffpatt.f90 /I%CRYSFML08_INSTALL%\include
ifort /c /fpp ..\..\src\api_init.f90 /I%CRYSFML08_INSTALL%\include
link *.obj /out:"pycrysfml08.dll" /libpath:%CRYSFML08_INSTALL%\lib /dll %LIBPYTHON% libCrysFML08.a
if not exist ..\..\dll (
    mkdir ..\..\dll
)
move pycrysfml08.dll ..\..\dll\pycrysfml08.pyd
del *.obj *.mod *.exp *.lib