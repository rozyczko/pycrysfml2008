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
rem Authors: ILL Scientific Computing Group
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

set INSTALLATION_DIR=C:\Users\oarce\pycrysfml08\.venv\Lib\site-packages
set CRYSFML08_INCLUDE_DIR=C:\Users\oarce\CrysFML\ifort_release\include
set CRYSFML08_LIB_DIR=C:\Users\oarce\CrysFML\ifort_release\lib
set LIBPYTHON=C:\Users\oarce\AppData\Local\Programs\Python\Python311\libs\python311.lib
if %INSTALLATION_DIR% == "" (
    echo Please set INSTALLATION_DIR by editing the script
    goto exit
)
if %CRYSFML08_INCLUDE_DIR% == "" (
    echo Please set CRYSFML08_INCLUDE_DIR by editing the script
    goto exit
)
if %CRYSFML08_LIB_DIR% == "" (
    echo Please set CRYSFML08_LIB_DIR by editing the script
    goto exit
)

set INSTALLATION_DIR=%INSTALLATION_DIR%\pycrysfml08
if not exist %INSTALLATION_DIR% (
    mkdir %INSTALLATION_DIR%
)

rem CFML_Atoms
echo Building py_cfml_atoms.pyd
ifort /c /fpp /nologo /Warn ..\..\src\py_cfml_atoms.f90 /I%CRYSFML08_INCLUDE_DIR%
link py_cfml_atoms.obj /out:"py_cfml_atoms.dll" /libpath:%CRYSFML08_LIB_DIR% /dll %LIBPYTHON% libCrysFML08.a
move py_cfml_atoms.dll %INSTALLATION_DIR%\py_cfml_atoms.pyd

rem CFML_BckPeaks
echo Building py_cfml_bckpeaks.pyd
ifort /c /fpp /nologo /Warn ..\..\src\py_cfml_bckpeaks.f90 /I%CRYSFML08_INCLUDE_DIR%
link py_cfml_bckpeaks.obj /out:"py_cfml_bckpeaks.dll" /libpath:%CRYSFML08_LIB_DIR% /dll %LIBPYTHON% libCrysFML08.a
move py_cfml_bckpeaks.dll %INSTALLATION_DIR%\py_cfml_bckpeaks.pyd

rem CFML_DiffPatt
echo Building py_cfml_diffpatt.pyd
ifort /c /fpp /nologo /Warn ..\..\src\py_cfml_diffpatt.f90 /I%CRYSFML08_INCLUDE_DIR%
link py_cfml_diffpatt.obj /out:"py_cfml_diffpatt.dll" /libpath:%CRYSFML08_LIB_DIR% /dll %LIBPYTHON% libCrysFML08.a
move py_cfml_diffpatt.dll %INSTALLATION_DIR%\py_cfml_diffpatt.pyd

rem CFML_gSpaceGroups
echo Building py_cfml_gspacegroups.pyd
ifort /c /fpp /nologo /Warn ..\..\src\py_cfml_gspacegroups.f90 /I%CRYSFML08_INCLUDE_DIR%
link py_cfml_gspacegroups.obj /out:"py_cfml_gspacegroups.dll" /libpath:%CRYSFML08_LIB_DIR% /dll %LIBPYTHON% libCrysFML08.a
move py_cfml_gspacegroups.dll %INSTALLATION_DIR%\py_cfml_gspacegroups.pyd

rem CFML_IOForm
echo Building py_cfml_ioform.pyd
ifort /c /fpp /nologo /Warn ..\..\src\py_cfml_ioform.f90 /I%CRYSFML08_INCLUDE_DIR%
link py_cfml_ioform.obj /out:"py_cfml_ioform.dll" /libpath:%CRYSFML08_LIB_DIR% /dll %LIBPYTHON% libCrysFML08.a
move py_cfml_ioform.dll %INSTALLATION_DIR%\py_cfml_ioform.pyd

rem CFML_Metrics
echo Building py_cfml_metrics.pyd
ifort /c /fpp /nologo /Warn ..\..\src\py_cfml_metrics.f90 /I%CRYSFML08_INCLUDE_DIR%
link py_cfml_metrics.obj /out:"py_cfml_metrics.dll" /libpath:%CRYSFML08_LIB_DIR% /dll %LIBPYTHON% libCrysFML08.a
move py_cfml_metrics.dll %INSTALLATION_DIR%\py_cfml_metrics.pyd

rem CFML_Profiles
echo Building py_cfml_profiles.pyd
ifort /c /fpp /nologo /Warn ..\..\src\py_cfml_profiles.f90 /I%CRYSFML08_INCLUDE_DIR%
link py_cfml_profiles.obj /out:"py_cfml_profiles.dll" /libpath:%CRYSFML08_LIB_DIR% /dll %LIBPYTHON% libCrysFML08.a
move py_cfml_profiles.dll %INSTALLATION_DIR%\py_cfml_profiles.pyd

rem CFML_Reflections
echo Building py_cfml_reflections.pyd
ifort /c /fpp /nologo /Warn ..\..\src\py_cfml_reflections.f90 /I%CRYSFML08_INCLUDE_DIR%
link py_cfml_reflections.obj /out:"py_cfml_reflections.dll" /libpath:%CRYSFML08_LIB_DIR% /dll %LIBPYTHON% libCrysFML08.a
move py_cfml_reflections.dll %INSTALLATION_DIR%\py_cfml_reflections.pyd

rem CFML_Structure_Factors
echo Building py_cfml_structure_factors.pyd
ifort /c /fpp /nologo /Warn ..\..\src\py_cfml_structure_factors.f90 /I%CRYSFML08_INCLUDE_DIR%
link py_cfml_structure_factors.obj /out:"py_cfml_structure_factors.dll" /libpath:%CRYSFML08_LIB_DIR% /dll %LIBPYTHON% libCrysFML08.a
move py_cfml_structure_factors.dll %INSTALLATION_DIR%\py_cfml_structure_factors.pyd

rem CFML_Sxtal_Geom
echo Building py_cfml_sxtal_geom.pyd
ifort /c /fpp /nologo /Warn ..\..\src\py_cfml_sxtal_geom.f90 /I%CRYSFML08_INCLUDE_DIR%
link py_cfml_sxtal_geom.obj /out:"py_cfml_sxtal_geom.dll" /libpath:%CRYSFML08_LIB_DIR% /dll %LIBPYTHON% libCrysFML08.a
move py_cfml_sxtal_geom.dll %INSTALLATION_DIR%\py_cfml_sxtal_geom.pyd

rem rem CFML_VTK
rem echo Building py_cfml_vtk.pyd
rem ifort /c /fpp /nologo /Warn ..\..\src\py_cfml_vtk.f90 /I%CRYSFML08_INCLUDE_DIR%
rem link py_cfml_vtk.obj /out:"py_cfml_vtk.dll" /libpath:%CRYSFML08_LIB_DIR% /dll %LIBPYTHON% libCrysFML08.a
rem move py_cfml_vtk.dll %INSTALLATION_DIR%\py_cfml_vtk.pyd

del *.obj *.mod *.exp *.lib

:exit
