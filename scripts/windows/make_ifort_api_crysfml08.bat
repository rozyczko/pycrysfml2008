@echo off
ifort /c /fpp %FORPY%\forpy_mod.f90
ifort /c /fpp ..\..\src\wraps_cfml_atoms.f90 /I%CRYSFML08_INSTALL%\include
ifort /c /fpp ..\..\src\wraps_cfml_metrics.f90 /I%CRYSFML08_INSTALL%\include
ifort /c /fpp ..\..\src\py_extension_cfml_messages.f90 /I%CRYSFML08_INSTALL%\include
ifort /c /fpp ..\..\src\py_extension_cfml_ioform.f90 /I%CRYSFML08_INSTALL%\include
ifort /c /fpp ..\..\src\py_extension_cfml_sxtal_geom.f90 /I%CRYSFML08_INSTALL%\include
ifort /c /fpp ..\..\src\api_init.f90 /I%CRYSFML08_INSTALL%\include
link *.obj /out:"pycrysfml08.dll" /libpath:%CRYSFML08_INSTALL%\lib /dll %LIBPYTHON% libCrysFML08.a
if not exist ..\..\dll (
    mkdir ..\..\dll
)
move pycrysfml08.dll ..\..\dll\pycrysfml08.pyd
del *.obj *.mod *.exp *.lib