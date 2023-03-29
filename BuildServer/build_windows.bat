set LIB="C:\\Program Files (x86)\\IntelSWTools\\compilers_and_libraries_2020.2.254\\windows\\compiler\\lib\\intel64_win";%LIB%
set PATH="C:\Program Files (x86)\Windows Kits\10\bin\10.0.19041.0\x64";%PATH%
set LIBPYTHON="C:\\Projects\\mdanse\\resources\\dependencies\\win-amd64\\Python3\\libs\\python36.lib"
md %CI_PROJECT_DIR%\\CFML08_Install
set CRYSFML08_INSTALL=%CI_PROJECT_DIR%\\CFML08_Install
git clone https://code.ill.fr/rodriguez-carvajal/CrysFML2008 crysfml2008
cd crysfml2008 && md build && cd build && cmake -G "NMake Makefiles" -D CMAKE_BUILD_TYPE=Debug -D CMAKE_INSTALL_PREFIX=%CRYSFML08_INSTALL% -D CRYSFML_PREFIX=LibC .. && cmake --build . && cmake --install . 
cd %CRYSFML08_INSTALL%\\libC
move crysfml.lib libcrysfml08.a
cd %CI_PROJECT_DIR%

call %CI_PROJECT_DIR%\\scripts\\windows\\make_ifort_pycrysfml08.bat
set STATUS=%ERRORLEVEL%
rem Exit now
if %STATUS% neq 0 (
    echo "Failure/Error during compilation"
    exit %STATUS%
)

del /s /q /f %CI_PROJECT_DIR%\\Dist
md %CI_PROJECT_DIR%\\Dist
xcopy /s /i /q %CI_PROJECT_DIR%\\DistLS\\Windows_64bits %CI_PROJECT_DIR%\\Dist\\Esmeralda
xcopy /s /i /q %CI_PROJECT_DIR%\\DistLS\\Docs %CI_PROJECT_DIR%\\Dist\\Docs
xcopy /s /i /q %CI_PROJECT_DIR%\\DistLS\\Examples %CI_PROJECT_DIR%\\Dist\\Examples
copy %CI_PROJECT_DIR%\\DistLS\\bg.png %CI_PROJECT_DIR%\\Dist\\Esmeralda\\bg.png
