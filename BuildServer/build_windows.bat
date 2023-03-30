md %CI_PROJECT_DIR%\\CFML08_Install
set CRYSFML08_INSTALL=%CI_PROJECT_DIR%\\CFML08_Install
git clone https://code.ill.fr/rodriguez-carvajal/CrysFML2008 crysfml2008
cd crysfml2008 && md build && cd build && cmake -G "NMake Makefiles" -D CMAKE_BUILD_TYPE=Debug -D CMAKE_INSTALL_PREFIX=%CRYSFML08_INSTALL% -D CRYSFML_PREFIX=LibC .. && cmake --build . && cmake --install . 
cd %CRYSFML08_INSTALL%\\libC
move crysfml.lib libcrysfml08.a
cd %CI_PROJECT_DIR%

cd %CI_PROJECT_DIR%\\scripts\\windows
call make_ifort_pycrysfml08.bat
set STATUS=%ERRORLEVEL%
rem Exit now
if %STATUS% neq 0 (
    echo "Failure/Error during compilation"
    exit %STATUS%
)

