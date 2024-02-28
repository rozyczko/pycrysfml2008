md %CI_PROJECT_DIR%\\crysfml08_dist
set CRYSFML08_INSTALL=%CI_PROJECT_DIR%\\crysfml08_dist
md %CI_PROJECT_DIR%\\pycrysfml08_dist
set INSTALLATION_DIR=%CI_PROJECT_DIR%\\pycrysfml08_dist
git clone https://code.ill.fr/rodriguez-carvajal/CrysFML2008 crysfml2008_repo
cd crysfml2008_repo && md build && cd build && cmake -G "NMake Makefiles" -D CMAKE_BUILD_TYPE=Debug -D CMAKE_INSTALL_PREFIX=%CRYSFML08_INSTALL% .. && cmake --build . && cmake --install . 
cd %CRYSFML08_INSTALL%\\lib
move crysfml08.lib libcrysfml08.a
cd %CI_PROJECT_DIR%

cd %CI_PROJECT_DIR%\\scripts\\windows
call make_ifort_pycrysfml08.bat
set STATUS=%ERRORLEVEL%
rem Exit now
if %STATUS% neq 0 (
    echo "Failure/Error during compilation"
    exit %STATUS%
)

