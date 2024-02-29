#################################
# Project section
#################################

# The minimum version number insuring a correct build.
cmake_minimum_required(VERSION 3.1.0 FATAL_ERROR)

# Set the a verbose make.
set(CMAKE_VERBOSE_MAKEFILE ON)

# Set the Fortran Compiler
if(DEFINED CMAKE_Fortran_COMPILER)
    set(CMAKE_Fortran_COMPILER ${CMAKE_Fortran_COMPILER} CACHE STRING "The Fortran compiler. One of 'ifort' or 'gfortran'.")
else()
    set(CMAKE_Fortran_COMPILER ifort CACHE STRING "The Fortran compiler. One of 'ifort' or 'gfortran'.")
endif()
message(STATUS "Setting compiler to ${CMAKE_Fortran_COMPILER}")

if(DEFINED CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE ${CMAKE_BUILD_TYPE} CACHE STRING "The build type. One of 'Release' or 'Debug'.")
else()
    set(CMAKE_BUILD_TYPE Release CACHE STRING "The build type. One of 'Release' or 'Debug'.")
endif()
message(STATUS "Setting build type to ${CMAKE_BUILD_TYPE}")

get_filename_component(COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME_WE)
if (DEFINED CMAKE_INSTALL_PREFIX)
    set(CMAKE_INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX} CACHE PATH "The installation path.")
else()
    set(CMAKE_INSTALL_PREFIX ${CMAKE_HOME_DIRECTORY}/${COMPILER_NAME} CACHE PATH "The installation path.")
endif()
message(STATUS "Setting CMAKE_INSTALL_PREFIX to ${CMAKE_INSTALL_PREFIX}")

if(DEFINED ENV{CRYSFML08_INSTALL})
    set(CRYSFML08_INSTALL $ENV{CRYSFML08_INSTALL} CACHE PATH "Installation directory" FORCE)
else()
    set(CRYSFML08_INSTALL CACHE PATH "Installation directory" FORCE)
endif()

if(DEFINED ENV{LIBPYTHON})
    set(LIBPYTHON $ENV{LIBPYTHON} CACHE FILEPATH "Installation directory" FORCE)
else()
    set(LIBPYTHON CACHE FILEPATH "Python library" FORCE)
endif()

project(pycrysfml08 Fortran)

add_subdirectory(src)