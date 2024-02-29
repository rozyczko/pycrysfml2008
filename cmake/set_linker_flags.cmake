macro(set_linker_flags)

    set(CMAKE_EXE_LINKER_FLAGS "")

    get_filename_component(COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME_WE)

    if(COMPILER_NAME STREQUAL ifort)

        if(UNIX)
            # Hack to prevent CMake from adding -i_dynamic linker flag that overrides the -static-intel flag to be used.
            set(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS "")
            set(CMAKE_EXE_LINKER_FLAGS "-static-intel")
        endif()

    endif()

endmacro()
