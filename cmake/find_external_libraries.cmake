macro(find_external_libraries EXTERNAL_LIBS)

    set(TEMP)

    foreach(f ${ARGN})

        find_library(LIB ${f})

        if(LIB MATCHES "LIB-NOTFOUND")
            message(FATAL_ERROR "${f} library not found")
        else()
            set(TEMP ${TEMP} ${LIB})
        endif()
        unset(LIB CACHE)

    endforeach()

    set(${EXTERNAL_LIBS} ${TEMP})

endmacro()