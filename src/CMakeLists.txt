set(PYCRYSFML08_SOURCES
    forpy_mod.F90
    api_init.f90
    py_cfml_sxtal_geom.f90
    py_extension_cfml_diffpatt.f90
    py_extension_cfml_export_vtk.f90
    py_extension_cfml_ioform.f90
    py_extension_cfml_messages.f90
    py_extension_cfml_reflections.f90
    py_extension_cfml_sxtal_geom.f90
    wraps_cfml_atoms.f90
    wraps_cfml_reflections.f90
    wraps_cfml_metrics.f90)

add_library(pycrysfml08 SHARED ${PYCRYSFML08_SOURCES})

target_include_directories(pycrysfml08 PUBLIC ${CRYSFML08_INSTALL}/include)
target_link_directories(pycrysfml08 PUBLIC ${CRYSFML08_INSTALL}/lib)
target_link_libraries(pycrysfml08 libCrysFML08 ${LIBPYTHON})
install(TARGETS pycrysfml08 DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)