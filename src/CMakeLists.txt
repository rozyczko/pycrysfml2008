set(PYCRYSFML08_SOURCES
    py_cfml_atoms.F90
    py_cfml_diffpatt.F90
    py_cfml_gspacegroups.F90
    py_cfml_ioform.F90
    py_cfml_metrics.F90
    py_cfml_profiles.F90
    py_cfml_reflections.F90
    py_cfml_structure_factors.F90
    py_cfml_sxtal_geom.f90
    py_cfml_vtk.F90
    )

add_library(pycrysfml08 SHARED ${PYCRYSFML08_SOURCES})

target_include_directories(pycrysfml08 PUBLIC ${CRYSFML08_INSTALL}/include)
target_link_directories(pycrysfml08 PUBLIC ${CRYSFML08_INSTALL}/lib)
target_link_libraries(pycrysfml08 libCrysFML08 ${LIBPYTHON})
install(TARGETS pycrysfml08 DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)