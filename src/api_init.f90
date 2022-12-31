! ------------------------------------------------------------
! CrysFML08 API
!
! @license    GNU LGPL (see LICENSE)
! @copyright  Institut Laue Langevin 2020-now
! @authors    Scientific Computing Group at ILL (see AUTHORS),
!             based on Elias Rabel work for Forpy
! ------------------------------------------------------------


module api_init

    use forpy_mod
    use iso_c_binding
    use extension_cfml_ioform
    use extension_cfml_sxtal_geom

    implicit none

    type(PythonModule), save :: mod_Def
    type(PythonMethodTable), save :: method_Table

    contains

    ! Initialization function for Python 3
    ! Called when importing module
    ! Must use bind(c, name="PyInit_<module name>")
    ! Return value must be type(c_ptr),
    ! use the return value of PythonModule%init
    function PyInit_pycrysfml08() bind(c,name="PyInit_pycrysfml08") result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_pycrysfml08

        ! Local variables
        type(c_ptr) :: m

        m = Init()

    end function PyInit_pycrysfml08

    function Init() result(m)

        ! Local variables
        type(c_ptr) :: m
        integer :: ierror

        ierror = Forpy_Initialize()

        ! Build method table
        call method_Table%init(2)
        call method_Table%add_method("xtal_structure_from_file",&
            "py_xtal_structure_from_file",METH_VARARGS,&
            c_funloc(py_xtal_structure_from_file))
        call method_Table%add_method("ganu_from_xz",&
            "py_ganu_from_xz",METH_VARARGS,&
            c_funloc(py_ganu_from_xz))

        ! Build mod_Def
        m = mod_Def%init("pycrysfml08","A Python API for CrysFML08",method_Table)

    end function Init

end module api_init