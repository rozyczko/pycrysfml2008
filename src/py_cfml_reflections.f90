!-------------------------------------------------------------
! PyCrysFML08
! -------------------------------------------------------------
! This file is part of PyCrysFML08
!
! The PyCrysFML08 is distributed under LGPL. In agreement with the
! Intergovernmental Convention of the ILL, this software cannot be used
! in military applications.
!
! PyCrysFML08 is based on Elias Rabel work for Forpy, see <https://github.com/ylikx/forpy>.
!
! Copyright (C) 2020-2022  Institut Laue-Langevin (ILL), Grenoble, FRANCE
!
! Authors: ILL Scientific Computing Group (ILL)
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 3.0 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, see <http://www.gnu.org/licenses/>.
!
! -------------------------------------------------------------

module py_cfml_reflections

    use forpy_mod
    use iso_c_binding

    use cfml_globaldeps
    use cfml_messages
    use cfml_reflections
    use extension_cfml_reflections

    implicit none

    type(PythonModule), save :: mod_reflections
    type(PythonMethodTable), save :: table_reflections

    contains

    ! Initialization function for Python 3
    ! Called when importing module
    ! Must use bind(c, name="PyInit_<module name>")
    ! Return value must be type(c_ptr),
    ! use the return value of PythonModule%init
    function PyInit_py_cfml_reflections() bind(c,name="PyInit_py_cfml_reflections") result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_reflections

        ! Local variables
        type(c_ptr) :: m

        m = Init()

    end function PyInit_py_cfml_reflections

    function Init() result(m)

        ! Local variables
        type(c_ptr) :: m
        integer :: ierror

        ierror = Forpy_Initialize()

        ! Build method table
        call table_reflections%init(1)
        call table_reflections%add_method("hkls_from_spg","py_hkls_from_spg",METH_VARARGS,c_funloc(py_hkls_from_spg))

        ! Build mod_reflections
        m = mod_reflections%init("py_cfml_reflections","A Python API for CrysFML08",table_reflections)

    end function Init

end module py_cfml_reflections
