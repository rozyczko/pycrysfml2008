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

module py_cfml_atoms

    use forpy_mod
    use iso_c_binding

    use cfml_atoms, only: atlist_type,check_symmetry_constraints
    use cfml_globaldeps, only: err_cfml,clear_error
    use cfml_gSpacegroups, only: spg_type,superspacegroup_type
    use cfml_metrics, only: cell_g_type
    use cfml_python, only: check_number_of_arguments,get_var_from_item,unwrap_atlist_type,wrap_atlist_type,unwrap_spg_type

    implicit none

    type(PythonModule), save :: mod_reflections
    type(PythonMethodTable), save :: table_atoms

    contains

    ! Initialization function for Python 3
    ! Called when importing module
    ! Must use bind(c, name="PyInit_<module name>")
    ! Return value must be type(c_ptr),
    ! use the return value of PythonModule%init
    function PyInit_py_cfml_atoms() bind(c,name="PyInit_py_cfml_atoms") result(m)
 #ifdef WIN32 
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_atoms
 #endif

        ! Local variables
        type(c_ptr) :: m

        m = Init()

    end function PyInit_py_cfml_atoms

    function Init() result(m)

        ! Local variables
        type(c_ptr) :: m
        integer :: ierror

        ierror = Forpy_Initialize()

        ! Build method table
        call table_atoms%init(1)
        call table_atoms%add_method("check_symmetry_constraints","py_check_symmetry_constraints",METH_VARARGS,c_funloc(py_check_symmetry_constraints))

        ! Build mod_reflections
        m = mod_reflections%init("py_cfml_atoms","A Python API for CrysFML08",table_atoms)

    end function Init

    function py_check_symmetry_constraints(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(dict)    :: di_spg
        type(dict)    :: di_atlist

        ! Local variables
        integer, parameter :: NMANDATORY = 2
        integer :: ierror,narg
        class(spg_type), allocatable :: spg
        type(atlist_type) :: atlist
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Get arguments
        call check_number_of_arguments('py_check_symmetry_constraints',args,NMANDATORY,narg,ierror)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_check_symmetry_constraints','spg',item,di_spg,ierror)
        if (ierror == 0) call unwrap_spg_type(di_spg,spg,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_check_symmetry_constraints','atlist',item,di_atlist,ierror)
        if (ierror == 0) call unwrap_atlist_type(di_atlist,atlist,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_check_symmetry_constraints: error parsing arguments'
        end if

        ! Calling Fortran procedure
        if (ierror == 0) call check_symmetry_constraints(spg,atlist)
        if (ierror == 0) ierror = err_cfml%ierr

        ! Wrap
        if (ierror == 0) call wrap_atlist_type(atlist,di_atlist,ierror)

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,2)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        resul = ret%get_c_ptr()

    end function py_check_symmetry_constraints

end module py_cfml_atoms
