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
! Authors: Nebil A. Katcho (ILL)
!          Juan Rodriguez-Carvajal (ILL)
!
!
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

module extension_cfml_reflections

    use forpy_mod
    use iso_c_binding

    use cfml_globaldeps, only: clear_error,err_cfml
    use cfml_gspacegroups, only: spg_type,set_spacegroup
    use cfml_metrics, only: cell_g_type,set_crystal_cell
    use cfml_reflections, only: reflist_type,hkl_gen_sxtal,wrap_reflist_type
    use extension_cfml_messages, only: check_error

    implicit none

    type(PythonModule), save :: mod_ext_reflections
    type(PythonMethodTable), save :: table_ext_reflections

    contains

    ! Initialization function for Python 3
    ! Called when importing module
    ! Must use bind(c, name="PyInit_<module name>")
    ! Return value must be type(c_ptr),
    ! use the return value of PythonModule%init
    function PyInit_py_extension_cfml_reflections() bind(c,name="PyInit_py_extension_cfml_reflections") result(m)
        !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_extension_cfml_reflections

        type(c_ptr) :: m

        m = Init()

    end function PyInit_py_extension_cfml_reflections

    function Init() result(m)

        ! Local variables
        type(c_ptr) :: m
        integer :: ierror

        ierror = Forpy_Initialize()

        ! Build method table
        call table_ext_reflections%init(1)
        call table_ext_reflections%add_method("gen_hkl_from_spg",&
            "gen_hkl_from_spg",METH_VARARGS,&
            c_funloc(py_gen_hkl_from_spg))

        ! Build mod_ext_reflections
        m = mod_ext_reflections%init("py_extension_cfml_reflections","Extension of module CFML_Reflections of CrysFML08",table_ext_reflections)

    end function Init

    function py_gen_hkl_from_spg(self_ptr,args_ptr) result(resul) bind(c)
        !! author: ILL Scientific Computing Group
        !! date: 24/03/2023
        !! display: public
        !! proc_internals: true
        !! summary: Generate a list of reflections for a given space group
        !!
        !! Generate a list of reflections for a given space group.
        !!
        !! ARGS_PTR = (spg_symb,cell,sintlmin,sintlmax)
        !!
        !! RESUL = (ierr,err_cfml%msg,di_hkls)
        !  --------           -----------         -----------
        !  Variable           Python type         Description
        !  --------           -----------         -----------
        !  spg_symb           string              space group symbol
        !  nd_cell            ndarray(6,float32)  cell parameters (a,b,c,alpha,beta,gamma)
        !  sintlmin           float               minimum vale for sin(theta) / lambda
        !  sintlmax           float               maximum value for sin(theta) / lambda
        !  ierr               integer             if ierr /= 0, an error occurred
        !  err_cfml%msg       string              error message
        !  di_hkls            dictionary          wrap of crysfml type reflist_type

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Python / Fortran interface variables
        character(len=:), allocatable :: spg_symb !! space group symbol
        type(ndarray)                 :: nd_cell  !! cell parameters (a,b,c,alpha,beta,gamma)
        real                          :: stlmin   !! minimum value for sin(theta) / lambda
        real                          :: stlmax   !! maximum value for sin(theta) / lambda
        integer                       :: ierr     !! error flag
        type(dict)                    :: di_hkls  !! list of calculated reflections => reflist_type

        ! Local variables
        integer :: ierror
        real, dimension(:), pointer :: p_cell
        type(cell_g_type) :: cell
        type(spg_type) :: spg
        type(reflist_type) :: hkls
        type(object) :: item
        type(tuple) :: args,ret

        ! Reset error variable
        ierr = 0
        ierror = 0
        call clear_error()

        ! In case of exception return C_NULL_PTR
        resul = C_NULL_PTR

        ! Get arguments
        call unsafe_cast_from_c_ptr(args,args_ptr)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) ierror = cast(spg_symb,item)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(nd_cell,item)
        if (ierror == 0) ierror = nd_cell%get_data(p_cell)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) ierror = cast(stlmin,item)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) ierror = cast(stlmax,item)
        if (ierror /= 0) then
            ierror = EXCEPTION_ERROR
            call raise_exception(RuntimeError,'py_gen_hkl_from_spg: Error getting !arguments')
        end if

        if (ierror == 0) call set_spacegroup('P 1',spg)
        if (ierror == 0) call check_error('py_gen_hkl_from_spg',ierror)
        if (ierror == 0) call set_crystal_cell(p_cell(1:3),p_cell(4:6),cell)
        if (ierror == 0) call check_error('py_gen_hkl_from_spg',ierror)
        if (ierror == 0) call hkl_gen_sxtal(cell,spg,stlmin,stlmax,hkls)
        if (ierror == 0) call check_error('py_gen_hkl_from_spg',ierror)
        if (ierror == 0) ierror = dict_create(di_hkls)
        if (ierror == 0) call wrap_reflist_type(hkls,di_hkls)
        if (ierror == 0) call check_error('py_gen_hkl_from_spg',ierror)

        ! Return tuple
        if (ierror == 0) then
            ierror = tuple_create(ret,3)
            ierror = ret%setitem(0,ierr)
            ierror = ret%setitem(1,trim(err_cfml%msg))
            ierror = ret%setitem(2,di_hkls)
        else
            ierr   = ierror
            ierror = tuple_create(ret,2)
            ierror = ret%setitem(0,ierr)
            ierror = ret%setitem(1,trim(err_cfml%msg))
        end if
        resul = ret%get_c_ptr()

    end function py_gen_hkl_from_spg

end module extension_cfml_reflections