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

module py_cfml_bckpeaks

    use forpy_mod
    use iso_c_binding

    use cfml_bckpeaks, only: automatic_pkb_search,pkb_type
    use cfml_diffpatt, only: diffpat_type,diffpat_e_type,diffpat_g_type
    use cfml_globaldeps, only: err_cfml,clear_error
    use cfml_python, only: check_number_of_arguments,get_var_from_item,unwrap_dict_item,unwrap_diffpat_type,wrap_pkb_type
   
    implicit none

    type(PythonModule), save :: mod_bckpeaks
    type(PythonMethodTable), save :: table_bckpeaks

    contains

    ! Initialization function for Python 3
    ! Called when importing module
    ! Must use bind(c, name="PyInit_<module name>")
    ! Return value must be type(c_ptr),
    ! use the return value of PythonModule%init
    function PyInit_py_cfml_bckpeaks() bind(c,name="PyInit_py_cfml_bckpeaks") result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_bckpeaks

        ! Local variables
        type(c_ptr) :: m

        m = Init()
    
    end function PyInit_py_cfml_bckpeaks

    function Init() result(m)

        ! Local variables
        type(c_ptr) :: m
        integer :: ierror

        ierror = Forpy_Initialize()

        ! Build method table
        call table_bckpeaks%init(1)
        call table_bckpeaks%add_method("automatic_pkb_search","py_automatic_pkb_search",METH_VARARGS,c_funloc(py_automatic_pkb_search))

        ! Build mod_bckpeaks
        m = mod_bckpeaks%init("py_cfml_bckpeaks","A Python API for CrysFML08",table_bckpeaks)

    end function Init

    function py_automatic_pkb_search(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(dict)    :: di_pat
        real          :: x1
        real          :: x2
        character(len=:), allocatable :: mode
        type(dict)    :: di_kwargs

        ! Local variables
        integer, parameter :: NMANDATORY = 4
        character(len=:), allocatable :: fortran_type
        integer :: ierror,narg
        logical :: is_print_pkb,print_pkb
        class(diffpat_type), allocatable :: pat
        type(pkb_type) :: pkb
        type(dict) :: di_pkb
        type(tuple) :: args,ret
        type(object) :: item

        ierror = 0
        is_print_pkb = .false.
        ierror = dict_create(di_pkb)
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Get arguments and check type signature
        call check_number_of_arguments('py_automatic_pkb_search',args,NMANDATORY,narg,ierror)

        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_automatic_pkb_search','pat',item,di_pat,ierror)
        if (ierror == 0) call unwrap_diffpat_type(di_pat,pat,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_automatic_pkb_search','x1',item,x1,ierror)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) call get_var_from_item('py_automatic_pkb_search','x2',item,x2,ierror)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) call get_var_from_item('py_automatic_pkb_search','mode',item,mode,ierror)
        if (ierror == 0 .and. narg > NMANDATORY) then
            ierror = args%getitem(item,4)
            if (ierror == 0) call get_var_from_item('py_automatic_pkb_search','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) then
                call unwrap_dict_item('py_automatic_pkb_search','print_pkb',di_kwargs,print_pkb,ierror)
                if (ierror == 0) then
                    is_print_pkb = .true.
                else
                    call clear_error()
                    call err_clear()
                    ierror = 0
                end if
            end if
        end if
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_automatic_pkb_search: error parsing arguments'
        end if

        ! Check types
        if (ierror == 0) then
            ierror = di_pat%getitem(fortran_type,"fortran_type")
            if (fortran_type == 'diffpat_type') then
                ierror = -1
                err_cfml%ierr = ierror
                err_cfml%msg = 'py_automatic_pkb_search: pat dictionary must have fortran_type = diffpat_e_type or diffpat_g_type'
            end if
        end if

        ! Calling Fortran procedure
        if (ierror == 0) then
            select type (A => pat)
                class is (diffpat_e_type)
                    if (.not. is_print_pkb) then
                        call automatic_pkb_search(A,x1,x2,mode,pkb)
                    else
                        call automatic_pkb_search(A,x1,x2,mode,pkb,print_pkb)
                    end if
            end select
        end if

        if (ierror == 0) call wrap_pkb_type(pkb,di_pkb,ierror)

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,di_pkb)
        resul = ret%get_c_ptr()

    end function py_automatic_pkb_search

end module py_cfml_bckpeaks
