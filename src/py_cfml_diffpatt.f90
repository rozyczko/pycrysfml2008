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

module py_cfml_diffpatt

    use forpy_mod
    use iso_c_binding

    use cfml_diffpatt, only: read_pattern,diffpat_g_type
    use cfml_globaldeps, only: err_cfml,clear_error
    use cfml_python, only: check_number_of_arguments,get_var_from_item,unwrap_dict_item,wrap_diffpat_type

    implicit none

    type(PythonModule), save :: mod_diffpatt
    type(PythonMethodTable), save :: table_diffpatt

    contains

    ! Initialization function for Python 3
    ! Called when importing module
    ! Must use bind(c, name="PyInit_<module name>")
    ! Return value must be type(c_ptr),
    ! use the return value of PythonModule%init
    function PyInit_py_cfml_diffpatt() bind(c,name="PyInit_py_cfml_diffpatt") result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_diffpatt

        ! Local variables
        type(c_ptr) :: m

        m = Init()

    end function PyInit_py_cfml_diffpatt

    function Init() result(m)

        ! Local variables
        type(c_ptr) :: m
        integer :: ierror

        ierror = Forpy_Initialize()

        ! Build method table
        call table_diffpatt%init(1)
        call table_diffpatt%add_method("read_pattern","py_read_pattern",METH_VARARGS,c_funloc(py_read_pattern))

        ! Build mod_diffpatt
        m = mod_diffpatt%init("py_cfml_diffpatt","A Python API for CrysFML08",table_diffpatt)

    end function Init

    function py_read_pattern(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr! Variables in args_ptr
        character(len=:), allocatable :: filename
        character(len=:), allocatable :: mode
        character(len=:), allocatable :: header
        integer :: npats
        logical :: sig
        type(dict)    :: di_kwargs

        ! Local variables
        integer, parameter :: NMANDATORY = 1
        integer :: ierror,narg,i
        logical :: single,is_kwargs,is_mode,is_sig,is_header
        type(diffpat_g_type) :: patt
        type(diffpat_g_type), dimension(:), allocatable :: patts
        type(dict) :: di_patt
        type(dict), dimension(:), allocatable :: di_patts
        type(list) :: li_patt
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        single = .true.
        is_mode = .false.
        is_sig = .false.
        is_header = .false.
        is_kwargs = .false.
        ierror = dict_create(di_patt)
        ierror = list_create(li_patt)
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Check the number of items
        call check_number_of_arguments('py_read_pattern',args,NMANDATORY,narg,ierror)
        if (ierror == 0) ierror = args%getitem(item,0)

        ! Get arguments
        if (ierror == 0) call get_var_from_item('py_read_pattern','filename',item,filename,ierror)
        if (ierror == 0 .and. narg > NMANDATORY) then
            ierror = args%getitem(item,1)
            if (ierror == 0) call get_var_from_item('py_read_pattern','npats',item,npats,ierror)
            if (ierror == 0) then
                single = .false.
                if (narg > 2) then
                    ierror = args%getitem(item,2)
                    if (ierror == 0) call get_var_from_item('py_read_pattern','kwargs',item,di_kwargs,ierror)
                    if (ierror == 0) is_kwargs = .true.
                end if
            else
                call get_var_from_item('py_read_pattern','kwargs',item,di_kwargs,ierror)
                if (ierror == 0) is_kwargs = .true.
            end if
            if (ierror == 0 .and. is_kwargs) then
                call unwrap_dict_item('py_read_pattern','mode',di_kwargs,mode,ierror)
                if (ierror /= 0) then
                    call err_clear()
                    call clear_error()
                    ierror = 0
                else
                    is_mode = .true.
                end if
                call unwrap_dict_item('py_read_pattern','sig',di_kwargs,sig,ierror)
                if (ierror /= 0) then
                    call err_clear()
                    call clear_error()
                    ierror = 0
                else
                    is_sig = .true.
                end if
                call unwrap_dict_item('py_read_pattern','header',di_kwargs,header,ierror)
                if (ierror /= 0) then
                    call err_clear()
                    call clear_error()
                    ierror = 0
                else
                    is_header = .true.
                end if
            end if
        end if
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_read_xtal_structure: error parsing arguments'
        end if

        ! Call Fortran procedure
        if (ierror == 0) then
            if (single) then
                if (.not. is_mode .and. .not. is_sig .and. .not. is_header) then
                    call read_pattern(filename,patt)
                else if (is_mode .and. .not. is_sig .and. .not. is_header) then
                    call read_pattern(filename,patt,mode=mode)
                else if (is_sig .and. .not. is_mode .and. .not. is_header) then
                    call read_pattern(filename,patt,sig=sig)
                else if (is_header .and. .not. is_sig .and. .not. is_mode) then
                    call read_pattern(filename,patt,header)
                else if (is_mode .and. is_sig .and. .not. is_header) then
                    call read_pattern(filename,patt,mode=mode,sig=sig)
                else if (is_mode .and. is_header .and. .not. is_sig) then
                    call read_pattern(filename,patt,mode=mode,header=header)
                else if (is_sig .and. is_header .and. .not. is_mode) then
                    call read_pattern(filename,patt,sig=sig,header=header)
                else if (is_mode .and. is_sig .and. is_header) then
                    call read_pattern(filename,patt,mode=mode,sig=sig,header=header)
                end if
            else
                allocate(patts(npats))
                if (.not. is_mode) then
                    call read_pattern(filename,patts,npats)
                else
                    call read_pattern(filename,patts,npats,mode)
                end if
            end if
            ierror = err_cfml%ierr
        end if

        if (ierror == 0) then
            if (single) then
                call wrap_diffpat_type(patt,di_patt,ierror)
            else
                allocate(di_patts(npats))
                do i = 1 , npats
                    ierror = dict_create(di_patts(i))
                    if (ierror == 0) call wrap_diffpat_type(patts(i),di_patts(i),ierror)
                    if (ierror == 0) ierror = li_patt%append(di_patts(i))
                end do
            end if
        end if

        ! Return
        if (ierror /= 0) call err_clear()
        if (single) then
            if (.not. is_header) then
                ierror = tuple_create(ret,3)
            else
                ierror = tuple_create(ret,4)
            end if
        else
            ierror = tuple_create(ret,3)
        end if
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        if (single) then
            ierror = ret%setitem(2,di_patt)
            if (is_header) ierror = ret%setitem(3,header)
        else
            ierror = ret%setitem(2,li_patt)
        end if
        resul = ret%get_c_ptr()

    end function py_read_pattern

end module py_cfml_diffpatt
