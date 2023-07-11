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

module py_cfml_gspacegroups

    use forpy_mod
    use iso_c_binding

    use cfml_globaldeps, only: err_cfml,clear_error
    use cfml_gSpaceGroups, only: spg_type,superspacegroup_type,init_spacegroup,set_spacegroup
    use cfml_python, only: wrap_group_type
    use cfml_rational, only: real
    use cfml_strings, only: u_case

    implicit none

    type(PythonModule), save :: mod_ioform
    type(PythonMethodTable), save :: table_ioform

    contains

    ! Initialization function for Python 3
    ! Called when importing module
    ! Must use bind(c, name="PyInit_<module name>")
    ! Return value must be type(c_ptr),
    ! use the return value of PythonModule%init
    function PyInit_py_cfml_gspacegroups() bind(c,name="PyInit_py_cfml_gspacegroups") result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_gspacegroups

        ! Local variables
        type(c_ptr) :: m

        m = Init()

    end function PyInit_py_cfml_gspacegroups

    function Init() result(m)

        ! Local variables
        type(c_ptr) :: m
        integer :: ierror

        ierror = Forpy_Initialize()

        ! Build method table
        call table_ioform%init(1)
        call table_ioform%add_method("set_spacegroup","py_set_spacegroup",METH_VARARGS,c_funloc(py_set_spacegroup))

        ! Build mod_ioform
        m = mod_ioform%init("py_cfml_gspacegroups","A Python API for CrysFML08",table_ioform)

    end function Init

    function py_set_spacegroup(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        character(len=:), allocatable :: generator !
        type(dict)                    :: di_spg    !! Space group

        ! Local variables
        integer, parameter :: NMANDATORY = 2
        integer :: ierror,narg,i,j,k
        logical :: is_setting
        character(len=5) :: mode
        character(len=180) :: setting
        character(len=:), allocatable :: key
        class(spg_type), allocatable :: spg
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        mode = ''
        is_setting = .false.
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Check the number of items
        ierror = args%len(narg)
        if (narg < NMANDATORY) then
            ierror = -1
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_read_setspacegroup: insufficient number of arguments'
        end if

        ! Check types
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) then
            if (.not. is_str(item)) then
                ierror = -1
                err_cfml%ierr = ierror
                err_cfml%msg = 'py_read_setspacegroup: first argument must be a string'
            else
                ierror = cast(generator,item)
            end if
        end if
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) then
            if (.not. is_dict(item)) then
                ierror = -1
                err_cfml%ierr = ierror
                err_cfml%msg = 'py_read_setspacegroup: second argument must be a dictionary'
            else
                ierror = cast(di_spg,item)
                if (ierror == 0) call di_spg%clear()
            end if
        end if
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_read_setspacegroup: error parsing arguments'
        end if

        ! Syntax analysis of generator and call to set_spacegroup
        if (ierror == 0) then
            generator = trim(generator)
            generator = adjustl(generator)
            i = index(generator,' ')
            if (i > 0) then
                key = u_case(generator(1:i-1))
                generator = adjustl(generator(i:))
                select case (key)
                case ('HALL','MHALL','SPGR','SPACEG')
                    allocate(spg_type :: spg)
                    mode = 'symb'
                case ('SHUB')
                    allocate(spg_type :: spg)
                    mode = 'shubn'
                case ('SSG','SUPER','SSPG')
                    allocate(superspacegroup_type :: spg)
                    mode = 'super'
                case ('GEN')
                    ! Get first generator
                    j = index(generator,'t')
                    k = index(generator,'x4')
                    if (j > 0 .or. k > 0) then
                        allocate(superspacegroup_type :: spg)
                    else
                        allocate(spg_type :: spg)
                    end if
                    mode = 'gen'
                case default
                    ierror = -1
                    err_cfml%ierr = ierror
                    err_cfml%msg = 'py_read_setspacegroup: error parsing generator. Keyword '//key//' unknown'
                end select
            else
                ierror = -1
                err_cfml%ierr = ierror
                err_cfml%msg = 'py_read_setspacegroup: error parsing generator.'
            end if
        end if
        if (ierror == 0) then
            i = index(generator,'::')
            if (i /= 0) then
                setting = adjustl(trim(generator(i+2:)))
                if (len(setting) > 0) is_setting = .true.
                generator = generator(:i-1)
            end if
        end if

        ! Call Fortran procedure
        if (ierror == 0) then
            call init_spacegroup(spg)
            select case (trim(mode))
            case ('shubn','super')
                if (is_setting) then
                    call set_spacegroup(generator,mode,spg,setting=setting)
                else
                    call set_spacegroup(generator,mode,spg)
                end if
            case default
                call set_spacegroup(generator,spg)
            end select
            ierror = err_cfml%ierr
        end if

        ! Wrapping
        if (ierror == 0) call wrap_group_type(spg,di_spg)

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,2)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        resul = ret%get_c_ptr()

    end function py_set_spacegroup

end module py_cfml_gspacegroups
