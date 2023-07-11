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

module py_cfml_ioform

    use forpy_mod
    use iso_c_binding

    use cfml_atoms, only: atlist_type,matom_list_type
    use cfml_globaldeps, only: err_cfml,clear_error
    use cfml_gSpaceGroups, only: spg_type
    use cfml_ioform, only: read_xtal_structure
    use cfml_kvec_Symmetry, only: magsymm_k_type,magnetic_domain_type
    use cfml_metrics, only: cell_g_type
    use cfml_python, only: wrap_atlist_type,wrap_cell_type,wrap_group_type

    implicit none

    type(PythonModule), save :: mod_ioform
    type(PythonMethodTable), save :: table_ioform

    contains

    ! Initialization function for Python 3
    ! Called when importing module
    ! Must use bind(c, name="PyInit_<module name>")
    ! Return value must be type(c_ptr),
    ! use the return value of PythonModule%init
    function PyInit_py_cfml_ioform() bind(c,name="PyInit_py_cfml_ioform") result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_ioform

        ! Local variables
        type(c_ptr) :: m

        m = Init()

    end function PyInit_py_cfml_ioform

    function Init() result(m)

        ! Local variables
        type(c_ptr) :: m
        integer :: ierror

        ierror = Forpy_Initialize()

        ! Build method table
        call table_ioform%init(1)
        call table_ioform%add_method("read_xtal_structure","py_read_xtal_structure",METH_VARARGS,c_funloc(py_read_xtal_structure))

        ! Build mod_ioform
        m = mod_ioform%init("py_cfml_ioform","A Python API for CrysFML08",table_ioform)

    end function Init

    function py_read_xtal_structure(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        character(len=:), allocatable :: filenam    !! Name of the file
        type(dict)                    :: di_cell    !! Unit cell
        type(dict)                    :: di_spg     !! Space group
        type(dict)                    :: di_atm     !! Atoms
        type(dict)                    :: di_kwargs  !! Optional arguments
        character(len=:), allocatable :: atm_typ    !! Atom type (optional)
        type(dict)                    :: di_mgp     !!
        type(dict)                    :: di_matm    !!
        type(dict)                    :: di_mag_dom !!
        integer                       :: iphase     !! Number of the phase

        ! Local variables
        integer, parameter :: NMANDATORY = 4
        integer :: ierror,narg
        logical :: is_kwargs,is_atm_typ,is_mgp,is_matm,is_mag_dom,is_iphase
        type(magsymm_k_type) :: mgp
        type(matom_list_type) :: matm
        type(magnetic_domain_type) :: mag_dom
        type(object) :: item
        type(tuple) :: args,ret
        type(cell_g_type):: cell
        class(spg_type), allocatable :: spg
        type(atlist_type) :: atm

        ierror = 0
        is_kwargs = .false.
        is_atm_typ = .false.
        is_mgp = .false.
        is_matm = .false.
        is_mag_dom = .false.
        is_iphase = .false.
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Check the number of items
        ierror = args%len(narg)
        if (narg < NMANDATORY) then
            ierror = -1
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_read_xtal_structure: insufficient number of arguments'
        end if

        ! Get mandatory arguments
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) ierror = cast(filenam,item)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(di_cell,item)
        if (ierror == 0) call di_cell%clear()
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) ierror = cast(di_spg,item)
        if (ierror == 0) call di_spg%clear()
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) ierror = cast(di_atm,item)
        if (ierror == 0) call di_atm%clear()

        ! Get optional arguments, if any
        if (ierror == 0 .and. narg > NMANDATORY) then
            ierror = args%getitem(item,4)
            if (ierror == 0) ierror = cast(di_kwargs,item)
            if (ierror == 0) then
                ierror = di_kwargs%getitem(atm_typ,"atm_typ")
                if (ierror /= 0) then
                    call err_clear()
                else
                    is_atm_typ = .true.
                end if
                ierror = di_kwargs%getitem(item,"mgp")
                if (ierror == 0) ierror = cast(di_mgp,item)
                if (ierror /= 0) then
                    call err_clear()
                else
                    is_mgp = .true.
                    call di_mgp%clear()
                end if
                ierror = di_kwargs%getitem(item,"matm")
                if (ierror == 0) ierror = cast(di_matm,item)
                if (ierror /= 0) then
                    call err_clear()
                else
                    is_matm = .true.
                    call di_matm%clear()
                end if
                ierror = di_kwargs%getitem(item,"mag_dom")
                if (ierror == 0) ierror = cast(di_mag_dom,item)
                if (ierror /= 0) then
                    call err_clear()
                else
                    is_mag_dom = .true.
                    call di_mag_dom%clear()
                end if
                ierror = di_kwargs%getitem(iphase,"iphase")
                if (ierror /= 0) then
                    call err_clear()
                else
                    is_iphase = .true.
                end if
            end if
        end if

        if (ierror /= 0 .and. narg >= NMANDATORY) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_read_xtal_structure: error while casting arguments'
        end if

        if (ierror == 0) then
            ! Call Fortran procedure
            if (narg == NMANDATORY) then
                call read_xtal_structure(filenam,cell,spg,atm)
            else if (is_iphase) then
                if (is_atm_typ) then
                    call read_xtal_structure(filenam,cell,spg,atm,atm_typ,iphase=iphase)
                else
                    call read_xtal_structure(filenam,cell,spg,atm,iphase=iphase)
                end if
            else if (is_mgp .and. is_matm) then
                if (is_mag_dom) then
                    call read_xtal_structure(filenam,cell,spg,atm,mgp=mgp,matm=matm,mag_dom=mag_dom)
                else
                    call read_xtal_structure(filenam,cell,spg,atm,mgp=mgp,matm=matm)
                end if
            end if
        end if

        if (ierror == 0) then
            ! Wrapping
            call wrap_cell_type(cell,di_cell)
            call wrap_atlist_type(atm,di_atm)
            call wrap_group_type(spg,di_spg)
        end if

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,2)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        resul = ret%get_c_ptr()

    end function py_read_xtal_structure

end module py_cfml_ioform
