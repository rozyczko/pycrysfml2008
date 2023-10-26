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

    use cfml_atoms, only: atlist_type,check_symmetry_constraints,extend_atom_list,atom_equiv_list_type
    use cfml_globaldeps, only: err_cfml,clear_error
    use cfml_gSpacegroups, only: spg_type,superspacegroup_type
    use cfml_metrics, only: cell_g_type
    use cfml_python, only: check_number_of_arguments,get_var_from_item,unwrap_atlist_type,wrap_atlist_type,unwrap_spg_type,&
                           wrap_atom_equiv_list_type,unwrap_cell_g_type,unwrap_dict_item
    use cfml_strings, only: l_case

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
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_atoms

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
        call table_atoms%init(2)
        call table_atoms%add_method("check_symmetry_constraints","py_check_symmetry_constraints",METH_VARARGS,c_funloc(py_check_symmetry_constraints))
        call table_atoms%add_method("extend_atom_list","py_extend_atom_list",METH_VARARGS,c_funloc(py_extend_atom_list))
        
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

    function py_extend_atom_list(self_ptr,args_ptr) result(resul) bind(c)
       
        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(dict)    :: di_spg
        type(dict)    :: di_cell
        type(dict)    :: di_a
        type(dict)    :: di_kwargs
        character(len=:), allocatable :: type_atm

        ! Local variables
        integer, parameter :: NMANDATORY = 3
        integer :: ierror,narg,lun
        logical :: is_lun,is_conven,conven,atom_equiv
        character(len=:), allocatable :: fortran_type
        class(spg_type), allocatable :: spg
        class(cell_g_type), allocatable :: cell
        type(atlist_type) :: a,b
        type(atom_equiv_list_type) :: ate
        type(object) :: item
        type(tuple) :: args, ret
        type(dict) :: di_ate
        type(dict) :: di_b

        ierror = 0
        atom_equiv = .false.
        is_lun = .false.
        is_conven = .false.
        ierror = dict_create(di_ate)
        ierror = dict_create(di_b)
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Get arguments and check type signature
        call check_number_of_arguments('py_extend_atom_list',args,NMANDATORY,narg,ierror)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_extend_atom_list','spg',item,di_spg,ierror)
        ierror = di_spg%getitem(fortran_type,"fortran_type")

        if (fortran_type == 'spg_type') then
            atom_equiv = .true.
        else if (fortran_type == 'atlist_type') then
            atom_equiv = .false.
        else
            ierror = -1
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_extend_atom_list: spg or a dictionaries must be of fortran type spg_type or atlist_type'
        end if  
        
        if (ierror == 0) then
            if (atom_equiv) then
                if (ierror == 0) call unwrap_spg_type(di_spg,spg,ierror)
                if (ierror == 0) ierror = args%getitem(item,1)
                if (ierror == 0) call get_var_from_item('py_extend_atom_list','cell',item,di_cell,ierror)
                if (ierror == 0) call unwrap_cell_g_type(di_cell,cell,ierror)
                if (ierror == 0) ierror = args%getitem(item,2)
                if (ierror == 0) call get_var_from_item('py_extend_atom_list','a',item,di_a,ierror)
                if (ierror == 0) call unwrap_atlist_type(di_a,a,ierror)
                if (ierror == 0 .and. narg > NMANDATORY) then
                    ierror = args%getitem(item,3)
                    if (ierror == 0) call get_var_from_item('py_extend_atom_list','kwargs',item,di_kwargs,ierror)
                    if (ierror == 0) then
                        call unwrap_dict_item('py_extend_atom_list','lun',di_kwargs,lun,ierror)
                        if (ierror == 0) then
                            is_lun = .true.
                        else
                            call clear_error()
                            call err_clear()
                            ierror = 0
                        end if
                    end if
                end if
            else
                call get_var_from_item('py_extend_atom_list','a',item,di_a,ierror)
                if (ierror == 0) call unwrap_atlist_type(di_a,a,ierror)
                if (ierror == 0) ierror = args%getitem(item,1)
                if (ierror == 0) call get_var_from_item('py_extend_atom_list','spg',item,di_spg,ierror)
                if (ierror == 0) call unwrap_spg_type(di_spg,spg,ierror)
                if (ierror == 0) ierror = args%getitem(item,2)
                if (ierror == 0) call get_var_from_item('py_extend_atom_list','type_atm',item,type_atm,ierror)
                if (ierror == 0 .and. narg > NMANDATORY) then
                    ierror = args%getitem(item,3)
                    if (ierror == 0) call get_var_from_item('py_extend_atom_list','kwargs',item,di_kwargs,ierror)
                    if (ierror == 0) then
                        call unwrap_dict_item('py_extend_atom_list','conven',di_kwargs,conven,ierror)
                        if (ierror == 0) then
                            is_conven = .true.
                        else
                            call clear_error()
                            call err_clear()
                            ierror = 0
                        end if
                    end if
                    if (ierror == 0) then
                        call unwrap_dict_item('py_extend_atom_list','lun',di_kwargs,lun,ierror)
                        if (ierror == 0) then
                            is_lun = .true.
                        else
                            call clear_error()
                            call err_clear()
                            ierror = 0
                        end if
                    end if
                end if
            end if
        end if
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_extend_atom_list: error parsing arguments'
        end if

        ! Check types
        if (ierror == 0) then
            if (atom_equiv) then
                ierror = di_spg%getitem(fortran_type,"fortran_type")
                if (fortran_type /= 'spg_type') then
                    ierror = -1
                    err_cfml%ierr = ierror
                    err_cfml%msg = 'py_extend_atom_list: spg dictionary must have fortran_type = spg_type'
                end if
                ierror = di_cell%getitem(fortran_type,"fortran_type")
                if (fortran_type /= 'cell_g_type') then
                    ierror = -1
                    err_cfml%ierr = ierror
                    err_cfml%msg = 'py_extend_atom_list: cell dictionary must have fortran_type = cell_g_type'
                end if
                ierror = di_a%getitem(fortran_type,"fortran_type")
                if (fortran_type /= 'atlist_type') then
                    ierror = -1
                    err_cfml%ierr = ierror
                    err_cfml%msg = 'py_extend_atom_list: a dictionary must have fortran_type = atlist_type'
                end if
            else
                ierror = di_a%getitem(fortran_type,"fortran_type")
                if (fortran_type /= 'atlist_type') then
                    ierror = -1
                    err_cfml%ierr = ierror
                    err_cfml%msg = 'py_extend_atom_list: a dictionary must have fortran_type = atlist_type'
                end if
                ierror = di_spg%getitem(fortran_type,"fortran_type")
                if (fortran_type /= 'spg_type') then
                    ierror = -1
                    err_cfml%ierr = ierror
                    err_cfml%msg = 'py_extend_atom_list: spg dictionary must have fortran_type = atlist_type'
                end if
                select case(trim(l_case(type_atm)))
                    case("atm_type")
                    case("atm_std_type")
                    case("modatm_std_type")
                    case("atm_ref_type")
                    case("modatm_ref_type")

                    case default
                        ierror = -1
                        err_cfml%ierr = ierror
                        err_cfml%msg = 'py_extend_atom_list: type_atm must take any of these values (atm_type, atm_std_type, modatm_std_type, atm_ref_type, modatm_ref_type)'
                end select
            end if
        end if
        ! Calling Fortran procedure
        if (ierror == 0) then
            if (atom_equiv) then
                if (is_lun) then
                    call extend_atom_list(spg,cell,a,ate,lun)
                else
                    call extend_atom_list(spg,cell,a,ate)
                end if
            else
                if (is_conven .and. is_lun) then
                    call extend_atom_list(a,b,spg,type_atm,conven=conven,lun=lun)
                else if (is_conven .and. .not. is_lun) then
                    call extend_atom_list(a,b,spg,type_atm,conven=conven)
                else if (.not. is_conven .and. is_lun) then
                    call extend_atom_list(a,b,spg,type_atm,lun=lun)
                else
                    call extend_atom_list(a,b,spg,type_atm)
                end if
            end if
        end if
        if (ierror == 0) ierror = err_cfml%ierr
        if (ierror == 0) then
            if (atom_equiv) then
                call wrap_atom_equiv_list_type(ate,di_ate,ierror)
            else
                call wrap_atlist_type(b,di_b,ierror)
            end if
        end if
        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        if (atom_equiv) then
            ierror = ret%setitem(2,di_ate)
        else
            ierror = ret%setitem(2,di_b)
        end if
        resul = ret%get_c_ptr()

    end function py_extend_atom_list

end module py_cfml_atoms
