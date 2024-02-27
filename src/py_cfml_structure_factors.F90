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

    use cfml_atoms, only: atlist_type
    use cfml_globaldeps, only: err_cfml,clear_error
    use cfml_gSpacegroups, only: spg_type,superspacegroup_type
    use cfml_python, only: check_number_of_arguments,get_var_from_item,ndarray_to_pointer,pointer_to_array,&
                           unwrap_atlist_type,unwrap_dict_item,unwrap_reflist_type,unwrap_spg_type
    use cfml_reflections, only: reflist_type
    use cfml_structure_factors, only: calc_hkl_strfactor,calc_strfactor,init_calc_hkl_strfactors,init_calc_strfactors,&
                                      init_structure_factors,structure_factors

    implicit none

    type(PythonModule), save :: mod_reflections
    type(PythonMethodTable), save :: table_structure_factors

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
        call table_structure_factors%init(6)
        call table_structure_factors%add_method("calc_hkl_strfactor","py_calc_hkl_strfactor",METH_VARARGS,c_funloc(py_calc_hkl_strfactor))
        call table_structure_factors%add_method("calc_strfactor","py_calc_strfactor",METH_VARARGS,c_funloc(py_calc_strfactor))
        call table_structure_factors%add_method("init_calc_hkl_strfactors","py_init_calc_hkl_strfactors",METH_VARARGS,c_funloc(py_init_calc_hkl_strfactors))
        call table_structure_factors%add_method("init_calc_strfactors","py_init_calc_strfactors",METH_VARARGS,c_funloc(py_init_calc_strfactors))
        call table_structure_factors%add_method("init_structure_factors","py_init_structure_factors",METH_VARARGS,c_funloc(py_init_structure_factors))
        call table_structure_factors%add_method("structure_factors","py_structure_factors",METH_VARARGS,c_funloc(py_structure_factors))

        ! Build mod_reflections
        m = mod_reflections%init("py_cfml_structure_factors","A Python API for CrysFML08",table_structure_factors)

    end function Init

    function py_calc_hkl_strfactor(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(ndarray)                 :: nd_nn
        real                          :: sn
        type(dict)                    :: di_spg
        type(dict)                    :: di_atlist
        character(len=:), allocatable :: mode
        character(len=:), allocatable :: rad
        type(dict)                    :: di_kwargs

        ! Local variables
        integer, parameter :: NMANDATORY = 6
        integer :: ierror,narg
        integer, dimension(3) :: nn
        integer, dimension(:), pointer :: p_nn
        real :: sf2
        logical :: is_deriv,is_fc
        class(spg_type), allocatable :: spg
        type(atlist_type) :: atlist
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        is_deriv= .false.
        is_fc = .false.
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Get arguments
        call check_number_of_arguments('py_calc_hkl_strfactor',args,NMANDATORY,narg,ierror)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_calc_hkl_strfactor','nn',item,nd_nn,ierror)
        if (ierror == 0) call ndarray_to_pointer('py_calc_hkl_strfactor','nn',nd_nn,p_nn,ierror)
        if (ierror == 0) call pointer_to_array('py_calc_hkl_strfactor','nn',p_nn,nn,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_calc_hkl_strfactor','sn',item,sn,ierror)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) call get_var_from_item('py_calc_hkl_strfactor','atlist',item,di_atlist,ierror)
        if (ierror == 0) call unwrap_atlist_type(di_atlist,atlist,ierror)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) call get_var_from_item('py_calc_hkl_strfactor','spg',item,di_spg,ierror)
        if (ierror == 0) call unwrap_spg_type(di_spg,spg,ierror)
        if (ierror == 0) then
            select type (A => spg)
                class is (superspacegroup_type)
                    ierror = -1
                    err_cfml%ierr = ierror
                    err_cfml%msg = 'py_calc_hkl_strfactor: fortran_type in spg must be spg_type'
            end select
        end if
        if (ierror == 0) ierror = args%getitem(item,4)
        if (ierror == 0) call get_var_from_item('py_calc_hkl_strfactor','mode',item,mode,ierror)
        if (ierror == 0) ierror = args%getitem(item,5)
        if (ierror == 0) call get_var_from_item('py_calc_hkl_strfactor','rad',item,rad,ierror)
        if (ierror == 0 .and. narg > NMANDATORY) then
            if (ierror == 0) ierror = args%getitem(item,NMANDATORY)
            if (ierror == 0) call get_var_from_item('py_calc_hkl_strfactor','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) then
            end if
        end if
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_calc_hkl_strfactor: error parsing arguments'
        end if

        ! Calling Fortran procedure
        if (ierror == 0) then
            if (.not. is_deriv .and. .not. is_fc) then
                call calc_hkl_strfactor(nn,sn,atlist,spg,mode,rad,sf2)
            else if (is_deriv .and. .not. is_fc) then
                !call calc_hkl_strfactor(nn,sn,atlist,spg,mode,rad,sf2,deriv)
            else if (.not. is_deriv .and. is_fc) then
                !call calc_hkl_strfactor(nn,sn,atlist,spg,mode,rad,sf2,fc=fc)
            else
                !call calc_hkl_strfactor(nn,sn,atlist,spg,mode,rad,sf2,deriv,fc)
            end if
            if (ierror == 0) ierror = err_cfml%ierr
        end if

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,sf2)
        resul = ret%get_c_ptr()

    end function py_calc_hkl_strfactor

    function py_calc_strfactor(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        integer                       :: nn
        real                          :: sn
        type(dict)                    :: di_spg
        type(dict)                    :: di_atlist
        character(len=:), allocatable :: mode
        character(len=:), allocatable :: rad
        type(dict)                    :: di_kwargs

        ! Local variables
        integer, parameter :: NMANDATORY = 6
        integer :: ierror,narg
        real :: sf2
        logical :: is_deriv,is_fc
        class(spg_type), allocatable :: spg
        type(atlist_type) :: atlist
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        is_deriv= .false.
        is_fc = .false.
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Get arguments
        call check_number_of_arguments('py_calc_strfactor',args,NMANDATORY,narg,ierror)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_calc_strfactor','nn',item,nn,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_calc_strfactor','sn',item,sn,ierror)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) call get_var_from_item('py_calc_strfactor','atlist',item,di_atlist,ierror)
        if (ierror == 0) call unwrap_atlist_type(di_atlist,atlist,ierror)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) call get_var_from_item('py_calc_strfactor','spg',item,di_spg,ierror)
        if (ierror == 0) call unwrap_spg_type(di_spg,spg,ierror)
        if (ierror == 0) then
            select type (A => spg)
                class is (superspacegroup_type)
                    ierror = -1
                    err_cfml%ierr = ierror
                    err_cfml%msg = 'py_calc_strfactor: fortran_type in spg must be spg_type'
            end select
        end if
        if (ierror == 0) ierror = args%getitem(item,4)
        if (ierror == 0) call get_var_from_item('py_calc_strfactor','mode',item,mode,ierror)
        if (ierror == 0) ierror = args%getitem(item,5)
        if (ierror == 0) call get_var_from_item('py_calc_strfactor','rad',item,rad,ierror)
        if (ierror == 0 .and. narg > NMANDATORY) then
            if (ierror == 0) ierror = args%getitem(item,NMANDATORY)
            if (ierror == 0) call get_var_from_item('py_calc_strfactor','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) then
            end if
        end if
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_calc_strfactor: error parsing arguments'
        end if

        ! Calling Fortran procedure
        if (ierror == 0) then
            if (.not. is_deriv .and. .not. is_fc) then
                call calc_strfactor(nn,sn,atlist,spg,mode,rad,sf2)
            else if (is_deriv .and. .not. is_fc) then
                !call calc_strfactor(nn,sn,atlist,spg,mode,rad,sf2,deriv)
            else if (.not. is_deriv .and. is_fc) then
                !call calc_strfactor(nn,sn,atlist,spg,mode,rad,sf2,fc=fc)
            else
                !call calc_strfactor(nn,sn,atlist,spg,mode,rad,sf2,deriv,fc)
            end if
            if (ierror == 0) ierror = err_cfml%ierr
        end if

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,sf2)
        resul = ret%get_c_ptr()

    end function py_calc_strfactor

    function py_init_calc_hkl_strfactors(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(dict)    :: di_spg
        type(dict)    :: di_atlist
        type(dict)    :: di_kwargs

        ! Local variables
        integer, parameter :: NMANDATORY = 2
        integer :: ierror,narg
        real :: lambda
        logical :: is_mode,is_lambda
        character(len=:), allocatable :: mode
        class(spg_type), allocatable :: spg
        type(atlist_type) :: atlist
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        is_mode = .false.
        is_lambda = .false.
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Get arguments
        call check_number_of_arguments('py_init_calc_hkl_strfactors',args,NMANDATORY,narg,ierror)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_init_calc_hkl_strfactors','atlist',item,di_atlist,ierror)
        if (ierror == 0) call unwrap_atlist_type(di_atlist,atlist,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_init_calc_hkl_strfactors','spg',item,di_spg,ierror)
        if (ierror == 0) call unwrap_spg_type(di_spg,spg,ierror)
        if (ierror == 0) then
            select type (A => spg)
                class is (superspacegroup_type)
                    ierror = -1
                    err_cfml%ierr = ierror
                    err_cfml%msg = 'py_init_calc_hkl_strfactors: fortran_type in spg must be spg_type'
            end select
        end if
        if (ierror == 0 .and. narg > NMANDATORY) then
            if (ierror == 0) ierror = args%getitem(item,NMANDATORY)
            if (ierror == 0) call get_var_from_item('py_init_calc_hkl_strfactors','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) then
                call unwrap_dict_item('py_init_calc_hkl_strfactors','mode',di_kwargs,mode,ierror)
                if (ierror /= 0) then
                    call err_clear()
                    call clear_error()
                    ierror = 0
                else
                    is_mode = .true.
                end if
                call unwrap_dict_item('py_init_calc_hkl_strfactors','lambda',di_kwargs,lambda,ierror)
                if (ierror /= 0) then
                    call err_clear()
                    call clear_error()
                    ierror = 0
                else
                    is_lambda = .true.
                end if
            end if
        end if
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_init_calc_hkl_strfactors: error parsing arguments'
        end if

        ! Calling Fortran procedure
        if (ierror == 0) then
            if (.not. is_mode .and. .not. is_lambda) then
                call init_calc_hkl_strfactors(atlist,spg)
            else if (is_mode .and. .not. is_lambda) then
                call init_calc_hkl_strfactors(atlist,spg,mode)
            else if (.not. is_mode .and. is_lambda) then
                call init_calc_hkl_strfactors(atlist,spg,lambda=lambda)
            else
                call init_calc_hkl_strfactors(atlist,spg,mode,lambda)
            end if
            if (ierror == 0) ierror = err_cfml%ierr
        end if

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,2)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        resul = ret%get_c_ptr()

    end function py_init_calc_hkl_strfactors

    function py_init_calc_strfactors(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(dict)    :: di_refl
        type(dict)    :: di_spg
        type(dict)    :: di_atlist
        type(dict)    :: di_kwargs

        ! Local variables
        integer, parameter :: NMANDATORY = 3
        integer :: ierror,narg
        real :: lambda
        logical :: is_mode,is_lambda
        character(len=:), allocatable :: mode
        class(spg_type), allocatable :: spg
        type(atlist_type) :: atlist
        type(reflist_type) :: refl
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        is_mode = .false.
        is_lambda = .false.
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Get arguments
        call check_number_of_arguments('py_init_calc_strfactors',args,NMANDATORY,narg,ierror)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_init_calc_strfactors','refl',item,di_refl,ierror)
        if (ierror == 0) call unwrap_reflist_type(di_refl,refl,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_init_calc_strfactors','atlist',item,di_atlist,ierror)
        if (ierror == 0) call unwrap_atlist_type(di_atlist,atlist,ierror)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) call get_var_from_item('py_init_calc_strfactors','spg',item,di_spg,ierror)
        if (ierror == 0) call unwrap_spg_type(di_spg,spg,ierror)
        if (ierror == 0) then
            select type (A => spg)
                class is (superspacegroup_type)
                    ierror = -1
                    err_cfml%ierr = ierror
                    err_cfml%msg = 'py_init_calc_strfactors: fortran_type in spg must be spg_type'
            end select
        end if
        if (ierror == 0 .and. narg > NMANDATORY) then
            if (ierror == 0) ierror = args%getitem(item,NMANDATORY)
            if (ierror == 0) call get_var_from_item('py_init_calc_strfactors','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) then
                call unwrap_dict_item('py_init_calc_strfactors','mode',di_kwargs,mode,ierror)
                if (ierror /= 0) then
                    call err_clear()
                    call clear_error()
                    ierror = 0
                else
                    is_mode = .true.
                end if
                call unwrap_dict_item('py_init_calc_strfactors','lambda',di_kwargs,lambda,ierror)
                if (ierror /= 0) then
                    call err_clear()
                    call clear_error()
                    ierror = 0
                else
                    is_lambda = .true.
                end if
            end if
        end if
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_init_calc_strfactors: error parsing arguments'
        end if

        ! Calling Fortran procedure
        if (ierror == 0) then
            if (.not. is_mode .and. .not. is_lambda) then
                call init_calc_strfactors(refl,atlist,spg)
            else if (is_mode .and. .not. is_lambda) then
                call init_calc_strfactors(refl,atlist,spg,mode)
            else if (.not. is_mode .and. is_lambda) then
                call init_calc_strfactors(refl,atlist,spg,lambda=lambda)
            else
                call init_calc_strfactors(refl,atlist,spg,mode,lambda)
            end if
            if (ierror == 0) ierror = err_cfml%ierr
        end if

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,2)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        resul = ret%get_c_ptr()

    end function py_init_calc_strfactors

    function py_init_structure_factors(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(dict)    :: di_refl
        type(dict)    :: di_spg
        type(dict)    :: di_atlist
        type(dict)    :: di_kwargs

        ! Local variables
        integer, parameter :: NMANDATORY = 3
        integer :: ierror,narg
        real :: lambda
        logical :: is_mode,is_lambda
        character(len=:), allocatable :: mode
        class(spg_type), allocatable :: spg
        type(atlist_type) :: atlist
        type(reflist_type) :: refl
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        is_mode = .false.
        is_lambda = .false.
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Get arguments
        call check_number_of_arguments('py_init_structure_factors',args,NMANDATORY,narg,ierror)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_init_structure_factors','refl',item,di_refl,ierror)
        if (ierror == 0) call unwrap_reflist_type(di_refl,refl,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_init_structure_factors','atlist',item,di_atlist,ierror)
        if (ierror == 0) call unwrap_atlist_type(di_atlist,atlist,ierror)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) call get_var_from_item('py_init_structure_factors','spg',item,di_spg,ierror)
        if (ierror == 0) call unwrap_spg_type(di_spg,spg,ierror)
        if (ierror == 0) then
            select type (A => spg)
                class is (superspacegroup_type)
                    ierror = -1
                    err_cfml%ierr = ierror
                    err_cfml%msg = 'py_init_structure_factors: fortran_type in spg must be spg_type'
            end select
        end if
        if (ierror == 0 .and. narg > NMANDATORY) then
            if (ierror == 0) ierror = args%getitem(item,NMANDATORY)
            if (ierror == 0) call get_var_from_item('py_init_structure_factors','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) then
                call unwrap_dict_item('py_init_structure_factors','mode',di_kwargs,mode,ierror)
                if (ierror /= 0) then
                    call err_clear()
                    call clear_error()
                    ierror = 0
                else
                    is_mode = .true.
                end if
                call unwrap_dict_item('py_init_structure_factors','lambda',di_kwargs,lambda,ierror)
                if (ierror /= 0) then
                    call err_clear()
                    call clear_error()
                    ierror = 0
                else
                    is_lambda = .true.
                end if
            end if
        end if
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_init_structure_factors: error parsing arguments'
        end if

        ! Calling Fortran procedure
        if (ierror == 0) then
            if (.not. is_mode .and. .not. is_lambda) then
                call init_structure_factors(refl,atlist,spg)
            else if (is_mode .and. .not. is_lambda) then
                call init_structure_factors(refl,atlist,spg,mode)
            else if (.not. is_mode .and. is_lambda) then
                call init_structure_factors(refl,atlist,spg,lambda=lambda)
            else
                call init_structure_factors(refl,atlist,spg,mode,lambda)
            end if
            if (ierror == 0) ierror = err_cfml%ierr
        end if

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,2)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        resul = ret%get_c_ptr()

    end function py_init_structure_factors

    function py_structure_factors(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(dict)    :: di_refl
        type(dict)    :: di_spg
        type(dict)    :: di_atlist
        type(dict)    :: di_kwargs

        ! Local variables
        integer, parameter :: NMANDATORY = 3
        integer :: ierror,narg
        real :: lambda
        logical :: is_mode,is_lambda
        character(len=:), allocatable :: mode
        class(spg_type), allocatable :: spg
        type(atlist_type) :: atlist
        type(reflist_type) :: refl
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        is_mode = .false.
        is_lambda = .false.
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Get arguments
        call check_number_of_arguments('py_structure_factors',args,NMANDATORY,narg,ierror)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_structure_factors','refl',item,di_refl,ierror)
        if (ierror == 0) call unwrap_reflist_type(di_refl,refl,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_structure_factors','atlist',item,di_atlist,ierror)
        if (ierror == 0) call unwrap_atlist_type(di_atlist,atlist,ierror)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) call get_var_from_item('py_structure_factors','spg',item,di_spg,ierror)
        if (ierror == 0) call unwrap_spg_type(di_spg,spg,ierror)
        if (ierror == 0) then
            select type (A => spg)
                class is (superspacegroup_type)
                    ierror = -1
                    err_cfml%ierr = ierror
                    err_cfml%msg = 'py_structure_factors: fortran_type in spg must be spg_type'
            end select
        end if
        if (ierror == 0 .and. narg > NMANDATORY) then
            if (ierror == 0) ierror = args%getitem(item,NMANDATORY)
            if (ierror == 0) call get_var_from_item('py_structure_factors','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) then
                call unwrap_dict_item('py_structure_factors','mode',di_kwargs,mode,ierror)
                if (ierror /= 0) then
                    call err_clear()
                    call clear_error()
                    ierror = 0
                else
                    is_mode = .true.
                end if
                call unwrap_dict_item('py_structure_factors','lambda',di_kwargs,lambda,ierror)
                if (ierror /= 0) then
                    call err_clear()
                    call clear_error()
                    ierror = 0
                else
                    is_lambda = .true.
                end if
            end if
        end if
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_structure_factors: error parsing arguments'
        end if

        ! Calling Fortran procedure
        if (ierror == 0) then
            if (.not. is_mode .and. .not. is_lambda) then
                call structure_factors(refl,atlist,spg)
            else if (is_mode .and. .not. is_lambda) then
                call structure_factors(refl,atlist,spg,mode)
            else if (.not. is_mode .and. is_lambda) then
                call structure_factors(refl,atlist,spg,lambda=lambda)
            else
                call structure_factors(refl,atlist,spg,mode,lambda)
            end if
            if (ierror == 0) ierror = err_cfml%ierr
        end if

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,2)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        resul = ret%get_c_ptr()

    end function py_structure_factors

end module py_cfml_atoms
