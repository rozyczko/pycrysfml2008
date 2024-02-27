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

module py_cfml_metrics

    use forpy_mod
    use iso_c_binding
    use, intrinsic :: iso_fortran_env, only: int64, int32, real32, real64

    use cfml_globaldeps, only: err_cfml,clear_error
    use cfml_metrics, only: cell_gls_type,cell_g_type,cell_type,change_setting_cell,get_betas_from_biso,&
                            get_betas_from_u,get_conventional_cell,get_transfm_matrix,get_twofold_axes,&
                            get_u_from_b,get_u_from_betas,set_crystal_cell,twofold_axes_type
    use cfml_python, only: check_number_of_arguments,get_var_from_item,ndarray_to_pointer,pointer_to_array,&
                           unwrap_dict_item,unwrap_cell_g_type,unwrap_twofold_axes_type,wrap_cell_type,&
                           wrap_twofold_axes_type

    implicit none

    type(PythonModule), save :: mod_metrics
    type(PythonMethodTable), save :: table_metrics

    contains

    ! Initialization function for Python 3
    ! Called when importing module
    ! Must use bind(c, name="PyInit_<module name>")
    ! Return value must be type(c_ptr),
    ! use the return value of PythonModule%init
    function PyInit_py_cfml_metrics() bind(c,name="PyInit_py_cfml_metrics") result(m)
#ifdef WIN32
        !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_metrics
#endif
        ! Local variables
        type(c_ptr) :: m

        m = Init()

    end function PyInit_py_cfml_metrics

    function Init() result(m)

        ! Local variables
        type(c_ptr) :: m
        integer :: ierror

        ierror = Forpy_Initialize()

        ! Build method table
        call table_metrics%init(9)
        call table_metrics%add_method("change_setting_cell","py_change_setting_cell",METH_VARARGS,c_funloc(py_change_setting_cell))
        call table_metrics%add_method("get_betas_from_biso","py_get_betas_from_biso",METH_VARARGS,c_funloc(py_get_betas_from_biso))
        call table_metrics%add_method("get_betas_from_u","py_get_betas_from_u",METH_VARARGS,c_funloc(py_get_betas_from_u))
        call table_metrics%add_method("get_conventional_cell","py_get_conventional_cell",METH_VARARGS,c_funloc(py_get_conventional_cell))
        call table_metrics%add_method("get_transfm_matrix","py_get_transfm_matrix",METH_VARARGS,c_funloc(py_get_transfm_matrix))
        call table_metrics%add_method("get_twofold_axes","py_get_twofold_axes",METH_VARARGS,c_funloc(py_get_twofold_axes))
        call table_metrics%add_method("get_u_from_b","py_get_u_from_b",METH_VARARGS,c_funloc(py_get_u_from_b))
        call table_metrics%add_method("get_u_from_betas","py_get_u_from_betas",METH_VARARGS,c_funloc(py_get_u_from_betas))
        call table_metrics%add_method("set_crystal_cell","py_set_crystal_cell",METH_VARARGS,c_funloc(py_set_crystal_cell))

        ! Build mod_metrics
        m = mod_metrics%init("py_cfml_metrics","A Python API for CrysFML08",table_metrics)

    end function Init

    function py_change_setting_cell(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(dict)                    :: di_cell
        type(ndarray)                 :: nd_mat
        character(len=:), allocatable :: sett
        type(dict)                    :: di_kwargs
        character(len=:), allocatable :: matkind

        ! Local variables
        integer, parameter :: NMANDATORY = 2
        integer :: ierror,narg
        real, dimension(3,3) :: mat
        real, dimension(:,:), pointer :: p_mat
        logical :: is_setting,is_matkind
        character(len=1) :: order
        character(len=:), allocatable :: fortran_type
        type(dict) :: di_celln
        type(object) :: item
        type(tuple) :: args,ret
        class(cell_g_type), allocatable :: cell,celln

        ierror = 0
        is_setting = .false.
        is_matkind = .false.
        ierror = dict_create(di_celln)
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Check the number of items
        call check_number_of_arguments('py_change_setting_cell',args,NMANDATORY,narg,ierror)

        ! Get arguments
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_change_setting_cell','cell',item,di_cell,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_change_setting_cell','sett',item,sett,ierror)
        if (ierror == 0) then
            is_setting = .true.
        else
            ierror = 0
            call clear_error()
            call get_var_from_item('py_change_setting_cell','mat',item,nd_mat,ierror)
        end if
        if (ierror == 0 .and. narg > NMANDATORY) then
            ! Get optional arguments
            ierror = args%getitem(item,2)
            if (ierror == 0) call get_var_from_item('py_change_setting_cell','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) call unwrap_dict_item('py_change_setting_cell','matkind',di_kwargs,matkind,ierror)
            if (ierror /= 0) then
                call err_clear()
                call clear_error()
                ierror = 0
            else
                if (ierror == 0) is_matkind = .true.
            end if
        end if
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_change_setting_cell: error parsing arguments'
        end if

        ! Check type of di_cell
        if (ierror == 0) ierror = di_cell%getitem(fortran_type,"fortran_type")
        if (ierror /= 0) then
            ierror = -1
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_change_setting_cell: error getting fortran type of cell'
        else
            if (fortran_type /= 'cell_g_type' .and. fortran_type /= 'cell_gls_type') then
                ierror = -1
                err_cfml%ierr = ierror
                err_cfml%msg = 'py_change_setting_cell: fortran_type of cell must be cell_g_type or cell_gls_type'
            else
                call unwrap_cell_g_type(di_cell,cell,ierror)
                if (ierror == 0) then
                    if (fortran_type == 'cell_g_type') then
                        allocate(cell_g_type :: celln)
                    else
                        allocate(cell_gls_type :: celln)
                    end if
                end if
            end if
        end if

        if (ierror == 0) then
            if (is_setting) then
                if (index(';',sett) == 0) sett = sett//';'
            else
                if (ierror == 0) call ndarray_to_pointer('py_change_setting_cell','mat',nd_mat,p_mat,ierror,order)
                if (ierror == 0) call pointer_to_array('py_change_setting_cell','mat',p_mat,mat,ierror,order)
            end if
        end if

        ! Call Fortran procedure
        if (ierror == 0) then
            if (.not. is_matkind) then
                if (is_setting) then
                    call change_setting_cell(cell,sett,celln)
                    ierror = err_cfml%ierr
                else
                    call change_setting_cell(cell,mat,celln)
                    ierror = err_cfml%ierr
                end if
            else
                if (is_setting) then
                    ierror = -1
                    err_cfml%ierr = ierror
                    err_cfml%msg = 'py_change_setting_cell: matkind can only be used with matrix'
                else
                    call change_setting_cell(cell,mat,celln,matkind)
                    ierror = err_cfml%ierr
                end if
            end if
        end if

        ! Wrap
        if (ierror == 0) call wrap_cell_type(celln,di_celln,ierror)

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,di_celln)
        resul = ret%get_c_ptr()

    end function py_change_setting_cell

    function py_get_betas_from_biso(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        real          :: biso
        type(dict)    :: di_cell       ! dictionary => class cell_g_type

        ! Local variables
        integer, parameter :: NMANDATORY = 2
        integer :: ierror,narg
        real, dimension(6) :: betas
        type(ndarray) :: nd_betas
        type(object) :: item
        type(tuple) :: args,ret
        class(cell_g_type), allocatable :: cell

        ierror = 0
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Check the number of items
        call check_number_of_arguments('py_get_betas_from_biso',args,NMANDATORY,narg,ierror)

        ! Get arguments
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_get_betas_from_biso','biso',item,biso,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_get_betas_from_biso','cell',item,di_cell,ierror)
        if (ierror == 0) call unwrap_cell_g_type(di_cell,cell,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_get_betas_from_biso: error parsing arguments'
        end if

        ! Call Fortran procedure
        if (ierror == 0) betas = get_betas_from_biso(biso,cell)
        if (ierror == 0) ierror = err_cfml%ierr
        if (ierror == 0) ierror = ndarray_create(nd_betas,betas)

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,nd_betas)
        resul = ret%get_c_ptr()

    end function py_get_betas_from_biso

    function py_get_betas_from_u(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(ndarray) :: nd_u
        type(dict)    :: di_cell       ! dictionary => class cell_g_type

        ! Local variables
        integer, parameter :: NMANDATORY = 2
        integer :: ierror,narg
        real, dimension(:), pointer :: p_u
        real, dimension(6) :: u,betas
        type(ndarray) :: nd_betas
        type(object) :: item
        type(tuple) :: args,ret
        class(cell_g_type), allocatable :: cell

        ierror = 0
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Check the number of items
        call check_number_of_arguments('py_get_betas_from_u',args,NMANDATORY,narg,ierror)

        ! Get arguments
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_get_betas_from_u','u',item,nd_u,ierror)
        if (ierror == 0) call ndarray_to_pointer('py_get_betas_from_u','u',nd_u,p_u,ierror)
        if (ierror == 0) call pointer_to_array('py_get_betas_from_u','u',p_u,u,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_get_betas_from_u','cell',item,di_cell,ierror)
        if (ierror == 0) call unwrap_cell_g_type(di_cell,cell,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_get_betas_from_u: error parsing arguments'
        end if

        ! Call Fortran procedure
        if (ierror == 0) betas = get_betas_from_u(u,cell)
        if (ierror == 0) ierror = err_cfml%ierr
        if (ierror == 0) ierror = ndarray_create(nd_betas,betas)

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,nd_betas)
        resul = ret%get_c_ptr()

    end function py_get_betas_from_u

    function py_get_conventional_cell(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(dict) :: di_twofold
        type(dict) :: di_kwargs

        ! Local variables
        integer, parameter :: NMANDATORY = 1
        integer :: ierror,narg
        integer, dimension(2) :: myshape
        integer, dimension(3,3) :: tr
        real :: told
        logical :: is_told
        character(len=1024) :: message
        type(cell_g_type) :: cell
        type(twofold_axes_type) :: twofold
        type(ndarray) :: nd_tr
        type(dict) :: di_cell
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        is_told = .false.
        tr = 0
        message = ''
        myshape(:) = (/ 3,3 /)
        ierror = dict_create(di_cell)
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Check the number of items
        call check_number_of_arguments('py_get_conventional_cell',args,NMANDATORY,narg,ierror)

        ! Get arguments
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_get_conventional_cell','twofold',item,di_twofold,ierror)
        if (ierror == 0) call unwrap_twofold_axes_type(di_twofold,twofold,ierror)
        if (ierror == 0 .and. narg > NMANDATORY) then
            if (ierror == 0) ierror = args%getitem(item,NMANDATORY)
            if (ierror == 0) call get_var_from_item('py_get_conventional_cell','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) call unwrap_dict_item('py_get_conventional_cell','told',di_kwargs,told,ierror)
            if (ierror /= 0) then
                call err_clear()
                call clear_error()
                ierror = 0
            else
                is_told = .true.
            end if
        end if
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_get_conventional_cell: error parsing arguments'
        end if

        ! Call Fortran procedure
        if (ierror == 0) then
            if (is_told) then
                call get_conventional_cell(twofold,cell,tr,message,told)
            else
                call get_conventional_cell(twofold,cell,tr,message)
            end if
            if (ierror == 0) ierror = err_cfml%ierr
            if (ierror == 0) call wrap_cell_type(cell,di_cell,ierror)
        end if
        if (ierror == 0) then
            ierror = ndarray_create(nd_tr,tr)
        else
            ierror = ndarray_create_zeros(nd_tr,myshape)
        end if

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,5)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,di_cell)
        ierror = ret%setitem(3,nd_tr)
        ierror = ret%setitem(4,trim(message))
        resul = ret%get_c_ptr()

    end function py_get_conventional_cell

    function py_get_transfm_matrix(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(dict) :: di_cell       ! dictionary => class cell_g_type
        type(dict) :: di_kwargs     ! dictionary with optional arguments

        ! Local variables
        integer, parameter :: NMANDATORY = 2
        integer :: ierror,narg
        integer, dimension(2) :: myshape
        real :: tol
        real, dimension(3,3) :: trm
        logical :: is_tol
        type(ndarray) :: nd_trm
        type(object) :: item
        type(tuple) :: args,ret
        class(cell_g_type), allocatable :: cella,cellb

        ierror = 0
        is_tol = .false.
        myshape(:) = (/ 3,3 /)
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Check the number of items
        call check_number_of_arguments('py_get_transfm_matrix',args,NMANDATORY,narg,ierror)

        ! Get arguments
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_get_transfm_matrix','cella',item,di_cell,ierror)
        if (ierror == 0) call unwrap_cell_g_type(di_cell,cella,ierror)
        if (ierror == 0) call di_cell%clear()
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_get_transfm_matrix','cellb',item,di_cell,ierror)
        if (ierror == 0) call unwrap_cell_g_type(di_cell,cellb,ierror)
        if (ierror == 0 .and. narg > NMANDATORY) then
            if (ierror == 0) ierror = args%getitem(item,2)
            if (ierror == 0) call get_var_from_item('py_get_transfm_matrix','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) call unwrap_dict_item('py_get_transfm_matrix','tol',di_kwargs,tol,ierror)
            if (ierror /= 0) then
                call err_clear()
                call clear_error()
                ierror = 0
            else
                is_tol = .true.
            end if
        end if
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_get_transfm_matrix: error parsing arguments'
        end if

        ! Call Fortran procedure
        if (ierror == 0) then
            if (is_tol) then
                trm = get_transfm_matrix(cella,cellb,tol)
            else
                trm = get_transfm_matrix(cella,cellb)
            end if
            ierror = err_cfml%ierr
        end if
        if (ierror == 0) then
            ierror = ndarray_create(nd_trm,trm)
        else
            ierror = ndarray_create_zeros(nd_trm,myshape)
        end if

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,nd_trm)
        resul = ret%get_c_ptr()

    end function py_get_transfm_matrix

    function py_get_twofold_axes(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(dict)    :: di_cell
        real          :: tol

        ! Local variables
        integer, parameter :: NMANDATORY = 2
        integer :: ierror,narg
        type(object) :: item
        type(tuple) :: args,ret
        class(cell_g_type), allocatable :: cell
        type(dict) :: di_twofold
        type(twofold_axes_type) :: twofold

        ierror = 0
        ierror = dict_create(di_twofold)
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Check the number of items
        call check_number_of_arguments('py_get_twofold_axes',args,NMANDATORY,narg,ierror)

        ! Get arguments
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_get_twofold_axes','cell',item,di_cell,ierror)
        if (ierror == 0) call unwrap_cell_g_type(di_cell,cell,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_get_twofold_axes','tol',item,tol,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_get_twofold_axes: error parsing arguments'
        end if

        ! Call Fortran procedure
        if (ierror == 0) twofold = get_twofold_axes(cell,tol)
        if (ierror == 0) ierror = err_cfml%ierr
        if (ierror == 0) call wrap_twofold_axes_type(twofold,di_twofold,ierror)

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,di_twofold)
        resul = ret%get_c_ptr()

    end function py_get_twofold_axes

    function py_get_u_from_b(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(ndarray) :: nd_b

        ! Local variables
        integer, parameter :: NMANDATORY = 1
        integer :: ierror,narg
        real, dimension(:), pointer :: p_b
        real, dimension(6) :: b,u
        type(ndarray) :: nd_u
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Check the number of items
        call check_number_of_arguments('py_get_u_from_b',args,NMANDATORY,narg,ierror)

        ! Get arguments
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_get_u_from_b','b',item,nd_b,ierror)
        if (ierror == 0) call ndarray_to_pointer('py_get_u_from_b','b',nd_b,p_b,ierror)
        if (ierror == 0) call pointer_to_array('py_get_u_from_b','b',p_b,b,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_get_u_from_b: error parsing arguments'
        end if

        ! Call Fortran procedure
        if (ierror == 0) u = get_u_from_b(b)
        if (ierror == 0) ierror = err_cfml%ierr
        if (ierror == 0) ierror = ndarray_create(nd_u,u)

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,nd_u)
        resul = ret%get_c_ptr()

    end function py_get_u_from_b

    function py_get_u_from_betas(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(ndarray) :: nd_betas
        type(dict) :: di_cell

        ! Local variables
        integer, parameter :: NMANDATORY = 2
        integer :: ierror,narg
        real, dimension(:), pointer :: p_betas
        real, dimension(6) :: betas,u
        type(ndarray) :: nd_u
        type(object) :: item
        type(tuple) :: args,ret
        class(cell_g_type), allocatable :: cell

        ierror = 0
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Check the number of items
        call check_number_of_arguments('py_get_u_from_betas',args,NMANDATORY,narg,ierror)

        ! Get arguments
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_get_u_from_betas','betas',item,nd_betas,ierror)
        if (ierror == 0) call ndarray_to_pointer('py_get_u_from_betas','betas',nd_betas,p_betas,ierror)
        if (ierror == 0) call pointer_to_array('py_get_u_from_betas','betas',p_betas,betas,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_get_u_from_betas','cell',item,di_cell,ierror)
        if (ierror == 0) call unwrap_cell_g_type(di_cell,cell,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_get_u_from_betas: error parsing arguments'
        end if

        ! Call Fortran procedure
        if (ierror == 0) u = get_u_from_betas(betas,cell)
        if (ierror == 0) ierror = err_cfml%ierr
        if (ierror == 0) ierror = ndarray_create(nd_u,u)

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,nd_u)
        resul = ret%get_c_ptr()

    end function py_get_u_from_betas

    function py_set_crystal_cell(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(ndarray)                 :: nd_abc
        type(ndarray)                 :: nd_albega
        type(dict)                    :: di_kwargs

        ! Local variables
        integer, parameter :: NMANDATORY = 2
        integer :: ierror,narg
        real, dimension(:), pointer :: p_abc,p_albega,p_sd_abc,p_sd_albega
        logical :: is_cartype,is_sd_abc,is_sd_albega
        character(len=:), allocatable :: cartype
        type(dict) :: di_cell
        type(object) :: item
        type(tuple) :: args,ret
        class(cell_g_type), allocatable :: cell

        ierror = 0
        is_cartype = .false.
        is_sd_abc = .false.
        is_sd_albega = .false.
        ierror = dict_create(di_cell)
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Check the number of items
        call check_number_of_arguments('py_set_crystal_cell',args,NMANDATORY,narg,ierror)

        ! Get arguments
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_set_crystal_cell','abc',item,nd_abc,ierror)
        if (ierror == 0) call ndarray_to_pointer('py_set_crystal_cell','abc',nd_abc,p_abc,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_set_crystal_cell','albega',item,nd_albega,ierror)
        if (ierror == 0) call ndarray_to_pointer('py_set_crystal_cell','albega',nd_albega,p_albega,ierror)
        if (ierror == 0 .and. narg > NMANDATORY) then
            ! Get optional arguments
            ierror = args%getitem(item,2)
            if (ierror == 0) call get_var_from_item('py_set_crystal_cell','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) then
                call unwrap_dict_item('py_set_crystal_cell','cartype',di_kwargs,cartype,ierror)
                if (ierror /= 0) then
                    call err_clear()
                    call clear_error()
                    ierror = 0
                else
                    is_cartype = .true.
                end if
                call unwrap_dict_item('py_set_crystal_cell','sd_abc',di_kwargs,p_sd_abc,ierror)
                if (ierror /= 0) then
                    call err_clear()
                    call clear_error()
                    ierror = 0
                else
                    if (ierror == 0) is_sd_abc = .true.
                end if
                call unwrap_dict_item('py_set_crystal_cell','sd_albega',di_kwargs,p_sd_albega,ierror)
                if (ierror /= 0) then
                    call err_clear()
                    call clear_error()
                    ierror = 0
                else
                    if (ierror == 0) is_sd_albega = .true.
                end if
            end if
        end if
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_set_crystal_cell: error parsing arguments'
        end if

        if (ierror == 0) then
            if (is_sd_abc .or. is_sd_albega) then
                allocate(cell_gls_type :: cell)
            else
                allocate(cell_g_type :: cell)
            end if
            ! Call Fortran procedure
            if (narg == NMANDATORY) then
                call set_crystal_cell(p_abc,p_albega,cell)
            else if (is_cartype .and. .not. is_sd_abc .and. .not. is_sd_albega) then
                call set_crystal_cell(p_abc,p_albega,cell,cartype=cartype)
            else if (.not. is_cartype .and. is_sd_abc .and. .not. is_sd_albega) then
                call set_crystal_cell(p_abc,p_albega,cell,vscell=p_sd_abc)
            else if (.not. is_cartype .and. .not. is_sd_abc .and. is_sd_albega) then
                call set_crystal_cell(p_abc,p_albega,cell,vsang=p_sd_albega)
            else if (is_cartype .and. is_sd_abc .and. .not. is_sd_albega) then
                call set_crystal_cell(p_abc,p_albega,cell,cartype=cartype,vscell=p_sd_abc)
            else if (is_cartype .and. .not. is_sd_abc .and. is_sd_albega) then
                call set_crystal_cell(p_abc,p_albega,cell,cartype=cartype,vsang=p_sd_albega)
            else if (.not. is_cartype .and. is_sd_abc .and. is_sd_albega) then
                call set_crystal_cell(p_abc,p_albega,cell,vscell=p_sd_abc,vsang=p_sd_albega)
            else if (is_cartype .and. is_sd_abc .and. is_sd_albega) then
                call set_crystal_cell(p_abc,p_albega,cell,cartype=cartype,vscell=p_sd_abc,vsang=p_sd_abc)
            end if
            ierror = err_cfml%ierr
        end if

        ! Wrap
        if (ierror == 0) call wrap_cell_type(cell,di_cell,ierror)

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,di_cell)
        resul = ret%get_c_ptr()

    end function py_set_crystal_cell

end module py_cfml_metrics
