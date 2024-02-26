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

    use cfml_globaldeps, only: err_cfml,clear_error
    use cfml_gSpacegroups, only: spg_type,superspacegroup_type
    use cfml_metrics, only: cell_g_type
    use cfml_python, only: check_number_of_arguments,get_var_from_item,ndarray_to_pointer,pointer_to_array,pointer_to_array_alloc,&
                           unwrap_cell_g_type,unwrap_dict_item,unwrap_spg_type,wrap_reflist_type
    use cfml_rational, only: rational
    use cfml_reflections, only: gener_reflections_shub,get_h_info,get_maxnumref,hkl_gen_sxtal,h_absent,h_equiv,&
                                h_latt_absent,h_mult,h_s,h_uni,reflist_type

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
        call table_reflections%init(10)
        call table_reflections%add_method("gener_reflections_shub","py_gener_reflections_shub",METH_VARARGS,c_funloc(py_gener_reflections_shub))
        call table_reflections%add_method("get_h_info","py_get_h_info",METH_VARARGS,c_funloc(py_get_h_info))
        call table_reflections%add_method("get_maxnumref","py_get_maxnumref",METH_VARARGS,c_funloc(py_get_maxnumref))
        call table_reflections%add_method("hkl_gen_sxtal","py_hkl_gen_sxtal",METH_VARARGS,c_funloc(py_hkl_gen_sxtal))
        call table_reflections%add_method("h_absent","py_h_absent",METH_VARARGS,c_funloc(py_h_absent))
        call table_reflections%add_method("h_equiv","py_h_equiv",METH_VARARGS,c_funloc(py_h_equiv))
        call table_reflections%add_method("h_latt_absent","py_h_latt_absent",METH_VARARGS,c_funloc(py_h_latt_absent))
        call table_reflections%add_method("h_mult","py_h_mult",METH_VARARGS,c_funloc(py_h_mult))
        call table_reflections%add_method("h_s","py_h_s",METH_VARARGS,c_funloc(py_h_s))
        call table_reflections%add_method("h_uni","py_h_uni",METH_VARARGS,c_funloc(py_h_uni))

        ! Build mod_reflections
        m = mod_reflections%init("py_cfml_reflections","A Python API for CrysFML08",table_reflections)

    end function Init

    function py_gener_reflections_shub(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(dict)   :: di_cell
        type(dict)   :: di_spg
        real         :: smax
        type(dict)   :: di_kwargs

        ! Local variables
        integer, parameter :: NMANDATORY = 3
        integer :: ierror, narg
        logical :: friedel, is_friedel
        character(len=:), allocatable :: fortran_type
        class(cell_g_type), allocatable :: cell
        class(spg_type), allocatable :: spg
        type(reflist_type) :: reflex
        type(dict) :: di_reflex
        type(object) :: item
        type(tuple) :: args, ret

        ierror = 0
        is_friedel = .false.
        ierror = dict_create(di_reflex)
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Get arguments
        call check_number_of_arguments('py_gener_reflections_shub',args,NMANDATORY,narg,ierror)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_gener_reflections_shub','cell',item,di_cell,ierror)
        if (ierror == 0) call unwrap_cell_g_type(di_cell,cell,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_gener_reflections_shub','spg',item,di_spg,ierror)
        if (ierror == 0) call unwrap_spg_type(di_spg,spg,ierror)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) call get_var_from_item('py_gener_reflections_shub','smax',item,smax,ierror)
        if (ierror == 0 .and. narg > NMANDATORY) then
            ierror = args%getitem(item,3)
            if (ierror == 0) call get_var_from_item('py_gener_reflections_shub','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) then
                call unwrap_dict_item('py_gener_reflections_shub','friedel',di_kwargs,friedel,ierror)
                if (ierror == 0) then
                    is_friedel = .true.
                else
                    call clear_error()
                    call err_clear()
                    ierror = 0
                end if
            end if
        end if
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_gener_reflections_shub: error parsing arguments'
        end if

        ! Check types
        if (ierror == 0) then
            ierror = di_cell%getitem(fortran_type,"fortran_type")
            if (fortran_type /= 'cell_g_type') then
                ierror = -1
                err_cfml%ierr = ierror
                err_cfml%msg = 'py_gener_reflections_shub: cell dictionary must have fortran_type = cell_g_type'
            end if
        end if
        if (ierror == 0) then
            ierror = di_spg%getitem(fortran_type,"fortran_type")
            if (fortran_type /= 'spg_type') then
                ierror = -1
                err_cfml%ierr = ierror
                err_cfml%msg = 'py_gener_reflections_shub: spg dictionary must have fortran_type = spg_type'
            end if
        end if

        ! Calling Fortran procedure
        if (ierror == 0) then
            if (.not. is_friedel) then
                call gener_reflections_shub(cell,spg,smax,reflex)
            else
                call gener_reflections_shub(cell,spg,smax,reflex,friedel)
            end if
        end if
        if (ierror == 0) ierror = err_cfml%ierr
        if (ierror == 0) call wrap_reflist_type(reflex,di_reflex,ierror)

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,di_reflex)
        resul = ret%get_c_ptr()

    end function py_gener_reflections_shub

    function py_get_h_info(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(ndarray) :: nd_h
        type(dict)    :: di_spg
        logical       :: mag

        ! Local variables
        integer, parameter :: NMANDATORY = 3
        integer :: ierror,narg
        integer, dimension(4) :: info
        integer, dimension(:), allocatable :: h
        integer, dimension(:), pointer :: p_h
        class(spg_type), allocatable :: spg
        type(object) :: item
        type(ndarray) :: nd_info
        type(tuple) :: args,ret

        ierror = 0
        info(:) = 0
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Get arguments
        call check_number_of_arguments('py_get_h_info',args,NMANDATORY,narg,ierror)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_get_h_info','h',item,nd_h,ierror)
        if (ierror == 0) call ndarray_to_pointer('py_get_h_info','h',nd_h,p_h,ierror)
        if (ierror == 0) call pointer_to_array_alloc('py_get_h_info','h',p_h,h,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_get_h_info','spg',item,di_spg,ierror)
        if (ierror == 0) call unwrap_spg_type(di_spg,spg,ierror)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) call get_var_from_item('py_get_h_info','mag',item,mag,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_get_h_info: error parsing arguments'
        end if

        ! Calling Fortran procedure
        if (ierror == 0) info = get_h_info(h,spg,mag)
        if (ierror == 0) ierror = err_cfml%ierr

        ! Wrap
        if (ierror == 0) ierror = ndarray_create(nd_info,info)

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,nd_info)
        resul = ret%get_c_ptr()

    end function py_get_h_info

    function py_get_maxnumref(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        real       :: sintlmax
        real       :: volcell
        type(dict) :: kwargs
        real       :: sintlmin
        integer    :: mult

        ! Local variables
        integer, parameter :: NMANDATORY = 2
        integer :: ierror,narg,numref
        logical :: is_sintlmin,is_mult
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        numref = 0
        is_sintlmin = .false.
        is_mult = .false.
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Get arguments
        call check_number_of_arguments('py_get_maxnumref',args,NMANDATORY,narg,ierror)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_get_maxnumref','sintlmax',item,sintlmax,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_get_maxnumref','volcell',item,volcell,ierror)
        if (ierror == 0 .and. narg > NMANDATORY) then
            ierror = args%getitem(item,2)
            if (ierror == 0) call get_var_from_item('py_get_maxnumref','kwargs',item,kwargs,ierror)
            if (ierror == 0) then
                call unwrap_dict_item('py_get_maxnumref','sintlmin',kwargs,sintlmin,ierror)
                if (ierror == 0) then
                    is_sintlmin = .true.
                else
                    call clear_error()
                    call err_clear()
                    ierror = 0
                end if
                call unwrap_dict_item('py_get_maxnumref','mult',kwargs,mult,ierror)
                if (ierror == 0) then
                    is_mult = .true.
                else
                    call clear_error()
                    call err_clear()
                    ierror = 0
                end if
            end if
        end if
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_get_maxnumref: error parsing arguments'
        end if

        ! Calling Fortran procedure
        if (ierror == 0) then
            if (.not. is_sintlmin .and. .not. is_mult) then
                numref = get_maxnumref(sintlmax,volcell)
            else if (is_sintlmin .and. .not. is_mult) then
                numref = get_maxnumref(sintlmax,volcell,sintlmin)
            else if (.not. is_sintlmin .and. is_mult) then
                numref = get_maxnumref(sintlmax,volcell,mult=mult)
            else
                numref = get_maxnumref(sintlmax,volcell,sintlmin,mult)
            end if
            ierror = err_cfml%ierr
        end if

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,numref)
        resul = ret%get_c_ptr()

    end function py_get_maxnumref

    function py_hkl_gen_sxtal(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(dict)    :: di_cell
        type(dict)    :: di_spg
        real          :: stlmin
        real          :: stlmax
        type(dict)    :: di_kwargs

        ! Local variables
        integer, parameter :: NMANDATORY = 4
        integer :: ierror,narg
        integer, dimension(3) :: ord
        integer, dimension(3,2) :: hlim
        integer, dimension(:), pointer :: p_ord
        integer, dimension(:,:), pointer :: p_hlim
        logical :: is_ord,is_hlim
        character(len=1) :: order
        character(len=:), allocatable :: fortran_type
        type(reflist_type) :: reflex
        class(cell_g_type), allocatable :: cell
        type(dict) :: di_reflex
        class(spg_type), allocatable :: spg
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        is_ord = .false.
        is_hlim = .false.
        ierror = dict_create(di_reflex)
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Get arguments
        call check_number_of_arguments('py_hkl_gen_sxtal',args,NMANDATORY,narg,ierror)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_hkl_gen_sxtal','cell',item,di_cell,ierror)
        if (ierror == 0) call unwrap_cell_g_type(di_cell,cell,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_hkl_gen_sxtal','spg',item,di_spg,ierror)
        if (ierror == 0) call unwrap_spg_type(di_spg,spg,ierror)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) call get_var_from_item('py_hkl_gen_sxtal','stlmin',item,stlmin,ierror)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) call get_var_from_item('py_hkl_gen_sxtal','stlmax',item,stlmax,ierror)
        if (ierror == 0 .and. narg > NMANDATORY) then
            ierror = args%getitem(item,4)
            if (ierror == 0) call get_var_from_item('py_hkl_gen_sxtal','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) then
                call unwrap_dict_item('py_hkl_gen_sxtal','ord',di_kwargs,p_ord,ierror)
                if (ierror == 0) call pointer_to_array('py_hkl_gen_sxtal','ord',p_ord,ord,ierror)
                if (ierror == 0) then
                    is_ord = .true.
                else
                    call clear_error()
                    call err_clear()
                    ierror = 0
                end if
                call unwrap_dict_item('py_hkl_gen_sxtal','hlim',di_kwargs,p_hlim,ierror,order)
                if (ierror == 0) call pointer_to_array('py_hkl_gen_sxtal','hlim',p_hlim,hlim,ierror,order)
                if (ierror == 0) then
                    is_hlim = .true.
                else
                    call clear_error()
                    call err_clear()
                    ierror = 0
                end if
            end if
        end if
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_hkl_gen_sxtal: error parsing arguments'
        end if
        ! Check types
        if (ierror == 0) then
            ierror = di_cell%getitem(fortran_type,"fortran_type")
            if (fortran_type /= 'cell_g_type') then
                ierror = -1
                err_cfml%ierr = ierror
                err_cfml%msg = 'py_hkl_gen_sxtal: cell dictionary must have fortran_type = cell_g_type'
            end if
        end if
        if (ierror == 0) then
            ierror = di_spg%getitem(fortran_type,"fortran_type")
            if (fortran_type /= 'spg_type') then
                ierror = -1
                err_cfml%ierr = ierror
                err_cfml%msg = 'py_hkl_gen_sxtal: spg dictionary must have fortran_type = spg_type'
            end if
        end if

        ! Calling Fortran procedure
        if (ierror == 0) then
            if (.not. is_ord .and. .not. is_hlim) then
                call hkl_gen_sxtal(cell,spg,stlmin,stlmax,reflex)
            else if (is_ord .and. .not. is_hlim) then
                call hkl_gen_sxtal(cell,spg,stlmin,stlmax,reflex,ord)
            else if (.not. is_ord .and. is_hlim) then
                call hkl_gen_sxtal(cell,spg,stlmin,stlmax,reflex,hlim=hlim)
            else
                call hkl_gen_sxtal(cell,spg,stlmin,stlmax,reflex,ord,hlim)
            end if
        end if
        if (ierror == 0) ierror = err_cfml%ierr
        if (ierror == 0) call wrap_reflist_type(reflex,di_reflex,ierror)

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,di_reflex)
        resul = ret%get_c_ptr()

    end function py_hkl_gen_sxtal

    function py_h_absent(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(ndarray) :: nd_h
        type(dict)    :: di_spg

        ! Local variables
        integer, parameter :: NMANDATORY = 2
        integer :: ierror,narg
        integer, dimension(:), allocatable :: h
        integer, dimension(:), pointer :: p_h
        logical :: info
        class(spg_type), allocatable :: spg
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Get arguments
        call check_number_of_arguments('py_h_absent',args,NMANDATORY,narg,ierror)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_h_absent','h',item,nd_h,ierror)
        if (ierror == 0) call ndarray_to_pointer('py_h_absent','h',nd_h,p_h,ierror)
        if (ierror == 0) call pointer_to_array_alloc('py_h_absent','h',p_h,h,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_h_absent','spg',item,di_spg,ierror)
        if (ierror == 0) call unwrap_spg_type(di_spg,spg,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_h_absent: error parsing arguments'
        end if

        ! Calling Fortran procedure
        if (ierror == 0) info = h_absent(h,spg)
        if (ierror == 0) ierror = err_cfml%ierr

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,info)
        resul = ret%get_c_ptr()

    end function py_h_absent

    function py_h_equiv(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(ndarray) :: nd_h
        type(ndarray) :: nd_k
        type(dict)    :: di_spg
        type(dict)    :: di_kwargs

        ! Local variables
        integer, parameter :: NMANDATORY = 3
        integer :: ierror,narg
        integer, dimension(:), allocatable :: h,k
        integer, dimension(:), pointer :: p_h,p_k
        logical :: friedel,is_friedel,info
        class(spg_type), allocatable :: spg
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        is_friedel = .false.
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Get arguments
        call check_number_of_arguments('py_h_equiv',args,NMANDATORY,narg,ierror)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_h_equiv','h',item,nd_h,ierror)
        if (ierror == 0) call ndarray_to_pointer('py_h_equiv','h',nd_h,p_h,ierror)
        if (ierror == 0) call pointer_to_array_alloc('py_h_equiv','h',p_h,h,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_h_equiv','k',item,nd_k,ierror)
        if (ierror == 0) call ndarray_to_pointer('py_h_equiv','k',nd_k,p_k,ierror)
        if (ierror == 0) call pointer_to_array_alloc('py_h_equiv','k',p_k,k,ierror)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) call get_var_from_item('py_h_equiv','spg',item,di_spg,ierror)
        if (ierror == 0) call unwrap_spg_type(di_spg,spg,ierror)
        if (ierror == 0 .and. narg > NMANDATORY) then
            if (ierror == 0) ierror = args%getitem(item,3)
            if (ierror == 0) call get_var_from_item('py_h_s','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) then
                call unwrap_dict_item('py_h_s','friedel',di_kwargs,friedel,ierror)
                if (ierror /= 0) then
                    call err_clear()
                    call clear_error()
                    ierror = 0
                else
                    is_friedel = .true.
                end if
            end if
        end if
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_h_equiv: error parsing arguments'
        end if

        ! Calling Fortran procedure
        if (ierror == 0) then
            if (is_friedel) then
                info = h_equiv(h,k,spg,friedel)
            else
                info = h_equiv(h,k,spg)
            end if
        end if
        if (ierror == 0) ierror = err_cfml%ierr

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,info)
        resul = ret%get_c_ptr()

    end function py_h_equiv

    function py_h_latt_absent(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(ndarray) :: nd_h
        type(ndarray) :: nd_latt
        integer       :: n

        ! Local variables
        integer, parameter :: NMANDATORY = 3
        integer :: ierror,narg
        integer, dimension(:), allocatable :: h
        integer, dimension(:), pointer :: p_h
        real, dimension(:,:), pointer :: p_latt
        logical :: info
        character(len=1) :: order
        type(rational), dimension(:,:), allocatable :: latt
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Get arguments
        call check_number_of_arguments('py_h_latt_absent',args,NMANDATORY,narg,ierror)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_h_latt_absent','h',item,nd_h,ierror)
        if (ierror == 0) call ndarray_to_pointer('py_h_latt_absent','h',nd_h,p_h,ierror)
        if (ierror == 0) call pointer_to_array_alloc('py_h_latt_absent','h',p_h,h,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_h_latt_absent','latt',item,nd_latt,ierror)
        if (ierror == 0) call ndarray_to_pointer('py_h_latt_absent','latt',nd_latt,p_latt,ierror,order)
        if (ierror == 0) call pointer_to_array_alloc('py_h_latt_absent','latt',p_latt,latt,ierror,order)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) call get_var_from_item('py_h_latt_absent','n',item,n,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_h_latt_absent: error parsing arguments'
        end if

        ! Calling Fortran procedure
        if (ierror == 0) info = h_latt_absent(h,latt,n)
        if (ierror == 0) ierror = err_cfml%ierr

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,info)
        resul = ret%get_c_ptr()

    end function py_h_latt_absent

    function py_h_mult(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(ndarray) :: nd_h
        type(dict)    :: di_spg
        logical       :: friedel

        ! Local variables
        integer, parameter :: NMANDATORY = 3
        integer :: ierror,narg,n
        integer, dimension(:), allocatable :: h
        integer, dimension(:), pointer :: p_h
        class(spg_type), allocatable :: spg
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Get arguments
        call check_number_of_arguments('py_h_mult',args,NMANDATORY,narg,ierror)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_h_mult','h',item,nd_h,ierror)
        if (ierror == 0) call ndarray_to_pointer('py_h_mult','h',nd_h,p_h,ierror)
        if (ierror == 0) call pointer_to_array_alloc('py_h_mult','h',p_h,h,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_h_mult','spg',item,di_spg,ierror)
        if (ierror == 0) call unwrap_spg_type(di_spg,spg,ierror)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) call get_var_from_item('py_h_mult','friedel',item,friedel,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_h_mult: error parsing arguments'
        end if

        ! Calling Fortran procedure
        if (ierror == 0) n = h_mult(h,spg,friedel)
        if (ierror == 0) ierror = err_cfml%ierr

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,n)
        resul = ret%get_c_ptr()

    end function py_h_mult

    function py_h_s(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(ndarray) :: nd_h
        type(dict)    :: di_cell
        type(dict)    :: di_kwargs
        integer       :: nk

        ! Local variables
        integer, parameter :: NMANDATORY = 2
        integer :: ierror,narg
        integer, dimension(:), allocatable :: h_int
        integer, dimension(:), pointer :: p_h_int
        real :: s
        real, dimension(:), pointer :: p_h_real
        real, dimension(:,:), pointer :: p_kv
        real, dimension(:,:), allocatable :: kv,kv_aux
        real, dimension(3) :: h_real
        logical :: is_h_real,is_nk,is_kv
        character(len=1) :: order
        class(cell_g_type), allocatable :: cell
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        nk = 0
        is_h_real = .true.
        is_nk = .false.
        is_kv = .false.
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Get arguments
        call check_number_of_arguments('py_h_s',args,NMANDATORY,narg,ierror)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_h_s','h',item,nd_h,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_h_s','cell',item,di_cell,ierror)
        if (ierror == 0) call unwrap_cell_g_type(di_cell,cell,ierror)
        if (narg > NMANDATORY .and. ierror == 0) then
            ierror = args%getitem(item,2)
            if (ierror == 0) call get_var_from_item('py_h_s','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) then
                call unwrap_dict_item('py_h_s','nk',di_kwargs,nk,ierror)
                if (ierror /= 0) then
                    call err_clear()
                    call clear_error()
                    ierror = 0
                else
                    is_nk = .true.
                end if
                call unwrap_dict_item('py_h_s','kv',di_kwargs,p_kv,ierror,order)
                if (ierror /= 0) then
                    call err_clear()
                    call clear_error()
                    ierror = 0
                else
                    is_kv = .true.
                end if
            end if
        end if

        ! nd_h
        if (ierror == 0) then
            if (ierror == 0) call ndarray_to_pointer('py_h_s','h',nd_h,p_h_real,ierror)
            if (ierror == 0) then
                call pointer_to_array('py_h_s','h',p_h_real,h_real,ierror)
            else
                ierror = 0
                call err_clear()
                call clear_error()
                is_h_real = .false.
                call ndarray_to_pointer('py_h_s','h',nd_h,p_h_int,ierror)
                if (ierror == 0) call pointer_to_array_alloc('py_h_s','h',p_h_int,h_int,ierror)
            end if
        end if

        ! nd_kv
        if (ierror == 0 .and. is_nk .and. is_kv) then
            if (ierror == 0) call pointer_to_array_alloc('py_h_s','kv',p_kv,kv_aux,ierror,order)
            if (ierror == 0) then
                ! Check dimensions
                if (size(kv_aux,1) /= nk .or. size(kv_aux,2) /= 3) then
                    ierror = -1
                    err_cfml%ierr = -1
                    err_cfml%msg = 'py_h_s: Dimensions of kv are inconsistent with nk and h'
                else
                    allocate(kv(3,nk))
                    kv = transpose(kv_aux)
                    deallocate(kv_aux)
                end if
            end if
        end if

        ! Calling Fortran procedure
        if (ierror == 0) then
            if (is_h_real) then
                s = h_s(h_real,cell)
            else
                if (.not. is_nk .or. .not. is_kv) then
                    s = h_s(h_int,cell)
                else
                    s = h_s(h_int,cell,nk,kv)
                end if
            end if
        end if
        if (ierror == 0) ierror = err_cfml%ierr

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,s)
        resul = ret%get_c_ptr()

    end function py_h_s

    function py_h_uni(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(dict)                    :: di_cell
        type(dict)                    :: di_spg
        logical                       :: friedel
        real                          :: vmin
        real                          :: vmax
        character(len=:), allocatable :: code
        integer                       :: maxref
        type(dict)                    :: di_kwargs

        ! Local variables
        integer, parameter :: NMANDATORY = 7
        integer :: ierror,narg
        integer, dimension(3,2) :: hlim
        integer, dimension(:,:), pointer :: p_hlim
        logical :: is_hlim
        character(len=1) :: order
        character(len=:), allocatable :: fortran_type
        type(reflist_type) :: reflex
        class(cell_g_type), allocatable :: cell
        type(dict) :: di_reflex
        class(spg_type), allocatable :: spg
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        is_hlim = .false.
        ierror = dict_create(di_reflex)
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Get arguments
        call check_number_of_arguments('py_h_uni',args,NMANDATORY,narg,ierror)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_h_uni','cell',item,di_cell,ierror)
        if (ierror == 0) call unwrap_cell_g_type(di_cell,cell,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_h_uni','spg',item,di_spg,ierror)
        if (ierror == 0) call unwrap_spg_type(di_spg,spg,ierror)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) call get_var_from_item('py_h_uni','friedel',item,friedel,ierror)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) call get_var_from_item('py_h_uni','vmin',item,vmin,ierror)
        if (ierror == 0) ierror = args%getitem(item,4)
        if (ierror == 0) call get_var_from_item('py_h_uni','vmax',item,vmax,ierror)
        if (ierror == 0) ierror = args%getitem(item,5)
        if (ierror == 0) call get_var_from_item('py_h_uni','code',item,code,ierror)
        if (ierror == 0) ierror = args%getitem(item,6)
        if (ierror == 0) call get_var_from_item('py_h_uni','maxref',item,maxref,ierror)
        if (ierror == 0 .and. narg > NMANDATORY) then
            ierror = args%getitem(item,7)
            if (ierror == 0) call get_var_from_item('py_h_uni','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) then
                call unwrap_dict_item('py_h_uni','hlim',di_kwargs,p_hlim,ierror,order)
                if (ierror == 0) call pointer_to_array('py_h_uni','hlim',p_hlim,hlim,ierror,order)
                if (ierror == 0) then
                    is_hlim = .true.
                else
                    call clear_error()
                    call err_clear()
                    ierror = 0
                end if
            end if
        end if
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_h_uni: error parsing arguments'
        end if
        ! Check types
        if (ierror == 0) then
            ierror = di_cell%getitem(fortran_type,"fortran_type")
            if (fortran_type /= 'cell_g_type') then
                ierror = -1
                err_cfml%ierr = ierror
                err_cfml%msg = 'py_h_uni: cell dictionary must have fortran_type = cell_g_type'
            end if
        end if

        ! Calling Fortran procedure
        if (ierror == 0) then
            if (.not. is_hlim) then
                call h_uni(cell,spg,friedel,vmin,vmax,code(1:1),maxref,reflex)
            else
                call h_uni(cell,spg,friedel,vmin,vmax,code(1:1),maxref,reflex,hlim=hlim)
            end if
        end if
        if (ierror == 0) ierror = err_cfml%ierr
        if (ierror == 0) call wrap_reflist_type(reflex,di_reflex,ierror)

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,di_reflex)
        resul = ret%get_c_ptr()

    end function py_h_uni

end module py_cfml_reflections
