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

module py_cfml_profiles

    use forpy_mod
    use iso_c_binding

    use cfml_globaldeps, only: err_cfml,clear_error
    use cfml_profiles, only: back_to_back_exp,exponential,gaussian,hat,ikeda_carpenter,lorentzian,pseudovoigt,split_pseudovoigt,tch_pvoigt
    use cfml_python

    implicit none

    type(PythonModule), save :: mod_profiles
    type(PythonMethodTable), save :: table_profiles

    contains

    ! Initialization function for Python 3
    ! Called when importing module
    ! Must use bind(c, name="PyInit_<module name>")
    ! Return value must be type(c_ptr),
    ! use the return value of PythonModule%init
    function PyInit_py_cfml_profiles() bind(c,name="PyInit_py_cfml_profiles") result(m)
#ifdef WIN32
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_profiles
#endif

        ! Local variables
        type(c_ptr) :: m

        m = Init()

    end function PyInit_py_cfml_profiles

    function Init() result(m)

        ! Local variables
        type(c_ptr) :: m
        integer :: ierror

        ierror = Forpy_Initialize()

        ! Build method table
        call table_profiles%init(9)
        call table_profiles%add_method("back_to_back_exp","py_back_to_back_exp",METH_VARARGS,c_funloc(py_back_to_back_exp))
        call table_profiles%add_method("exponential","py_exponential",METH_VARARGS,c_funloc(py_exponential))
        call table_profiles%add_method("gaussian","py_gaussian",METH_VARARGS,c_funloc(py_gaussian))
        call table_profiles%add_method("hat","py_hat",METH_VARARGS,c_funloc(py_hat))
        call table_profiles%add_method("ikeda_carpenter","py_ikeda_carpenter",METH_VARARGS,c_funloc(py_ikeda_carpenter))
        call table_profiles%add_method("lorentzian","py_lorentzian",METH_VARARGS,c_funloc(py_lorentzian))
        call table_profiles%add_method("pseudovoigt","py_pseudovoigt",METH_VARARGS,c_funloc(py_pseudovoigt))
        call table_profiles%add_method("split_pseudovoigt","py_split_pseudovoigt",METH_VARARGS,c_funloc(py_split_pseudovoigt))
        call table_profiles%add_method("tch_pvoigt","py_tch_pvoigt",METH_VARARGS,c_funloc(py_tch_pvoigt))

        ! Build mod_profiles
        m = mod_profiles%init("py_cfml_profiles","A Python API for CrysFML08",table_profiles)

    end function Init

    function py_back_to_back_exp(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        real :: x
        real :: alpha
        real :: beta

        ! Local variables
        integer, parameter :: NMANDATORY = 3
        integer :: ierror,narg
        real :: bb_val
        real, dimension(2) :: par
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        bb_val = 0.0
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Get arguments
        call check_number_of_arguments('py_back_to_back_exp',args,NMANDATORY,narg,ierror)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_back_to_back_exp','x',item,x,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_back_to_back_exp','alpha',item,alpha,ierror)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) call get_var_from_item('py_back_to_back_exp','beta',item,beta,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_back_to_back_exp: error parsing arguments'
        end if

        ! Call Fortran procedure
        if (ierror == 0) then
            par(1) = alpha
            par(2) = beta
            bb_val = back_to_back_exp(x,par)
            ierror = err_cfml%ierr
        end if

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,bb_val)
        resul = ret%get_c_ptr()

    end function py_back_to_back_exp

    function py_exponential(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        real :: x
        real :: p

        ! Local variables
        integer, parameter :: NMANDATORY = 2
        integer :: ierror,narg
        real :: exp_val
        real, dimension(1) :: par
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        exp_val = 0.0
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Get arguments
        call check_number_of_arguments('py_exponential',args,NMANDATORY,narg,ierror)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_exponential','x',item,x,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_exponential','p',item,p,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_exponential: error parsing arguments'
        end if

        ! Call Fortran procedure
        if (ierror == 0) then
            par(1) = p
            exp_val = exponential(x,par)
            ierror = err_cfml%ierr
        end if

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,exp_val)
        resul = ret%get_c_ptr()

    end function py_exponential

    function py_gaussian(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        real :: x
        real :: p

        ! Local variables
        integer, parameter :: NMANDATORY = 2
        integer :: ierror,narg
        real :: gauss_val
        real, dimension(1) :: par
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        gauss_val = 0.0
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Get arguments
        call check_number_of_arguments('py_gaussian',args,NMANDATORY,narg,ierror)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_gaussian','x',item,x,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_gaussian','p',item,p,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_gaussian: error parsing arguments'
        end if

        ! Call Fortran procedure
        if (ierror == 0) then
            par(1) = p
            gauss_val = gaussian(x,par)
            ierror = err_cfml%ierr
        end if

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,gauss_val)
        resul = ret%get_c_ptr()

    end function py_gaussian

    function py_hat(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        real :: x
        real :: p

        ! Local variables
        integer, parameter :: NMANDATORY = 2
        integer :: ierror,narg
        real :: hat_val
        real, dimension(1) :: par
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        hat_val = 0.0
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Get arguments
        call check_number_of_arguments('py_hat',args,NMANDATORY,narg,ierror)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_hat','x',item,x,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_hat','p',item,p,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_hat: error parsing arguments'
        end if

        ! Call Fortran procedure
        if (ierror == 0) then
            par(1) = p
            hat_val = hat(x,par)
            ierror = err_cfml%ierr
        end if

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,hat_val)
        resul = ret%get_c_ptr()

    end function py_hat

    function py_ikeda_carpenter(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        real :: x
        real :: alpha
        real :: beta
        real :: r

        ! Local variables
        integer, parameter :: NMANDATORY = 4
        integer :: ierror,narg
        real :: ik_val
        real, dimension(3) :: par
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        ik_val = 0.0
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Get arguments
        call check_number_of_arguments('py_ikeda_carpenter',args,NMANDATORY,narg,ierror)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_ikeda_carpenter','x',item,x,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_ikeda_carpenter','alpha',item,alpha,ierror)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) call get_var_from_item('py_ikeda_carpenter','beta',item,beta,ierror)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) call get_var_from_item('py_ikeda_carpenter','r',item,r,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_ikeda_carpenter: error parsing arguments'
        end if

        ! Call Fortran procedure
        if (ierror == 0) then
            par(1) = alpha
            par(2) = beta
            par(3) = r
            ik_val = ikeda_carpenter(x,par)
            ierror = err_cfml%ierr
        end if

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,ik_val)
        resul = ret%get_c_ptr()

    end function py_ikeda_carpenter

    function py_lorentzian(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        real :: x
        real :: p

        ! Local variables
        integer, parameter :: NMANDATORY = 2
        integer :: ierror,narg
        real :: lorentzian_val
        real, dimension(1) :: par
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        lorentzian_val = 0.0
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Get arguments
        call check_number_of_arguments('py_lorentzian',args,NMANDATORY,narg,ierror)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_lorentzian','x',item,x,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_lorentzian','p',item,p,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_lorentzian: error parsing arguments'
        end if

        ! Call Fortran procedure
        if (ierror == 0) then
            par(1) = p
            lorentzian_val = lorentzian(x,par)
            ierror = err_cfml%ierr
        end if

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,lorentzian_val)
        resul = ret%get_c_ptr()

    end function py_lorentzian

    function py_pseudovoigt(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        real :: x
        real :: h
        real :: eta

        ! Local variables
        integer, parameter :: NMANDATORY = 3
        integer :: ierror,narg
        real :: pv_val
        real, dimension(2) :: par
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        pv_val = 0.0
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Get arguments
        call check_number_of_arguments('py_pseudovoigt',args,NMANDATORY,narg,ierror)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_pseudovoigt','x',item,x,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_pseudovoigt','h',item,h,ierror)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) call get_var_from_item('py_pseudovoigt','eta',item,eta,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_pseudovoigt: error parsing arguments'
        end if

        ! Call Fortran procedure
        if (ierror == 0) then
            par(1) = h
            par(2) = eta
            pv_val = pseudovoigt(x,par)
            ierror = err_cfml%ierr
        end if

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,pv_val)
        resul = ret%get_c_ptr()

    end function py_pseudovoigt

    function py_split_pseudovoigt(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        real :: x
        real :: h1
        real :: h2
        real :: eta1
        real :: eta2

        ! Local variables
        integer, parameter :: NMANDATORY = 5
        integer :: ierror,narg
        real :: pv_val
        real, dimension(4) :: par
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        pv_val = 0.0
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Get arguments
        call check_number_of_arguments('py_split_pseudovoigt',args,NMANDATORY,narg,ierror)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_split_pseudovoigt','x',item,x,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_split_pseudovoigt','h1',item,h1,ierror)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) call get_var_from_item('py_split_pseudovoigt','h2',item,h2,ierror)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) call get_var_from_item('py_split_pseudovoigt','eta1',item,eta1,ierror)
        if (ierror == 0) ierror = args%getitem(item,4)
        if (ierror == 0) call get_var_from_item('py_split_pseudovoigt','eta2',item,eta2,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_split_pseudovoigt: error parsing arguments'
        end if

        ! Call Fortran procedure
        if (ierror == 0) then
            par(1) = h1
            par(2) = h2
            par(3) = eta1
            par(4) = eta2
            pv_val = split_pseudovoigt(x,par)
            ierror = err_cfml%ierr
        end if

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,pv_val)
        resul = ret%get_c_ptr()

    end function py_split_pseudovoigt

    function py_tch_pvoigt(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        real :: x
        real :: hg
        real :: hl

        ! Local variables
        integer, parameter :: NMANDATORY = 3
        integer :: ierror,narg
        real :: pv_val
        real, dimension(2) :: par
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        pv_val = 0.0
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Get arguments
        call check_number_of_arguments('py_pseudovoigt',args,NMANDATORY,narg,ierror)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_pseudovoigt','x',item,x,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_pseudovoigt','hg',item,hg,ierror)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) call get_var_from_item('py_pseudovoigt','hl',item,hl,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_pseudovoigt: error parsing arguments'
        end if

        ! Call Fortran procedure
        if (ierror == 0) then
            par(1) = hg
            par(2) = hl
            pv_val = tch_pvoigt(x,par)
            ierror = err_cfml%ierr
        end if

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,pv_val)
        resul = ret%get_c_ptr()

    end function py_tch_pvoigt

end module py_cfml_profiles
