!-------------------------------------------------------------
! cfml_sxtal_geom
! -------------------------------------------------------------
! This file is part of cfml_sxtal_geom
!
! The cfml_sxtal_geom is distributed under LGPL. In agreement with the
! Intergovernmental Convention of the ILL, this software cannot be used
! in military applications.
!
! cfml_sxtal_geom is based on Elias Rabel work for Forpy, see <https://github.com/ylikx/forpy>.
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

module py_cfml_sxtal_geom

    use forpy_mod
    use iso_c_binding

    use cfml_globaldeps
    use cfml_messages
    use cfml_sxtal_geom

    implicit none

    contains

    function py_z1frmd(self_ptr,args_ptr) result(resul) bind(c)
        !! author: ILL Scientific Computing Group
        !! date: 24/03/2023
        !! display: public
        !! proc_internals: true
        !! summary: Wrapper for function z1frmd.
        !! summary: Compute the scattering vector from angles and wavelength for 4-circle geometry.
        !!
        !! ARGS_PTR = (wave,ch,ph,ga,om,nu)
        !!
        !! RESUL = (ierr,err_cfml%msg,nd_z1)
        !  --------           -----------         -----------
        !  Variable           Python type         Description
        !  --------           -----------         -----------
        !  wave               float               wavelength
        !  ch                 float               chi (degrees)
        !  ph                 float               phi (degrees)
        !  ga                 float               gamma (degrees)
        !  om                 float               omega (degrees)
        !  nu                 float               nu (degrees)
        !  ierr               integer             if ierr /= 0, an error occurred
        !  err_cfml%msg       string              error message
        !  nd_z1              ndarray(3;float32)  scattering vector

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Python / Fortran interface variables
        real(kind=cp) :: wave   !! wavelength
        real(kind=cp) :: ch     !! chi angle (degrees)
        real(kind=cp) :: ph     !! phi angle (degrees)
        real(kind=cp) :: ga     !! gamma angle (degrees)
        real(kind=cp) :: om     !! omega angle (degrees)
        real(kind=cp) :: nu     !! nu angle (degrees)
        integer       :: ierr   !! error flag
        type(ndarray) :: nd_z1  !! scattering vector

        ! Local variables
        integer :: ierror
        real(kind=cp), dimension(3) :: z1 ! scattering vector
        type(object) :: item
        type(tuple) :: args,ret

        ! Reset error variable
        ierr   = 0
        ierror = 0
        call clear_error()

        ! In case of exception return C_NULL_PTR
        resul = C_NULL_PTR

        ! Unwrap_arguments
        call unsafe_cast_from_c_ptr(args,args_ptr)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) ierror = cast(wave,item)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(ch,item)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) ierror = cast(ph,item)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) ierror = cast(ga,item)
        if (ierror == 0) ierror = args%getitem(item,4)
        if (ierror == 0) ierror = cast(om,item)
        if (ierror == 0) ierror = args%getitem(item,5)
        if (ierror == 0) ierror = cast(nu,item)
        if (ierror /= 0) then
            err_cfml%ierr = -1
            err_cfml%msg = 'py_z1frmd: Error getting arguments'
        end if

        ! Call CrysFML procedure
        if (ierror == 0) z1 = z1frmd(wave,ch,ph,ga,om,nu)
        if (ierror == 0) ierror = err_cfml%ierr

        ! Return tuple
        if (ierror == 0) then
            ierror = tuple_create(ret,3)
            ierror = ret%setitem(0,ierr)
            ierror = ret%setitem(1,trim(err_cfml%msg))
            ierror = ndarray_create(nd_z1,z1)
            ierror = ret%setitem(2,nd_z1)
        else
            ierr   = ierror
            ierror = tuple_create(ret,2)
            ierror = ret%setitem(0,ierr)
            ierror = ret%setitem(1,trim(err_cfml%msg))
        end if
        resul = ret%get_c_ptr()

    end function py_z1frmd

    function py_z1frnb(self_ptr,args_ptr) result(resul) bind(c)
        !! author: ILL Scientific Computing Group
        !! date: 24/03/2023
        !! display: public
        !! proc_internals: true
        !! summary: Wrapper for function z1frnb.
        !! summary: Compute the scattering vector from angles and wavelength for normal-beam geometry
        !!
        !! ARGS_PTR = (wave,ga,om,nu)
        !!
        !! RESUL = (ierr,err_cfml%msg,nd_z1)
        !  --------           -----------         -----------
        !  Variable           Python type         Description
        !  --------           -----------         -----------
        !  wave               float               wavelength
        !  ga                 float               gamma (degrees)
        !  om                 float               omega (degrees)
        !  nu                 float               nu (degrees)
        !  ierr               integer             if ierr /= 0, an error occurred
        !  err_cfml%msg       string              error message
        !  nd_z1              ndarray(3;float32)  scattering vector

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Python / Fortran interface variables
        real          :: wave  !! wavelength
        real          :: ga    !! gamma angle (degrees)
        real          :: om    !! omega angle (degress)
        real          :: nu    !! nu angle (degrees)
        integer       :: ierr  !! error flag
        type(ndarray) :: nd_z1 !! Scattering vector

        ! Other local variables
        integer :: ierror
        real(kind=cp), dimension(3) :: z1 ! scattering vector
        type(object) :: item
        type(tuple) :: args,ret

        ! Reset error variable
        ierr = 0
        ierror = 0
        call clear_error()

        ! In case of exception return C_NULL_PTR
        resul = C_NULL_PTR

        ! Unwrap_arguments
        call unsafe_cast_from_c_ptr(args,args_ptr)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) ierror = cast(wave,item)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(ga,item)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) ierror = cast(om,item)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) ierror = cast(nu,item)
        if (ierror /= 0) then
            err_cfml%ierr = -1
            err_cfml%msg = 'py_z1frnb: Error getting arguments'
        end if

        ! Call CrysFML procedure
        if (ierror == 0) z1 = z1frnb(wave,ga,om,nu)
        if (ierror == 0) ierror = err_cfml%ierr

        ! Return tuple
        if (ierror == 0) then
            ierror = tuple_create(ret,3)
            ierror = ret%setitem(0,ierr)
            ierror = ret%setitem(1,trim(err_cfml%msg))
            ierror = ndarray_create(nd_z1,z1)
            ierror = ret%setitem(2,nd_z1)
        else
            ierr   = ierror
            ierror = tuple_create(ret,2)
            ierror = ret%setitem(0,ierr)
            ierror = ret%setitem(1,trim(err_cfml%msg))
        end if
        resul = ret%get_c_ptr()

    end function py_z1frnb

end module py_cfml_sxtal_geom
