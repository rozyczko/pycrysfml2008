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
! Authors: Nebil A. Katcho (ILL)
!          Juan Rodriguez-Carvajal (ILL)
!
!
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
    use cfml_sxtal_geom

    implicit none

    contains

    function py_z1frmd(self_ptr,args_ptr) result(resul) bind(c)

        !! author: Nebil A. Katcho
        !! date: 09/03/2023
        !! display: public
        !! proc_internals: true
        !!
        !!* args_ptr = [wave,ch,ph,ga,om,nu]
        !!* result = [ierr,z1]
        !!
        !! Wrapper for procedure z1frmd.</br><br> Compute the scattering vector from angles and wavelength for 4-circle geometry.</br>
        !!
        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Local variables for the CrysFML08 procedure
        real(kind=cp) :: wave !! Wavelength
        real(kind=cp) :: ch   !! Chi angle (degrees)
        real(kind=cp) :: ph   !! Phi angle (degrees)
        real(kind=cp) :: ga   !! Gamma angle (degrees)
        real(kind=cp) :: om   !! Omega angle (degrees)
        real(kind=cp) :: nu   !! Nu angle (degrees)
        real(kind=cp), dimension(3) :: z1 !! Scattering vector

        ! Other local variables
        integer :: ierror,ierr
        type(ndarray) :: nd_z1
        type(object) :: item
        type(tuple) :: args,ret

        ! Reset error variable
        ierror = 0

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

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            z1 = z1frmd(wave,ch,ph,ga,om,nu)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"z1frmd: "//trim(err_cfml%msg))
            end if
        else
            ierror = EXCEPTION_ERROR
            call raise_exception(RuntimeError,"z1frmd: Error reading arguments")
        end if

        ! Return tuple
        if (ierror == 0) then
            ierr = 0
            ierror = tuple_create(ret,2)
            ierror = ret%setitem(0,ierr)
            ierror = ndarray_create(nd_z1,z1)
            ierror = ret%setitem(1,nd_z1)
        else
            ierr = -1
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,ierr)
        end if
        resul = ret%get_c_ptr()

    end function py_z1frmd

    function py_z1frnb(self_ptr,args_ptr) result(resul) bind(c)

        !! author: Nebil A. Katcho
        !! date: 09/03/2023
        !! summary: Wrapper for procedure z1frnb.
        !! summary: Compute the scattering vector from angles and wavelength for normal-beam geometry
        !! display: public
        !! proc_internals: true

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Local variables for the CrysFML08 procedure
        real(kind=cp) :: wave !! Wavelength,item 1 in args_ptr
        real(kind=cp) :: ga   !! Gamma angle, item 2 in args_ptr
        real(kind=cp) :: om   !! Omega angle, item 3 in args_ptr
        real(kind=cp) :: nu   !! Nu angle, item 4 in args_ptr
        real(kind=cp), dimension(3) :: z1 !! Scattering vector

        ! Other local variables
        integer :: ierror,ierr
        type(ndarray) :: nd_z1
        type(object) :: item
        type(tuple) :: args,ret

        ! Reset error variable
        ierror = 0

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

        ! Call CrysFML procedure
        if (ierror == 0) then
            call clear_error()
            z1 = z1frnb(wave,ga,om,nu)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"z1frnb: "//trim(err_cfml%msg))
            end if
        else
            ierror = EXCEPTION_ERROR
            call raise_exception(RuntimeError,"z1frnb: Error reading arguments")
        end if

        ! Return tuple
        ! Return tuple
        if (ierror == 0) then
            ierr = 0
            ierror = tuple_create(ret,2)
            ierror = ret%setitem(0,ierr)
            ierror = ndarray_create(nd_z1,z1)
            ierror = ret%setitem(1,nd_z1)
        else
            ierr = -1
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,ierr)
        end if
        resul = ret%get_c_ptr()

    end function py_z1frnb

end module py_cfml_sxtal_geom
