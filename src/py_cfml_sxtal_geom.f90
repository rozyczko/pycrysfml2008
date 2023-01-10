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

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Arguments for the Fortran procedure
        real(kind=cp) :: wave ! CrysFML type: real
        real(kind=cp) :: ch   ! CrysFML type: real
        real(kind=cp) :: ph   ! CrysFML type: real
        real(kind=cp) :: ga   ! CrysFML type: real
        real(kind=cp) :: om   ! CrysFML type: real
        real(kind=cp) :: nu   ! CrysFML type: real
        real(kind=cp), dimension(3) :: z1 ! CrysFML type: real, dimension(3)

        ! Local variables
        integer :: ierror
        type(ndarray) :: nd_z1
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

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
        if (ierror == 0) ierror = tuple_create(ret,1)
        if (ierror == 0) ierror = ndarray_create(nd_z1,z1)
        if (ierror == 0) ierror = ret%setitem(0,nd_z1)
        if (ierror == 0) then
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_z1frmd

end module py_cfml_sxtal_geom
