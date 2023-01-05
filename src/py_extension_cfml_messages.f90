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

module extension_cfml_messages

    !<
    ! -------------------
    ! Internal procedures
    ! -------------------
    ! subroutine check_error(procedure,ierror)
    !>

    use forpy_mod
    use iso_c_binding

    use CFML_GlobalDeps, only: Err_CFML

    contains

    subroutine check_error(procedure,ierror)

        ! Arguments
        character(len=*), intent(in)    :: procedure
        integer,          intent(inout) :: ierror

        if (err_cfml%flag) then
            ierror = EXCEPTION_ERROR
            call raise_exception(RuntimeError,procedure//': '//trim(err_cfml%msg))
        end if

    end subroutine check_error

end module extension_cfml_messages