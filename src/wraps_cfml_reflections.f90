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

module wraps_cfml_reflections

    use forpy_mod
    use cfml_reflections

    implicit none

    contains

    subroutine wrap_reflist_type(for_var,dic_var,ierror)

        ! Arguments
        class(reflist_type), intent(in)    :: for_var
        type(dict),          intent(inout) :: dic_var
        integer,             intent(out)   :: ierror

        ! Local variables
        integer :: i
        type(list) :: li_ref
        type(dict), dimension(:), allocatable :: di_ref

        ierror = 0
        if (ierror == 0) ierror = dic_var%setitem('nref',for_var%nref)
        if (ierror == 0) then
            if (ierror == 0) ierror = list_create(li_ref)
            allocate(di_ref(for_var%nref))
            do i = 1 , for_var%nref
                if (ierror == 0) ierror = dict_create(di_ref(i))
                if (ierror == 0) call wrap_refl_type(for_var%ref(i),di_ref(i),ierror)
                if (ierror == 0) ierror = li_ref%append(di_ref(i))
            end do
        end if
        if (ierror == 0) ierror = dic_var%setitem('ref',li_ref)

    end subroutine wrap_reflist_type

    subroutine wrap_refl_type(for_var,dic_var,ierror)

        ! Arguments
        class(refl_type), intent(in)    :: for_var
        type(dict),       intent(inout) :: dic_var
        integer,          intent(out)   :: ierror

        ! Local variables
        type(ndarray) :: nd_h
        ierror = 0
        if (ierror == 0) ierror = ndarray_create(nd_h,for_var%h)
        if (ierror == 0) ierror = dic_var%setitem('h',nd_h)
        if (ierror == 0) ierror = dic_var%setitem('mult',for_var%mult)
        if (ierror == 0) ierror = dic_var%setitem('s',for_var%s)
        if (ierror == 0) ierror = dic_var%setitem('imag',for_var%imag)
        if (ierror == 0) ierror = dic_var%setitem('pcoeff',for_var%pcoeff)
        select type (A => for_var)
            class is (srefl_type)
                if (ierror == 0) ierror = dic_var%setitem('fo',A%fo)
                if (ierror == 0) ierror = dic_var%setitem('fc',A%fc)
                if (ierror == 0) ierror = dic_var%setitem('sfo',A%sfo)
                if (ierror == 0) ierror = dic_var%setitem('phase',A%phase)
                if (ierror == 0) ierror = dic_var%setitem('a',A%a)
                if (ierror == 0) ierror = dic_var%setitem('b',A%b)
                if (ierror == 0) ierror = dic_var%setitem('w',A%w)
        end select
        select type (A => for_var)
            type is (refl_type)
                if (ierror == 0) ierror = dic_var%setitem('fortran_type','refl_type')
            type is (srefl_type)
                if (ierror == 0) ierror = dic_var%setitem('fortran_type','srefl_type')
        end select

    end subroutine wrap_refl_type

end module wraps_cfml_reflections