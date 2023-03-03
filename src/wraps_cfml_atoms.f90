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

module wraps_cfml_atoms

    !<
    ! -------------------
    ! Internal procedures
    ! -------------------
    ! subroutine wrap_atm_type(for_var,dic_var,ierror)
    ! subroutine wrap_atm_type(for_var,dic_var,ierror)
    ! subroutine wrap_atlist_type(for_var,dic_var,ierror)
    !>

    use forpy_mod
    use cfml_atoms

    implicit none

    contains

    subroutine wrap_atm_type(for_var,dic_var,ierror)

        ! Arguments
        class(atm_type), intent(in)    :: for_var
        type(dict),      intent(inout) :: dic_var
        integer,         intent(out)   :: ierror

        ! Local variables
        type(ndarray) :: nd_x,nd_u,nd_moment,nd_ind_ff,nd_varf

        ierror = 0
        if (ierror == 0) ierror = dic_var%setitem('lab',for_var%lab)
        if (ierror == 0) ierror = dic_var%setitem('chemsymb',for_var%chemsymb)
        if (ierror == 0) ierror = dic_var%setitem('sfacsymb',for_var%sfacsymb)
        if (ierror == 0) ierror = dic_var%setitem('z',for_var%z)
        if (ierror == 0) ierror = dic_var%setitem('mult',for_var%mult)
        if (ierror == 0) ierror = dic_var%setitem('charge',for_var%charge)
        if (ierror == 0) ierror = ndarray_create(nd_x,for_var%x)
        if (ierror == 0) ierror = dic_var%setitem('x',nd_x)
        if (ierror == 0) ierror = dic_var%setitem('u_iso',for_var%u_iso)
        if (ierror == 0) ierror = dic_var%setitem('occ',for_var%occ)
        if (ierror == 0) ierror = dic_var%setitem('utype',for_var%utype)
        if (ierror == 0) ierror = dic_var%setitem('thtype',for_var%thtype)
        if (ierror == 0) ierror = ndarray_create(nd_u,for_var%u)
        if (ierror == 0) ierror = dic_var%setitem('u',nd_u)
        if (ierror == 0) ierror = dic_var%setitem('magnetic',for_var%magnetic)
        if (ierror == 0) ierror = dic_var%setitem('mom',for_var%mom)
        if (ierror == 0) ierror = ndarray_create(nd_moment,for_var%moment)
        if (ierror == 0) ierror = dic_var%setitem('moment',nd_moment)
        if (ierror == 0) ierror = ndarray_create(nd_ind_ff,for_var%ind_ff)
        if (ierror == 0) ierror = dic_var%setitem('ind_ff',nd_ind_ff)
        if (ierror == 0) ierror = dic_var%setitem('atminfo',for_var%atminfo)
        if (ierror == 0) ierror = dic_var%setitem('wyck',for_var%wyck)
        if (ierror == 0) ierror = ndarray_create(nd_varf,for_var%varf)
        if (ierror == 0) ierror = dic_var%setitem('varf',nd_varf)
        if (ierror == 0) ierror = dic_var%setitem('active',for_var%active)

    end subroutine wrap_atm_type

    subroutine wrap_atlist_type(for_var,dic_var,ierror)

        ! Arguments
        type(atlist_type),  intent(in)    :: for_var
        type(dict),         intent(inout) :: dic_var
        integer,            intent(out)   :: ierror

        ! Local variables
        integer :: i
        type(ndarray) :: nd_iph
        type(dict), dimension(:), allocatable :: di_atom
        type(list) :: li_atom,li_active

        ierror = 0
        if (ierror == 0) ierror = dic_var%setitem('natoms',for_var%natoms)
        if (ierror == 0) ierror = dic_var%setitem('mcomp',for_var%mcomp)
        if (ierror == 0) ierror = dic_var%setitem('symm_checked',for_var%symm_checked)
        if (ierror == 0) ierror = list_create(li_active)
        do i = 1 , size(for_var%active)
            if (ierror == 0) ierror = li_active%append(for_var%active(i))
        end do
        if (ierror == 0) ierror = dic_var%setitem('active',li_active)
        if (ierror == 0) ierror = ndarray_create(nd_iph,for_var%iph)
        if (ierror == 0) ierror = dic_var%setitem('iph',nd_iph)
        if (ierror == 0) ierror = list_create(li_atom)
        if (ierror == 0) allocate(di_atom(for_var%natoms))
        do i = 1 , for_var%natoms
            ierror = dict_create(di_atom(i))
            if (ierror == 0) call wrap_atm_type(for_var%atom(i),di_atom(i),ierror)
            if (ierror == 0) ierror = li_atom%append(di_atom(i))
        end do
        if (ierror == 0) ierror = dic_var%setitem('atom',li_atom)

    end subroutine wrap_atlist_type

end module wraps_cfml_atoms