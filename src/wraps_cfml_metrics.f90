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

module wraps_cfml_metrics

    use forpy_mod
    use cfml_metrics

    implicit none

    contains

    subroutine wrap_cell_type(for_var,dic_var,ierror)

        ! Arguments
        class(cell_type), intent(in)     :: for_var
        type(dict),       intent(inout) :: dic_var
        integer,          intent(out)   :: ierror

        ! Local variables
        type(ndarray) :: nd_cell,nd_rcell,nd_scell,nd_ang,nd_rang,nd_sang,nd_GD,nd_GR,&
                         nd_cr_orth_cel,nd_orth_cr_cel,nd_bl_m,nd_inv_bl_m

        ierror = 0
        if (ierror == 0) ierror = ndarray_create(nd_cell,for_var%cell)
        if (ierror == 0) ierror = dic_var%setitem('cell',nd_cell)
        if (ierror == 0) ierror = ndarray_create(nd_scell,for_var%scell)
        if (ierror == 0) ierror = dic_var%setitem('scell',nd_scell)
        if (ierror == 0) ierror = ndarray_create(nd_ang,for_var%ang)
        if (ierror == 0) ierror = dic_var%setitem('ang',nd_ang)
        if (ierror == 0) ierror = ndarray_create(nd_sang,for_var%sang)
        if (ierror == 0) ierror = dic_var%setitem('sang',nd_sang)
        if (ierror == 0) ierror = dic_var%setitem('vol',for_var%vol)
        if (ierror == 0) ierror = dic_var%setitem('svol',for_var%svol)
        select type (A => for_var)
            type is (cell_g_type)
                if (ierror == 0) ierror = ndarray_create(nd_rcell,A%rcell)
                if (ierror == 0) ierror = dic_var%setitem('rcell',nd_rcell)
                if (ierror == 0) ierror = ndarray_create(nd_rang,A%rang)
                if (ierror == 0) ierror = dic_var%setitem('rang',nd_rang)
                if (ierror == 0) ierror = dic_var%setitem('rvol',A%rvol)
                if (ierror == 0) ierror = ndarray_create(nd_GD,A%GD)
                if (ierror == 0) ierror = dic_var%setitem('GD',nd_GD)
                if (ierror == 0) ierror = ndarray_create(nd_GR,A%GR)
                if (ierror == 0) ierror = dic_var%setitem('GR',nd_GR)
                if (ierror == 0) ierror = ndarray_create(nd_cr_orth_cel,A%cr_orth_cel)
                if (ierror == 0) ierror = dic_var%setitem('cr_orth_cel',nd_cr_orth_cel)
                if (ierror == 0) ierror = ndarray_create(nd_orth_cr_cel,A%orth_cr_cel)
                if (ierror == 0) ierror = dic_var%setitem('orth_cr_cel',nd_orth_cr_cel)
                if (ierror == 0) ierror = ndarray_create(nd_bl_m,A%bl_m)
                if (ierror == 0) ierror = dic_var%setitem('bl_m',nd_bl_m)
                if (ierror == 0) ierror = ndarray_create(nd_inv_bl_m,A%inv_bl_m)
                if (ierror == 0) ierror = dic_var%setitem('inv_bl_m',nd_inv_bl_m)
                if (ierror == 0) ierror = dic_var%setitem('carttype',A%carttype)
        end select
        select type (A => for_var)
            type is (cell_type)
                if (ierror == 0) ierror = dic_var%setitem('fortran_type','cell_type')
            type is (cell_g_type)
                if (ierror == 0) ierror = dic_var%setitem('fortran_type','cell_g_type')
        end select

    end subroutine wrap_cell_type

end module wraps_cfml_metrics