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

module extension_cfml_reflections

    use forpy_mod
    use iso_c_binding

    use CFML_GlobalDeps, only: Clear_Error,Err_CFML
    use CFML_gSpaceGroups, only: SPG_Type,Set_SpaceGroup
    use CFML_Metrics, only: Cell_G_Type,Set_Crystal_Cell
    use CFML_Reflections, only: RefList_Type,Hkl_Gen_Sxtal
    use extension_cfml_messages, only: check_error
    use wraps_cfml_reflections

    implicit none

    contains

    function py_generate_reflections(self_ptr,args_ptr) result(resul) bind(c)
        !! author: Nebil A. Katcho
        !! date: 20/03/2023
        !! display: public
        !! proc_internals: true
        !! summary: Generate a list of reflections for a given space group
        !
        !!  Generate a list of reflections for a given space group
        !!
        !!  ARGS_PTR = (spg_symb,cell,sintlmin,sintlmax)
        !   --------           -----------         -----------
        !   Variable           Python type         Description
        !   --------           -----------         -----------
        !   spg_symb           string              space group symbol
        !   nd_cell            ndarray(6,float32)  cell parameters (a,b,c,alpha,beta,gamma)
        !   sintlmin           float               minimum vale for sin(theta) / lambda
        !   sintlmax           float               maximum value for sin(theta) / lambda
        !
        !!  RESUL = (ierr,di_hkls)
        !   --------           -----------         -----------
        !   Variable           Python type         Description
        !   --------           -----------         -----------
        !   ierr               integer             if ierr /= 0, an error occurred
        !   di_hkls            dictionary

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Local variables
        integer :: ierror
        real :: stlmin,stlmax
        real, dimension(:), pointer :: p_cell
        character(len=:), allocatable :: spg_symb !! Space group symbol
        type(Cell_G_Type) :: cell                 !! Crystal cell
        type(SPG_Type) :: spg                     !! Space group
        type(RefList_Type) :: hkls                !! List of calculated reflections
        type(dict) :: di_hkls                     !! Dictionary with the list of calculated reflections
        type(ndarray) :: nd_cell                  !! cell parameters (a,b,c,alpha,beta,gamma)
        type(object) :: item
        type(tuple) :: args,ret

        call Clear_Error()
        ierror = 0

        ! Get arguments
        call unsafe_cast_from_c_ptr(args,args_ptr)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) ierror = cast(spg_symb,item)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(nd_cell,item)
        if (ierror == 0) ierror = nd_cell%get_data(p_cell)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) ierror = cast(stlmin,item)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) ierror = cast(stlmax,item)
        if (ierror /= 0) then
            ierror = EXCEPTION_ERROR
            call raise_exception(RuntimeError,'py_generate_reflections: Error getting !arguments')
        end if

        if (ierror == 0) call Set_SpaceGroup('P 1',spg)
        if (ierror == 0) call check_error('py_generate_reflections',ierror)
        if (ierror == 0) call Set_Crystal_Cell(p_cell(1:3),p_cell(4:6),cell)
        if (ierror == 0) call check_error('py_generate_reflections',ierror)
        if (ierror == 0) call HKL_Gen_Sxtal(cell,spg,stlmin,stlmax,hkls)
        if (ierror == 0) call check_error('py_HKL_Gen_Sxtal',ierror)
        if (ierror == 0) call wrap_reflist_type(hkls,di_hkls,ierror)

        ! Return tuple
        if (ierror == 0) then
            ierror = tuple_create(ret,2)
            ierror = ret%setitem(0,0)
            ierror = ret%setitem(1,di_hkls)
        else
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,-1)
        end if
        resul = ret%get_c_ptr()

    end function py_generate_reflections

end module extension_cfml_reflections