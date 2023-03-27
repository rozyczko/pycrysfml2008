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

    use cfml_globaldeps, only: clear_error,err_cfml
    use cfml_gspacegroups, only: spg_type,set_spacegroup
    use cfml_messages, only: set_python_error_flag
    use cfml_metrics, only: cell_g_type,set_crystal_cell
    use cfml_reflections, only: reflist_type,hkl_gen_sxtal,wrap_reflist_type

    implicit none

    contains

    function py_generate_reflections(self_ptr,args_ptr) result(resul) bind(c)
        !! author: ILL Scientific Computing Group
        !! date: 27/03/2023
        !! display: public
        !! proc_internals: true
        !! summary: Generate a list of reflections for a given space group
        !
        !!  Generate a list of reflections for a given space group.
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
        !   err_cfml%msg       string              error message
        !   di_hkls            dictionary          wrap of crysfml type RefList_Type

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        character(len=:), allocatable :: spg_symb !! Space group symbol
        type(ndarray)                 :: nd_cell  !! Cell parameters (a,b,c,alpha,beta,gamma)
        real                          :: stlmin   !! Minimum value for sin(theta) / lambda
        real                          :: stlmax   !! Maximum value for sin(theta) / lambda

        ! Variables in resul
        integer                       :: ierr     !! if ierr /= 0, an error ocurred
        type(dict)                    :: di_hkls  !! Dictionary with the list of calculated reflections

        ! Local variables
        integer :: ierror
        real, dimension(:), pointer :: p_cell
        type(Cell_G_Type) :: cell
        type(spg_type) :: spg
        type(RefList_Type) :: hkls
        type(object) :: item
        type(tuple) :: args,ret

        call clear_error()
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
            err_cfml%Ierr = -1
            err_cfml%Msg = 'py_generate_reflections: Error getting arguments'
            call set_python_error_flag(ierror)
        end if

        if (ierror == 0) call set_spacegroup('P 1',spg)
        if (ierror == 0) call set_python_error_flag(ierror)
        if (ierror == 0) call Set_Crystal_Cell(p_cell(1:3),p_cell(4:6),cell)
        if (ierror == 0) call set_python_error_flag(ierror)
        if (ierror == 0) call HKL_Gen_Sxtal(cell,spg,stlmin,stlmax,hkls)
        if (ierror == 0) call set_python_error_flag(ierror)
        if (ierror == 0) ierror = dict_create(di_hkls)
        if (ierror == 0) call wrap_reflist_type(hkls,di_hkls,ierror)

        ! Return tuple
        ierr = ierror
        if (ierror == 0) then
            ierror = tuple_create(ret,3)
            ierror = ret%setitem(0,ierr)
            ierror = ret%setitem(1,trim(err_cfml%msg))
            ierror = ret%setitem(2,di_hkls)
        else
            ierror = tuple_create(ret,2)
            ierror = ret%setitem(0,ierr)
            ierror = ret%setitem(1,trim(err_cfml%msg))
        end if
        resul = ret%get_c_ptr()

    end function py_generate_reflections

end module extension_cfml_reflections