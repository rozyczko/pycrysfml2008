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

module extension_cfml_ioform

    !<
    ! --------------------------------
    ! Functions accesibles from Python
    ! --------------------------------
    ! function py_xtal_structure_from_file(self_ptr,args_ptr) result(resul) bind(c)
    !
    ! -------------------
    ! Internal procedures
    ! -------------------
    ! subroutine get_orbits_cartesian(cell,asu,spg,li_orb)
    ! subroutine wrap_orbit_cartesian(U,cell,orbit,di_orb,ierror)
    !>

    use forpy_mod
    use iso_c_binding

    use CFML_Atoms, only: AtList_Type
    use CFML_GlobalDeps, only: Clear_Error,Err_CFML
    use CFML_gSpaceGroups, only: Get_Orbit,Point_Orbit,Spg_Type
    use CFML_IOForm, only: Read_Xtal_Structure
    use CFML_Metrics, only: Cell_G_Type
    use extension_cfml_messages, only: check_error
    use wraps_cfml_atoms
    use wraps_cfml_metrics

    implicit none

    contains

    function py_xtal_structure_from_file(self_ptr,args_ptr) result(resul) bind(c)

        !> Read the crystal structure from a file. Returns the unit cell

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Local variables
        integer :: ierror,i
        character(len=:), allocatable :: f_name,m
        type(Cell_G_Type) :: cell
        class(Spg_Type), allocatable :: spg
        type(AtList_Type) :: asu
        type(dict) :: di_cell,di_asu
        type(list) :: li_orb
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

        call Clear_Error()

        ! Get filename
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%getitem(item,0)
        if (ierror == 0) ierror = cast(f_name,item)
        if (ierror /= 0) then
            ierror = EXCEPTION_ERROR
            call raise_exception(RuntimeError,'xtal_structure_from_file: Error getting name of the structure file')
        end if

        ! Read crystal structure
        if (ierror == 0) call Read_Xtal_Structure(f_name,cell,spg,asu)
        call check_error('py_xtal_structure_from_file',ierror)
        if (ierror == 0 .and. spg%D > 4) then
            ierror = EXCEPTION_ERROR
            call raise_exception(RuntimeError,'xtal_structure_from_file: Superspace groups not implemented yet')
        end if

        ! Get the orbits for each atom
        if (ierror == 0) ierror = list_create(li_orb)
        if (ierror == 0) call get_orbits_cartesian(cell,asu,spg,li_orb,ierror)

        ! Wrap
        if (ierror == 0) ierror = dict_create(di_cell)
        if (ierror == 0) call wrap_cell_g_type(cell,di_cell,ierror)
        if (ierror == 0) ierror = dict_create(di_asu)
        if (ierror == 0) call wrap_atlist_type(asu,di_asu,ierror)

        ! Return tuple
        if (ierror == 0) then
            ierror = tuple_create(ret,3)
            if (ierror == 0) ierror = ret%setitem(0,di_cell)
            if (ierror == 0) ierror = ret%setitem(1,di_asu)
            if (ierror == 0) ierror = ret%setitem(2,li_orb)
        end if
        if (ierror == 0) then
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_xtal_structure_from_file

    subroutine get_orbits_cartesian(cell,asu,spg,li_orb,ierror)

        ! Arguments
        type(Cell_G_Type),            intent(in)    :: cell
        type(AtList_Type),            intent(in)    :: asu
        class(Spg_Type),              intent(in)    :: spg
        type(list),                   intent(inout) :: li_orb
        integer,                      intent(inout) :: ierror

        ! Local variables
        integer :: i,j
        real :: a
        real, dimension(3,3) :: U ! Matrix used for computing magnetic moment in cartesian coordinates
        character(len=:), allocatable :: m
        type(Point_Orbit) :: orbit
        type(dict), dimension(:), allocatable :: di_orb

        ierror = 0
        allocate(di_orb(asu%natoms))
        do i = 1 , 3
            a = 0.0
            do j = 1 , 3
                a = a + cell%cr_orth_cel(j,i)**2
            end do
            U(:,i) = cell%cr_orth_cel(:,i) / sqrt(a)
        end do
        do i = 1 , asu%natoms
            if (ierror == 0) call Get_Orbit(asu%atom(i)%x,spg,orbit,asu%atom(i)%moment)
            call check_error('get_orbits_cartesian',ierror)
            if (ierror == 0) ierror = dict_create(di_orb(i))
            if (ierror == 0) call wrap_orbit_cartesian(U,cell,orbit,di_orb(i),ierror)
            if (ierror == 0) ierror = li_orb%append(di_orb(i))
        end do

    end subroutine get_orbits_cartesian

    subroutine wrap_orbit_cartesian(U,cell,orbit,di_orb,ierror)

        ! Arguments
        real, dimension(3,3), intent(in)    :: U
        type(Cell_G_Type),    intent(in)    :: cell
        type(Point_Orbit),    intent(in)    :: orbit
        type(dict),           intent(inout) :: di_orb
        integer,              intent(inout) :: ierror

        ! Local variables
        real, dimension(:), allocatable :: phi_y,phi_z,mmom
        character(len=:), allocatable :: m
        type(ndarray) :: pos,pos_c,mom,mom_c

        ierror = di_orb%setitem('mult',orbit%mult)
        if (ierror == 0) ierror = ndarray_create(pos,orbit%pos)
        if (ierror == 0) ierror = di_orb%setitem('pos',pos)
        if (ierror == 0) ierror = ndarray_create(pos_c,matmul(cell%cr_orth_cel,orbit%pos))
        if (ierror == 0) ierror = di_orb%setitem('pos_c',pos_c)
        if (ierror == 0) ierror = ndarray_create(mom,orbit%mom)
        if (ierror == 0) ierror = di_orb%setitem('mom',mom)
        if (ierror == 0) ierror = ndarray_create(mom_c,matmul(U,orbit%mom))
        if (ierror == 0) ierror = di_orb%setitem('mom_c',mom_c)

    end subroutine wrap_orbit_cartesian

end module extension_cfml_ioform