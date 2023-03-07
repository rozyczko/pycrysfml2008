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

module extension_cfml_export_vtk

    !<
    ! --------------------------------
    ! Functions accesibles from Python
    ! --------------------------------
    ! function py_ganu_from_xz(self_ptr,args_ptr) result(resul) bind(c)
    !
    ! -------------------
    ! Internal procedures
    ! -------------------
    ! psd_convert_new(diffractometer,conversion_type,ga_D,nu_D,px,pz,x_D,z_D,ga_P,nu_P,ierr)
    !>

    use forpy_mod
    use iso_c_binding

    use CFML_GlobalDeps, only: Clear_Error
    implicit none

    contains

    function py_vtk_scan_arrays(self_ptr,args_ptr) result(resul) bind(c)

        !< Compute the minimum threshold acceptable and the maximum counts / pixel
        !  for a given scan
        !
        !   Arguments in args_ptr
        !   --------           -----------            -----------
        !   Variable           Python type            Description
        !   --------           -----------            -----------
        !   is_fortran         bool                   c | fortran order of counts
        !   counts             ndarray(nframe,nz,nx;
        !                              dtype=int32)   scan counts
        !   th                 integer                threshold
        !   nd_vtk_counts      ndarray(npoints,2;
        !                              dtype=int32)
        !   nd_vtk_cells       ndarray(npoints,2;
        !                              dtype=int64)
        !   nd_vtk_xyz         ndarray(npoints,3;
        !                              dtype=int32)
        !
        !>

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Local variables
        !  MAX_NPOINTS -> number of stored values (32 bits)
        !                 equivalent to 100MB. VTK raises an
        !                 exception when storage > 2GB. From
        !                 python I don't know how to catch
        !                 this exception
        integer :: ierror,i,j,k,n
        integer :: th
        integer(kind=4), dimension(:), pointer :: p_vtk_counts
        integer(kind=8), dimension(:,:), pointer :: p_vtk_cells
        integer(kind=4), dimension(:,:), pointer :: p_vtk_xyz
        integer(kind=4), dimension(:,:,:), pointer :: p_counts
        logical :: is_fortran
        type(ndarray) :: nd_counts,nd_vtk_counts,nd_vtk_cells,nd_vtk_xyz
        type(object) :: item
        type(tuple) :: args,ret

        call Clear_Error()
        ierror = 0

        ! Get arguments
        call unsafe_cast_from_c_ptr(args,args_ptr)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) ierror = cast(is_fortran,item)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(nd_counts,item)
        if (ierror == 0) then
            if (is_fortran) then
                ierror = nd_counts%get_data(p_counts)
            else
                ierror = nd_counts%get_data(p_counts,order='C')
            end if
        end if
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) ierror = cast(th,item)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) ierror = cast(nd_vtk_counts,item)
        if (ierror == 0) ierror = nd_vtk_counts%get_data(p_vtk_counts)
        if (ierror == 0) ierror = args%getitem(item,4)
        if (ierror == 0) ierror = cast(nd_vtk_cells,item)
        if (ierror == 0) ierror = nd_vtk_cells%get_data(p_vtk_cells,order='C')
        if (ierror == 0) ierror = args%getitem(item,5)
        if (ierror == 0) ierror = cast(nd_vtk_xyz,item)
        if (ierror == 0) ierror = nd_vtk_xyz%get_data(p_vtk_xyz,order='C')
        if (ierror /= 0) then
            ierror = EXCEPTION_ERROR
            call raise_exception(RuntimeError,'py_vtk_scan_arrays: Error getting arguments')
        end if
        ! Build arrays
        n = 0
        if (ierror == 0) then
            do k = 1 , size(p_counts,3)
                do j = 1 , size(p_counts,2)
                    do i = 1 , size(p_counts,1)
                        if (p_counts(i,j,k) >= th) then
                            n = n + 1
                            p_vtk_xyz(1,n) = i-1
                            p_vtk_xyz(2,n) = j-1
                            p_vtk_xyz(3,n) = k-1
                            p_vtk_cells(1,n) = 1
                            p_vtk_cells(2,n) = n
                            p_vtk_counts(n) = p_counts(i,j,k)
                        end if
                    end do
                end do
            end do
        end if
        ! Return tuple
        if (ierror == 0) then
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,0)
        else
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,-1)
        end if
        resul = ret%get_c_ptr()

    end function py_vtk_scan_arrays

    function py_vtk_scan_limits(self_ptr,args_ptr) result(resul) bind(c)

        !< Compute the minimum threshold acceptable and the maximum counts / pixel
        !  for a given scan
        !
        !   Arguments in args_ptr
        !   --------           -----------            -----------
        !   Variable           Python type            Description
        !   --------           -----------            -----------
        !   is_fortran         bool                   c | fortran order of counts
        !   counts             ndarray(frame,z,x;
        !                              dtype=int32)   scan counts
        !>

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Local variables
        !  MAX_NPOINTS -> number of stored values (32 bits)
        !                 equivalent to 100MB. VTK raises an
        !                 exception when storage > 2GB. From
        !                 python I don't know how to catch
        !                 this exception
        integer, parameter :: MAX_NPOINTS = 3125000
        integer, parameter :: MAX_HISTO = 100 ! Maximum number of counts for histogram calculation
        integer :: ierror,i,j,k,n
        integer :: th,max_cnt,npoints
        integer, dimension(0:MAX_HISTO) :: histo
        integer(kind=4), dimension(:,:,:), pointer :: p_counts
        logical :: is_fortran
        type(ndarray) :: nd_counts
        type(object) :: item
        type(tuple) :: args,ret

        call Clear_Error()
        ierror = 0

        ! Get arguments
        call unsafe_cast_from_c_ptr(args,args_ptr)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) ierror = cast(is_fortran,item)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(nd_counts,item)
        if (ierror == 0) then
            if (is_fortran) then
                ierror = nd_counts%get_data(p_counts)
            else
                ierror = nd_counts%get_data(p_counts,order='C')
            end if
        end if
        if (ierror /= 0) then
            ierror = EXCEPTION_ERROR
            call raise_exception(RuntimeError,'py_vtk_scan_limits: Error getting arguments')
        end if
        if (ierror == 0) then
            ! Determine threshold and max_cnt
            histo(:) = 0
            max_cnt  = 0
            do k = 1 , size(p_counts,3)
                do j = 1 , size(p_counts,2)
                    do i = 1 , size(p_counts,1)
                        n = min(p_counts(i,j,k),MAX_HISTO)
                        if (p_counts(i,j,k) > max_cnt) max_cnt = p_counts(i,j,k)
                        histo(n) = histo(n) + 1
                    end do
                end do
            end do
            npoints = size(P_counts,1) * size(P_counts,2) * size(P_counts,3)
            th = 0
            ! Compute threshold
            do
                if (npoints <= MAX_NPOINTS) exit
                if (th == MAX_HISTO) exit
                npoints = npoints - histo(th)
                th = th + 1
            end do
            if (th == MAX_HISTO) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'py_vtk_scan_limits: Threshold cannot be computed')
            end if
        end if
        ! Return tuple
        if (ierror == 0) then
            ierror = tuple_create(ret,4)
            ierror = ret%setitem(0,0)
            ierror = ret%setitem(1,th)
            ierror = ret%setitem(2,max_cnt)
            ierror = ret%setitem(3,npoints)
        else
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,-1)
        end if
        resul = ret%get_c_ptr()

    end function py_vtk_scan_limits

end module extension_cfml_export_vtk