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

module py_cfml_vtk

    use forpy_mod
    use iso_c_binding

    use cfml_globaldeps
    use cfml_vtk

    implicit none

    type(PythonModule), save :: mod_vtk
    type(PythonMethodTable), save :: table_vtk

    contains

    ! Initialization function for Python 3
    ! Called when importing module
    ! Must use bind(c, name="PyInit_<module name>")
    ! Return value must be type(c_ptr),
    ! use the return value of PythonModule%init
    function PyInit_py_cfml_vtk() bind(c,name="PyInit_py_cfml_vtk") result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_vtk

        ! Local variables
        type(c_ptr) :: m

        m = Init()

    end function PyInit_py_cfml_vtk

    function Init() result(m)

        ! Local variables
        type(c_ptr) :: m
        integer :: ierror

        ierror = Forpy_Initialize()

        ! Build method table
        call table_vtk%init(2)
        call table_vtk%add_method("scan_arrays","scan_arrays",METH_VARARGS,c_funloc(py_scan_arrays))
        call table_vtk%add_method("scan_limits","scan_limits",METH_VARARGS,c_funloc(py_scan_limits))

        ! Build mod_vtk
        m = mod_vtk%init("py_cfml_vtk","A Python API for CrysFML08",table_vtk)

    end function Init

    function py_scan_arrays(self_ptr,args_ptr) result(resul) bind(c)
        !! author: ILL Scientific Computing Group
        !! date: 28/03/2023
        !! display: public
        !! proc_internals: true
        !! summary: Wrapper for function scan_arrays.
        !! summary: Build vtk arrays for 3D representation of a scan.
        !!
        !! ARGS_PTR = (nd_counts,th,nd_vtk_points,nd_vtk_cells,nd_vtk_counts)
        !!
        !! RESUL = (ierr,err_cfml%msg)
        !  --------           -----------         -----------
        !  Variable           Python type         Description
        !  --------           -----------         -----------
        !  nd_counts          ndarray[:,:,:]      scan counts
        !                     np.int32
        !                     order = C
        !  th                 integer             threshold
        !  nd_vtk_points      ndarray[:,:]
        !                     np.int32
        !                     order = C
        !  nd_vtk_cells       ndarray[:,:]
        !                     np.int64
        !                     order = C
        !  nd_vtk_counts      ndarray[:]
        !                     np.int32

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Python / Fortran interface variables
        type(ndarray) :: nd_counts       !! scan counts
        integer       :: th              !! threshold
        type(ndarray) :: nd_vtk_points
        type(ndarray) :: nd_vtk_cells
        type(ndarray) :: nd_vtk_counts
        integer       :: ierr            !! error flag

        ! Local variables
        integer :: ierror
        integer(kind=4), dimension(:), pointer :: p_vtk_counts
        integer(kind=8), dimension(:,:), pointer :: p_vtk_cells
        integer(kind=4), dimension(:,:), pointer :: p_vtk_points
        integer(kind=4), dimension(:,:,:), pointer :: p_counts
        type(object) :: item
        type(tuple) :: args,ret

        ! Reset error variable
        ierr   = 0
        ierror = 0
        call clear_error()

        ! In case of exception return C_NULL_PTR
        resul = C_NULL_PTR

        ! Unwrap_arguments
        call unsafe_cast_from_c_ptr(args,args_ptr)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) ierror = cast(nd_counts,item)
        if (ierror == 0) ierror = nd_counts%get_data(p_counts,order='C')
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(th,item)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) ierror = cast(nd_vtk_points,item)
        if (ierror == 0) ierror = nd_vtk_points%get_data(p_vtk_points,order='C')
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) ierror = cast(nd_vtk_cells,item)
        if (ierror == 0) ierror = nd_vtk_cells%get_data(p_vtk_cells,order='C')
        if (ierror == 0) ierror = args%getitem(item,4)
        if (ierror == 0) ierror = cast(nd_vtk_counts,item)
        if (ierror == 0) ierror = nd_vtk_counts%get_data(p_vtk_counts)
        if (ierror /= 0) then
            err_cfml%ierr = -1
            err_cfml%msg = 'py_scan_arrays: Error getting arguments'
        end if

        ! Call CrysFML procedure
        if (ierror == 0) call scan_arrays(p_counts,th,p_vtk_points,p_vtk_cells,p_vtk_counts)
        if (ierror == 0) ierror = err_cfml%ierr

        ! Return tuple
        if (ierror == 0) then
            ierror = tuple_create(ret,2)
            ierror = ret%setitem(0,ierr)
            ierror = ret%setitem(1,trim(err_cfml%msg))
        else
            ierr = ierror
            ierror = tuple_create(ret,2)
            ierror = ret%setitem(0,ierr)
            ierror = ret%setitem(1,trim(err_cfml%msg))
        end if
        resul = ret%get_c_ptr()

    end function py_scan_arrays

    function py_scan_limits(self_ptr,args_ptr) result(resul) bind(c)
        !! author: ILL Scientific Computing Group
        !! date: 28/03/2023
        !! display: public
        !! proc_internals: true
        !! summary: Wrapper for function scan_limits.
        !! summary: Estimate a convenient threshold for a 3D representation of a scan with VTK
        !!
        !! ARGS_PTR = (nd_counts)
        !!
        !! RESUL = (ierr,err_cfml%msg,th,max_cnt,npoints)
        !  --------           -----------         -----------
        !  Variable           Python type         Description
        !  --------           -----------         -----------
        !  nd_counts          ndarray[:,:,:]      scan counts
        !                     np.int32,
        !                     order = C
        !  ierr               integer             if ierr /= 0, an error occurred
        !  err_cfml%msg       string              error message
        !  th                 integer             threshold
        !  max_cnt            integer             maximum counts / pixel
        !  npoints            integer             number of points in the range [th,max_cnt]

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Python / Fortran interface variables
        type(ndarray) :: nd_counts !! scan counts
        integer       :: ierr      !! error flag
        integer       :: th        !! threshold
        integer       :: max_cnt   !! maximum number of counts / pixel
        integer       :: npoints   !! number of points in the range [th,max_cnt]

        ! Local variables
        integer :: ierror
        integer(kind=4), dimension(:,:,:), pointer :: p_counts
        type(object) :: item
        type(tuple) :: args,ret

        ! Reset error variable
        ierr   = 0
        ierror = 0
        call clear_error()

        ! In case of exception return C_NULL_PTR
        resul = C_NULL_PTR

        ! Unwrap_arguments
        call unsafe_cast_from_c_ptr(args,args_ptr)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) ierror = cast(nd_counts,item)
        if (ierror == 0) ierror = nd_counts%get_data(p_counts,order='C')
        if (ierror /= 0) then
            err_cfml%ierr = -1
            err_cfml%msg = 'py_scan_limits: Error getting arguments'
        end if

        ! Call CrysFML procedure
        if (ierror == 0) call scan_limits(p_counts,th,max_cnt,npoints)
        if (ierror == 0) ierror = err_cfml%ierr

        ! Return tuple
        if (ierror == 0) then
            ierror = tuple_create(ret,5)
            ierror = ret%setitem(0,ierr)
            ierror = ret%setitem(1,trim(err_cfml%msg))
            ierror = ret%setitem(2,th)
            ierror = ret%setitem(3,max_cnt)
            ierror = ret%setitem(4,npoints)
        else
            ierr = ierror
            ierror = tuple_create(ret,2)
            ierror = ret%setitem(0,ierr)
            ierror = ret%setitem(1,trim(err_cfml%msg))
        end if
        resul = ret%get_c_ptr()

    end function py_scan_limits

end module py_cfml_vtk