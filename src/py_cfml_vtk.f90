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

    use cfml_globaldeps, only: err_cfml,clear_error
	use cfml_ill_instrm_data, only: current_instrm,read_current_instrm
    use cfml_python, only: check_number_of_arguments,get_var_from_item,ndarray_to_pointer,pointer_to_array,pointer_to_array_alloc
    use cfml_strings, only: l_case
    use cfml_sxtal_geom, only: psd_convert,z1frmd,z1frnb
    use cfml_vtk
	use nexus_mod

    implicit none


    type(nexus_type) :: nexus
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
        call table_vtk%init(4)
        call table_vtk%add_method("scan_arrays","py_scan_arrays",METH_VARARGS,c_funloc(py_scan_arrays))
        call table_vtk%add_method("scan_limits","py_scan_limits",METH_VARARGS,c_funloc(py_scan_limits))
        call table_vtk%add_method("scan_load","py_scan_load",METH_VARARGS,c_funloc(py_scan_load))
        call table_vtk%add_method("scan_to_reciprocal","py_scan_to_reciprocal",METH_VARARGS,c_funloc(py_scan_to_reciprocal))

        ! Build mod_vtk
        m = mod_vtk%init("py_cfml_vtk","A Python API for CrysFML08",table_vtk)

    end function Init

    function py_scan_arrays(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(ndarray) :: nd_counts       !! scan counts
        integer       :: th              !! threshold
        type(ndarray) :: nd_vtk_points
        type(ndarray) :: nd_vtk_cells
        type(ndarray) :: nd_vtk_counts

        ! Local variables
        integer, parameter :: NMANDATORY = 5
        integer :: ierror,narg
        integer(kind=4), dimension(:), pointer :: p_vtk_counts
        integer(kind=8), dimension(:,:), pointer :: p_vtk_cells
        integer(kind=4), dimension(:,:), pointer :: p_vtk_points
        integer(kind=4), dimension(:,:,:), pointer :: p_counts
        character(len=1) :: order
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Check the number of items
        call check_number_of_arguments('py_scan_arrays',args,NMANDATORY,narg,ierror)

        ! Get arguments
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_scan_arrays','counts',item,nd_counts,ierror)
        if (ierror == 0) call ndarray_to_pointer('py_scan_arrays','counts',nd_counts,p_counts,ierror,order)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_scan_arrays','th',item,th,ierror)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) call get_var_from_item('py_scan_arrays','vtk_points',item,nd_vtk_points,ierror)
        if (ierror == 0) call ndarray_to_pointer('py_scan_arrays','vtk_points',nd_vtk_points,p_vtk_points,ierror,order)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) call get_var_from_item('py_scan_arrays','vtk_cells',item,nd_vtk_cells,ierror)
        if (ierror == 0) call ndarray_to_pointer('py_scan_arrays','vtk_cells',nd_vtk_cells,p_vtk_cells,ierror,order)
        if (ierror == 0) ierror = args%getitem(item,4)
        if (ierror == 0) call get_var_from_item('py_scan_arrays','vtk_counts',item,nd_vtk_counts,ierror)
        if (ierror == 0) call ndarray_to_pointer('py_scan_arrays','vtk_counts',nd_vtk_counts,p_vtk_counts,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_scan_arrays: error parsing arguments'
        end if

        ! Call Fortran procedure
        if (ierror == 0) call scan_arrays(p_counts,th,p_vtk_points,p_vtk_cells,p_vtk_counts)
        if (ierror == 0) ierror = err_cfml%ierr

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,2)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        resul = ret%get_c_ptr()

    end function py_scan_arrays

    function py_scan_limits(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(ndarray) :: nd_counts !! scan counts

        ! Local variables
        integer, parameter :: NMANDATORY = 1
        integer :: ierror,narg
        integer :: th        !! threshold
        integer :: max_cnt   !! maximum number of counts / pixel
        integer :: npoints   !! number of points in the range [th,max_cnt]
        integer(kind=4), dimension(:,:,:), pointer :: p_counts
        character(len=1) :: order
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Check the number of items
        call check_number_of_arguments('py_scan_limits',args,NMANDATORY,narg,ierror)

        ! Get arguments
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_scan_limits','counts',item,nd_counts,ierror)
        if (ierror == 0) call ndarray_to_pointer('py_scan_limits','counts',nd_counts,p_counts,ierror,order)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_scan_limits: error parsing arguments'
        end if

        ! Call Fortran procedure
        if (ierror == 0) call scan_limits(p_counts,th,max_cnt,npoints)
        if (ierror == 0) ierror = err_cfml%ierr

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,5)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,th)
        ierror = ret%setitem(3,max_cnt)
        ierror = ret%setitem(4,npoints)
        resul = ret%get_c_ptr()

    end function py_scan_limits

    function py_scan_load(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
		character(len=:), allocatable :: file_nexus
		character(len=:), allocatable :: file_instrument

		! Local variables
        integer, parameter :: NMANDATORY = 2
		integer :: i,j,k,ii,jj
        integer :: ierror,narg,orig
        integer :: min_cnt   !! threshold
        integer :: max_cnt   !! maximum number of counts / pixel
        integer :: npoints   !! number of points in the range [min_cnt,max_cnt]
		integer, dimension(:,:,:), allocatable :: counts
		logical :: presente
        type(object) :: item
        type(tuple) :: args,ret

		ierror = 0
        call clear_error()

		! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Check the number of items
        call check_number_of_arguments('py_scan_load',args,NMANDATORY,narg,ierror)

		! Get arguments
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_scan_load','file_nexus',item,file_nexus,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_scan_load','file_instrument',item,file_instrument,ierror)
		if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_scan_load: error parsing arguments'
        end if

		! Transformation to the reciprocal space
		if (ierror == 0) then
			! Read instrument
			inquire(file = file_instrument, exist = presente)
			if (.not. presente) then
				err_cfml%ierr = 1
				err_cfml%msg = 'py_scan_load: Instrument file '//trim(file_instrument)//' not found'
			else
				call read_current_instrm(trim(file_instrument))
			end if
			! Read nexus
			if (err_cfml%ierr == 0) then
				call initialize_nexus(nexus)
				inquire(file=file_nexus,exist=presente)
				if (.not. presente) then
					err_cfml%ierr = 1
					err_cfml%msg = 'py_scan_load: Nexus file '//trim(file_nexus)//' not found'
				else
					call read_nexus(file_nexus,nexus)
					if (err_nexus) then
						err_cfml%ierr = 1
						err_cfml%msg = trim(err_nexus_mess)
					end if
				end if
			end if
			! Coordinates in Busing-Levy
            current_instrm%data_ordering = L_Case(current_instrm%data_ordering)
            i = index(current_instrm%data_ordering,'top')
            j = index(current_instrm%data_ordering,'left')
            if (i > 0) then
                if (j > 0) then
                    orig = 0 ! Top Left
                else
                    orig = 1 ! Top Right
                end if
            else
                if (j > 0) then
                    orig = 3 ! Bottom Left
                else
                    orig = 2 ! Bottom Right
                end if
            end if
			if (err_cfml%ierr == 0) then
                allocate(counts(nexus%nz,nexus%nx,nexus%nf))
                counts = nexus%counts
                do k = 1 , nexus%nf
                    do j = 1 , nexus%nx
                        jj = j
                        if (orig == 1 .or. orig == 2) jj = current_instrm%np_horiz - jj + 1
                        if (current_instrm%bl_frame == 'z-down') jj = current_instrm%np_horiz - jj + 1
                        do i = 1 , nexus%nz
                            ii = i
                            if (orig == 0 .or. orig == 1) ii = current_instrm%np_vert  - ii + 1
                            if (current_instrm%bl_frame == 'z-down') ii = current_instrm%np_vert - ii + 1
                            nexus%counts(ii,jj,k) = counts(i,j,k)
                        end do
                    end do
                end do
                deallocate(counts)
			end if
            ! Scan limits
            if (err_cfml%ierr == 0) call scan_limits(nexus%counts,min_cnt,max_cnt,npoints)
		end if

		! Return
        ierror = tuple_create(ret,5)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,npoints)
        ierror = ret%setitem(3,min_cnt)
        ierror = ret%setitem(4,max_cnt)
        resul = ret%get_c_ptr()

    end function py_scan_load

    function py_scan_to_reciprocal(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        integer       :: th              !! threshold
        type(ndarray) :: nd_vtk_points
        type(ndarray) :: nd_vtk_cells
        type(ndarray) :: nd_vtk_counts

        ! Local variables
        integer, parameter :: NMANDATORY = 4
        integer :: ierror,narg,i,j,k,n
        integer(kind=8), dimension(:,:), pointer :: p_vtk_cells
        real :: ga_D,ga_P,nu_D,nu_P,px,pz,x_D,z_D,chi,phi,ome
        real, dimension(3) :: doff,z1
        real, dimension(:), pointer :: p_vtk_counts
        real, dimension(:,:), pointer :: p_vtk_points
        character(len=1) :: order
        character(len=:), allocatable :: name_inst
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        call clear_error()
        name_inst = l_case(current_instrm%name_inst)
        doff = current_instrm%det_offsets(:)

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Check the number of items
        call check_number_of_arguments('py_scan_to_reciprocal',args,NMANDATORY,narg,ierror)

        ! Get arguments
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_scan_to_reciprocal','th',item,th,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_scan_to_reciprocal','vtk_points',item,nd_vtk_points,ierror)
        if (ierror == 0) call ndarray_to_pointer('py_scan_to_reciprocal','vtk_points',nd_vtk_points,p_vtk_points,ierror,order)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) call get_var_from_item('py_scan_to_reciprocal','vtk_cells',item,nd_vtk_cells,ierror)
        if (ierror == 0) call ndarray_to_pointer('py_scan_to_reciprocal','vtk_cells',nd_vtk_cells,p_vtk_cells,ierror,order)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) call get_var_from_item('py_scan_to_reciprocal','vtk_counts',item,nd_vtk_counts,ierror)
        if (ierror == 0) call ndarray_to_pointer('py_scan_to_reciprocal','vtk_counts',nd_vtk_counts,p_vtk_counts,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_scan_to_reciprocal: error parsing arguments'
        end if

        ! Convert to reciprocal space
        n = 0
        do k = 1 , size(nexus%counts,3) ! frames
            if (name_inst == 'd10' .or. name_inst == 'xtremed') then
                ga_D = nexus%angles(8,k)
            else
                ga_D = nexus%angles(4,k)
            end if
            nu_D = nexus%angles(7,k)
            phi = nexus%angles(1,k) + current_instrm%ang_offsets(5)
            chi = nexus%angles(2,k) + current_instrm%ang_offsets(4)
            if (nexus%scan_type == 'canne') then
                ome = nexus%angles(6,k) + current_instrm%ang_offsets(3)
            else
                ome = nexus%angles(3,k) + current_instrm%ang_offsets(3)
            end if
            do j = 1 , size(nexus%counts,2) ! x
                px = j
                do i = 1 , size(nexus%counts,1) ! z
                    pz = i
                    if (nexus%counts(i,j,k) >= th) then
                        n = n + 1
                        if (n > size(p_vtk_counts)) then
                            err_cfml%ierr = -1
                            err_cfml%flag = .true.
                            err_cfml%msg = 'py_scan_to_reciprocal: threshold inconsistent with array dimension'
                            return
                        end if
                        if (abs(current_instrm%ga_d) > 0.01) ga_D = current_instrm%ga_d
                        if (abs(current_instrm%nu_d) > 0.01) nu_D = current_instrm%nu_d
                        call psd_convert(current_instrm,1,0,ga_D,nu_D,px,pz,x_D,z_D,ga_P,nu_P,origin=3)
                        if (nexus%geometry == '4C') then
                            z1(:) = z1frmd(current_instrm%wave,chi,phi,ga_P,ome,nu_P)
                        else if (nexus%geometry == 'NB') then
                            z1(:) = z1frnb(current_instrm%wave,ga_P,ome,nu_P)
                        end if
                        p_vtk_points(:,n) = z1(:)
                        p_vtk_cells(1,n) = 1
                        p_vtk_cells(2,n) = n
                        p_vtk_counts(n) = nexus%counts(i,j,k)
                    end if
                end do
            end do
        end do

        ! Return
        ierror = tuple_create(ret,2)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        resul = ret%get_c_ptr()

    end function py_scan_to_reciprocal

end module py_cfml_vtk