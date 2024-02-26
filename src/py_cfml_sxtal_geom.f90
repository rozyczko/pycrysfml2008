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

module py_cfml_sxtal_geom

    use forpy_mod
    use iso_c_binding

    use cfml_globaldeps, only: err_cfml,clear_error
    use cfml_python, only: check_number_of_arguments,get_var_from_item,ndarray_to_pointer,pointer_to_array,pointer_to_array_alloc,unwrap_dict_item,unwrap_dict_item_string_alloc
    use cfml_sxtal_geom, only: ganu_from_xz,ubfrqcel,z1frmd,z1frnb

    implicit none

    type(PythonModule), save :: mod_sxtal_geom
    type(PythonMethodTable), save :: table_sxtal_geom

    contains

    ! Initialization function for Python 3
    ! Called when importing module
    ! Must use bind(c, name="PyInit_<module name>")
    ! Return value must be type(c_ptr),
    ! use the return value of PythonModule%init
    function PyInit_py_cfml_sxtal_geom() bind(c,name="PyInit_py_cfml_sxtal_geom") result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_sxtal_geom

        ! Local variables
        type(c_ptr) :: m

        m = Init()

    end function PyInit_py_cfml_sxtal_geom

    function Init() result(m)

        ! Local variables
        type(c_ptr) :: m
        integer :: ierror

        ierror = Forpy_Initialize()

        ! Build method table
        call table_sxtal_geom%init(4)
        call table_sxtal_geom%add_method("ganu_from_xz","py_ganu_from_xyz",METH_VARARGS,c_funloc(py_ganu_from_xyz))
        call table_sxtal_geom%add_method("ubfrqcel","py_ubfrqcel",METH_VARARGS,c_funloc(py_ubfrqcel))
        call table_sxtal_geom%add_method("z1frmd","py_z1frmd",METH_VARARGS,c_funloc(py_z1frmd))
        call table_sxtal_geom%add_method("z1frnb","py_z1frnb",METH_VARARGS,c_funloc(py_z1frnb))

        ! Build mod_sxtal_geom
        m = mod_sxtal_geom%init("py_cfml_sxtal_geom","A Python API for CrysFML08",table_sxtal_geom)

    end function Init

    function py_ganu_from_xyz(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        real          :: px                  !! x coordinate, in pixels
        real          :: pz                  !! z coordinate, in pixels
        real          :: ga_D                !! gamma angle of the center of the detector, in degrees
        real          :: nu_D                !! nu    angle of the center of the detector, in degrees
        integer       :: ipsd                !! detector type
        type(ndarray) :: nd_npix             !! number of horizontal and vertical pixels
        type(ndarray) :: nd_pisi             !! horizontal and vertical pixel sizes
        real          :: dist_samp_detector  !! sample detector distance
        type(ndarray) :: nd_det_offsets      !! x, y and z detector offsets
        integer       :: origin              !! origin for numbering pixels

        ! Local variables
        integer, parameter :: NMANDATORY = 10
        integer :: ierror,narg
        integer, dimension(2) :: npix
        integer, dimension(:), pointer :: p_npix
        real :: ga_P,nu_P
        real, dimension(2) :: pisi
        real, dimension(3) :: det_offsets
        real, dimension(:), pointer :: p_pisi,p_det_offsets
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Check the number of items
        call check_number_of_arguments('py_ganu_from_xyz',args,NMANDATORY,narg,ierror)

        ! Get arguments
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_ganu_from_xyz','px',item,px,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_ganu_from_xyz','pz',item,pz,ierror)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) call get_var_from_item('py_ganu_from_xyz','ga_D',item,ga_D,ierror)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) call get_var_from_item('py_ganu_from_xyz','nu_D',item,nu_D,ierror)
        if (ierror == 0) ierror = args%getitem(item,4)
        if (ierror == 0) call get_var_from_item('py_ganu_from_xyz','ipsd',item,ipsd,ierror)
        if (ierror == 0) ierror = args%getitem(item,5)
        if (ierror == 0) call get_var_from_item('py_ganu_from_xyz','npix',item,nd_npix,ierror)
        if (ierror == 0) call ndarray_to_pointer('py_ganu_from_xyz','npix',nd_npix,p_npix,ierror)
        if (ierror == 0) call pointer_to_array('py_ganu_from_xyz','npix',p_npix,npix,ierror)
        if (ierror == 0) ierror = args%getitem(item,6)
        if (ierror == 0) call get_var_from_item('py_ganu_from_xyz','pisi',item,nd_pisi,ierror)
        if (ierror == 0) call ndarray_to_pointer('py_ganu_from_xyz','pisi',nd_pisi,p_pisi,ierror)
        if (ierror == 0) call pointer_to_array('py_ganu_from_xyz','pisi',p_pisi,pisi,ierror)
        if (ierror == 0) ierror = args%getitem(item,7)
        if (ierror == 0) call get_var_from_item('py_ganu_from_xyz','dist_samp_detector',item,dist_samp_detector,ierror)
        if (ierror == 0) ierror = args%getitem(item,8)
        if (ierror == 0) call get_var_from_item('py_ganu_from_xyz','det_offsets',item,nd_det_offsets,ierror)
        if (ierror == 0) call ndarray_to_pointer('py_ganu_from_xyz','det_offsets',nd_det_offsets,p_det_offsets,ierror)
        if (ierror == 0) call pointer_to_array('py_ganu_from_xyz','det_offsets',p_det_offsets,det_offsets,ierror)
        if (ierror == 0) ierror = args%getitem(item,9)
        if (ierror == 0) call get_var_from_item('py_ganu_from_xyz','origin',item,origin,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_ganu_from_xyz: error parsing arguments'
        end if

        ! Call Fortran procedure
        if (ierror == 0) call ganu_from_xz(px,pz,ga_D,nu_D,ipsd,p_npix,p_pisi,dist_samp_detector,p_det_offsets,origin,ga_P,nu_P)
        if (ierror == 0) ierror = err_cfml%ierr

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,4)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,ga_P)
        ierror = ret%setitem(3,nu_P)
        resul = ret%get_c_ptr()

    end function py_ganu_from_xyz

    function py_ubfrqcel(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        character(len=:), allocatable :: spg_id      !! space group (number || symbol)
        type(ndarray) :: nd_q                        !! scattering vectors, dim = [nq,3;np.float32)]
        type(ndarray) :: nd_cell                     !! cell parameters (a,b,c,alpha,beta,gamma) dim = [6;np.float32]
        real          :: rtol                        !! tolerance in reciprocal space
        integer       :: nrefs_max                   !! maximum number of reflections used for finding pairs
        integer       :: npairs_max                  !! maximum number of pairs to be tested
        real          :: angle_min                   !! minimum angle between reflections for testing
        real          :: rfac_max                    !! maximum allowed value for R-factor
        type(dict)    :: di_kwargs

        ! Local variables
        integer, parameter :: NMANDATORY = 8
        integer :: ierror,narg
        real, dimension(6) :: cell
        real, dimension(:), pointer :: p_cell
        real, dimension(:), allocatable :: rfac
        real, dimension(:,:), pointer :: p_q
        real, dimension(:,:,:), allocatable :: ub
        character(len=1) :: order
        character(len=:), allocatable :: filename    !! full path of the output file
        logical :: is_filename
        type(ndarray) :: nd_ub                       !! ub-matrices, dim = [nub,3,3;np.float32)]
        type(ndarray) :: nd_rfac                     !! r-factors dim = [nub;np.float32]
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        is_filename = .false.
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Check the number of items
        call check_number_of_arguments('py_ubfrqcel',args,NMANDATORY,narg,ierror)

        ! Get arguments
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_ubfrqcel','spg_id',item,spg_id,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_ubfrqcel','q',item,nd_q,ierror)
        if (ierror == 0) call ndarray_to_pointer('py_ubfrqcel','q',nd_q,p_q,ierror,order)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) call get_var_from_item('py_ubfrqcel','cell',item,nd_cell,ierror)
        if (ierror == 0) call ndarray_to_pointer('py_ubfrqcel','cell',nd_cell,p_cell,ierror)
        if (ierror == 0) call pointer_to_array('py_ubfrqcel','cell',p_cell,cell,ierror)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) call get_var_from_item('py_ubfrqcel','nrefs_max',item,nrefs_max,ierror)
        if (ierror == 0) ierror = args%getitem(item,4)
        if (ierror == 0) call get_var_from_item('py_ubfrqcel','npairs_max',item,npairs_max,ierror)
        if (ierror == 0) ierror = args%getitem(item,5)
        if (ierror == 0) call get_var_from_item('py_ubfrqcel','angle_min',item,angle_min,ierror)
        if (ierror == 0) ierror = args%getitem(item,6)
        if (ierror == 0) call get_var_from_item('py_ubfrqcel','rtol',item,rtol,ierror)
        if (ierror == 0) ierror = args%getitem(item,7)
        if (ierror == 0) call get_var_from_item('py_ubfrqcel','rfac_max',item,rfac_max,ierror)
        if (ierror == 0 .and. narg > NMANDATORY) then
            if (ierror == 0) ierror = args%getitem(item,8)
            if (ierror == 0) call get_var_from_item('py_ubfrqcel','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) then
                call unwrap_dict_item_string_alloc('py_ubfrqcel','filename',di_kwargs,filename,ierror)
                if (ierror /= 0) then
                    call err_clear()
                    call clear_error()
                    ierror = 0
                else
                    is_filename = .true.
                end if
            end if
        end if
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_ubfrqcel: error parsing arguments'
        end if

        ! Call Fortran procedure
        if (ierror == 0) then
            if (is_filename) then
                call ubfrqcel(spg_id,p_q,cell,ub,rfac,nq_max_user=nrefs_max,npairs_max_user=npairs_max,angle_min_user=angle_min,rtol_user=rtol,rfac_max_user=rfac_max,output_file=filename)
            else
                call ubfrqcel(spg_id,p_q,cell,ub,rfac,nq_max_user=nrefs_max,npairs_max_user=npairs_max,angle_min_user=angle_min,rtol_user=rtol,rfac_max_user=rfac_max)
            end if
        end if
        if (ierror == 0) ierror = err_cfml%ierr
        if (ierror == 0) then
            ierror = ndarray_create(nd_ub,ub)
            ierror = ndarray_create(nd_rfac,rfac)
        else
            ierror = ndarray_create_zeros(nd_ub,1)
            ierror = ndarray_create_zeros(nd_rfac,1)
        end if

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,4)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,nd_ub)
        ierror = ret%setitem(3,nd_rfac)
        resul = ret%get_c_ptr()

    end function py_ubfrqcel

    function py_z1frmd(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        real :: wave   !! wavelength
        real :: ch     !! chi angle (degrees)
        real :: ph     !! phi angle (degrees)
        real :: ga     !! gamma angle (degrees)
        real :: om     !! omega angle (degrees)
        real :: nu     !! nu angle (degrees)

        ! Local variables
        integer, parameter :: NMANDATORY = 6
        integer :: ierror,narg
        real, dimension(3) :: z1 ! scattering vector
        type(ndarray) :: nd_z1
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Check the number of items
        call check_number_of_arguments('py_z1frmd',args,NMANDATORY,narg,ierror)

        ! Get arguments
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_z1frmd','wave',item,wave,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_z1frmd','ch',item,ch,ierror)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) call get_var_from_item('py_z1frmd','ph',item,ph,ierror)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) call get_var_from_item('py_z1frmd','ga',item,ga,ierror)
        if (ierror == 0) ierror = args%getitem(item,4)
        if (ierror == 0) call get_var_from_item('py_z1frmd','om',item,om,ierror)
        if (ierror == 0) ierror = args%getitem(item,5)
        if (ierror == 0) call get_var_from_item('py_z1frmd','nu',item,nu,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_z1frmd: error parsing arguments'
        end if

        ! Call Fortran procedure
        if (ierror == 0) z1 = z1frmd(wave,ch,ph,ga,om,nu)
        if (ierror == 0) ierror = err_cfml%ierr
        if (ierror == 0) then
            ierror = ndarray_create(nd_z1,z1)
        else
            ierror = ndarray_create_zeros(nd_z1,3)
        end if

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,nd_z1)
        resul = ret%get_c_ptr()

    end function py_z1frmd

    function py_z1frnb(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        real :: wave   !! wavelength
        real :: ga     !! gamma angle (degrees)
        real :: om     !! omega angle (degrees)
        real :: nu     !! nu angle (degrees)

        ! Local variables
        integer, parameter :: NMANDATORY = 4
        integer :: ierror,narg
        real, dimension(3) :: z1 ! scattering vector
        type(ndarray) :: nd_z1
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Check the number of items
        call check_number_of_arguments('py_z1frnb',args,NMANDATORY,narg,ierror)

        ! Get arguments
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_z1frnb','wave',item,wave,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_z1frnb','ga',item,ga,ierror)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) call get_var_from_item('py_z1frnb','om',item,om,ierror)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) call get_var_from_item('py_z1frnb','nu',item,nu,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_z1frnb: error parsing arguments'
        end if

        ! Call Fortran procedure
        if (ierror == 0) z1 = z1frnb(wave,ga,om,nu)
        if (ierror == 0) ierror = err_cfml%ierr
        if (ierror == 0) then
            ierror = ndarray_create(nd_z1,z1)
        else
            ierror = ndarray_create_zeros(nd_z1,3)
        end if

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,nd_z1)
        resul = ret%get_c_ptr()

    end function py_z1frnb

end module py_cfml_sxtal_geom
