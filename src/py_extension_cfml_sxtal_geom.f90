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

module extension_cfml_sxtal_geom

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

    use CFML_GlobalDeps, only: Clear_Error,Err_CFML,To_Rad
    use CFML_ILL_Instrm_Data, only: diffractometer_type
    use CFML_Sxtal_Geom, only: psd_convert
    use extension_cfml_messages, only: check_error

    implicit none

    contains

    function py_ganu_from_xz(self_ptr,args_ptr) result(resul) bind(c)

        !< Compute two theta angle from detector coordinates
        !
        !   Arguments in args_ptr
        !   --------           -----------         -----------
        !   Variable           Python type         Description
        !   --------           -----------         -----------
        !   px                 float               x coordinate, in pixels
        !   pz                 float               z coordinate, in pixels
        !   ga_D               float               gamma angle of the center of the detector, in degrees
        !   nu_D               float               nu    angle of the center of the detector, in degrees
        !   ipsd               integer             detector type
        !                                          2: flat detector
        !                                          3: horizontal banana
        !   nd_npix            ndarray(2,int32)    number of horizontal and vertical pixels
        !   nd_pisi            ndarray(2,float32)  horizontal and vertical pixel sizes
        !   dist_samp_detector float               sample detector distance
        !   nd_det_offsets     ndarray(3,float32)  x, y and z detector offsets
        !   origin             integer             origin for numbering pixels
        !                                          0: top    left
        !                                          1: top    right
        !                                          2: bottom right
        !                                          3: bottom left
        !   blfr               integer             Busing-Levy frame
        !                                          0: z-up
        !                                          1: z-down
        !>

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Local variables
        integer :: ierror
        integer :: ipsd,origin,blfr
        integer, dimension(:), pointer :: p_npix
        real :: px,pz,ga_D,nu_D,x_D,z_D,dist_samp_detector,ga_P,nu_P
        real, dimension(:), pointer :: p_pisi,p_det_offsets
        type(diffractometer_type) :: diffractometer
        type(ndarray) :: nd_npix,nd_pisi,nd_det_offsets
        type(object) :: item
        type(tuple) :: args,ret

        call Clear_Error()
        ierror = 0

        ! Get arguments
        call unsafe_cast_from_c_ptr(args,args_ptr)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) ierror = cast(px,item)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(pz,item)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) ierror = cast(ga_D,item)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) ierror = cast(nu_D,item)
        if (ierror == 0) ierror = args%getitem(item,4)
        if (ierror == 0) ierror = cast(ipsd,item)
        if (ierror == 0) ierror = args%getitem(item,5)
        if (ierror == 0) ierror = cast(nd_npix,item)
        if (ierror == 0) ierror = nd_npix%get_data(p_npix)
        if (ierror == 0) ierror = args%getitem(item,6)
        if (ierror == 0) ierror = cast(nd_pisi,item)
        if (ierror == 0) ierror = nd_pisi%get_data(p_pisi)
        if (ierror == 0) ierror = args%getitem(item,7)
        if (ierror == 0) ierror = cast(dist_samp_detector,item)
        if (ierror == 0) ierror = args%getitem(item,8)
        if (ierror == 0) ierror = cast(nd_det_offsets,item)
        if (ierror == 0) ierror = nd_det_offsets%get_data(p_det_offsets)
        if (ierror == 0) ierror = args%getitem(item,9)
        if (ierror == 0) ierror = cast(origin,item)
        if (ierror == 0) ierror = args%getitem(item,10)
        if (ierror == 0) ierror = cast(blfr,item)
        if (ierror /= 0) then
            ierror = EXCEPTION_ERROR
            call raise_exception(RuntimeError,'py_ganu_from_xz: Error getting arguments')
        end if

        ! Build the diffractometer object
        if (ierror == 0) then
            diffractometer%ipsd = ipsd
            diffractometer%np_horiz = p_npix(1)
            diffractometer%np_vert = p_npix(2)
            diffractometer%cgap = p_pisi(1)
            diffractometer%agap = p_pisi(2)
            diffractometer%dist_samp_detector = dist_samp_detector
            diffractometer%det_offsets(:) = p_det_offsets(:)
            if (blfr == 1) then
                diffractometer%bl_frame = 'z-down'
            else
                diffractometer%bl_frame = 'z-up'
            end if
        end if

        ! Compute ga_P and nu_P
        if (ierror == 0) call psd_convert(diffractometer,1,0,ga_D,nu_D,px,pz,x_D,z_D,ga_P,nu_P,origin=origin)
        call check_error('py_ganu_from_xz',ierror)

        ! Return tuple
        if (ierror == 0) then
            ierror = tuple_create(ret,3)
            ierror = ret%setitem(0,0)
            if (ierror == 0) ierror = ret%setitem(1,ga_P)
            if (ierror == 0) ierror = ret%setitem(2,nu_P)
        else
            ierror = tuple_create(ret,1)
            ierror = ret%setitem(0,-1)
        end if
        resul = ret%get_c_ptr()

    end function py_ganu_from_xz

end module extension_cfml_sxtal_geom