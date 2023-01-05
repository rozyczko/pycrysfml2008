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
    ! psd_convert_new(diffractometer,f_virtual,conversion_type,ga_D,nu_D,px,pz,x_D,z_D,ga_P,nu_P,ierr)
    !>

    use forpy_mod
    use iso_c_binding

    use CFML_GlobalDeps, only: Clear_Error,Err_CFML,To_Rad
    use CFML_ILL_Instrm_Data, only: diffractometer_type
    use extension_cfml_messages, only: check_error

    implicit none

    contains

    function py_ganu_from_xz(self_ptr,args_ptr) result(resul) bind(c)

        !> Compute two theta angle from detector coordinates

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Local variables
        integer :: ierror,ierr
        integer :: ipsd,np_horiz,np_vert,f_virtual
        integer, dimension(:), pointer :: p_npix
        real :: px,pz,ga_D,nu_D,x_D,z_D,cgap,agap,dist_samp_detector,ga_P,nu_P
        real, dimension(3) :: det_offsets
        real, dimension(:), pointer :: p_pisi,p_det_offsets
        character(len=:), allocatable :: data_ordering
        type(diffractometer_type) :: diffractometer
        type(ndarray) :: nd_npix,nd_pisi,nd_det_offsets
        type(object) :: item
        type(tuple) :: args,ret
        type(nonetype) :: nret

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
        if (ierror == 0) ierror = cast(data_ordering,item)
        if (ierror == 0) ierror = args%getitem(item,10)
        if (ierror == 0) ierror = cast(f_virtual,item)
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
            diffractometer%data_ordering = data_ordering
        end if

        ! Compute ga_P and nu_P
        if (ierror == 0) call psd_convert_new(diffractometer,f_virtual,0,ga_D,nu_D,px,pz,x_D,z_D,ga_P,nu_P,ierr)
        call check_error('py_ganu_from_xz',ierror)

        ! Return tuple
        if (ierror == 0) then
            ierror = tuple_create(ret,2)
            if (ierror == 0) ierror = ret%setitem(0,ga_P)
            if (ierror == 0) ierror = ret%setitem(1,nu_P)
        end if
        if (ierror == 0) then
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_ganu_from_xz

    subroutine psd_convert_new(diffractometer,f_virtual,conversion_type,ga_D,nu_D,px,pz,x_D,z_D,ga_P,nu_P,ierr)

        ! Pixel numbering start by zero
        !
        ! r_D contains the pixel coordinates in the detector
        ! reference system
        !
        ! r_L contains the pixel coordinates in the laboratory
        ! reference system, when the detector is at gamma = 0
        ! and nu = 0
        !
        ! conversion_type = 0: pixels to angles
        ! conversion_type = 1: angles to pixels

        ! Arguments
        type(diffractometer_type), intent(inout) :: diffractometer
        integer,                   intent(in)    :: f_virtual
        integer,                   intent(in)    :: conversion_type
        real,                      intent(in)    :: ga_D
        real,                      intent(in)    :: nu_D
        real,                      intent(inout) :: px ! pixel x
        real,                      intent(inout) :: pz ! pixel z
        real,                      intent(out)   :: x_D
        real,                      intent(out)   :: z_D
        real,                      intent(inout) :: ga_P
        real,                      intent(inout) :: nu_P
        integer,                   intent(out)   :: ierr

        ! Local variables
        integer :: i,j
        real :: px_mid,pz_mid,radius,y_D,x_L,y_L,z_L,px_,pz_

        ierr = 0
        diffractometer%np_horiz = diffractometer%np_horiz * f_virtual
        diffractometer%cgap = diffractometer%cgap / f_virtual
        px_mid = diffractometer%np_horiz / 2.0
        pz_mid = diffractometer%np_vert / 2.0
        px_ = px
        pz_ = pz

        if (conversion_type == 0) then ! pixels to angles

            ! Refer pixels to the origin at the bottom left
            i = index(diffractometer%data_ordering,'top')
            j = index(diffractometer%data_ordering,'right')
            if (i > 0) pz_ = diffractometer%np_vert - pz_ - 1
            if (j > 0) px_ = diffractometer%np_horiz - px_ - 1

            ! Detector reference system: origin at the center of the detector
            x_D = (px_ - px_mid) * diffractometer%cgap
            y_D = 0.0
            z_D = (pz_ - pz_mid) * diffractometer%agap

            ! Cartesian coordinates in the laboratory system
            select case(diffractometer%ipsd)
                case(2) ! Flat detector
                    x_L = x_D + diffractometer%det_offsets(1)
                    y_L = y_D + diffractometer%det_offsets(2) + diffractometer%dist_samp_detector
                    z_L = z_D + diffractometer%det_offsets(3)
                case(3) ! Horizontal banana
                    x_L = diffractometer%dist_samp_detector * sin(x_D/diffractometer%dist_samp_detector) + &
                        diffractometer%det_offsets(1)
                    y_L = diffractometer%dist_samp_detector * cos(x_D/diffractometer%dist_samp_detector) + &
                        diffractometer%det_offsets(2)
                    z_L = z_D + diffractometer%det_offsets(3)
                case default ! Unknown detector
                    ierr = -1
                    return
            end select

            ga_P = ga_D + atan2d(x_L,y_L)
            nu_P = nu_D + atan2d(z_L,sqrt(x_L**2 + y_L**2))

        else ! angles to pixels

            ga_P = ga_P - ga_D
            nu_P = nu_P - nu_D
            radius = diffractometer%dist_samp_detector + diffractometer%det_offsets(2)

            select case(diffractometer%ipsd)
                case(2) ! Flat detector
                    x_D = radius * tand(ga_P) - diffractometer%det_offsets(1)
                    z_D = radius * tand(nu_P) / cosd(ga_P) - diffractometer%det_offsets(3)
                case(3) ! Horizontal banana
                    x_D = radius * ga_P * to_rad - diffractometer%det_offsets(1)
                    z_D = radius * tand(nu_P) - diffractometer%det_offsets(3)
                case default ! Unknown detector
                    ierr = -1
                    return
            end select

            ! Restore original values of ga_P and nu_P
            ga_P = ga_P + ga_D
            nu_P = nu_P + nu_D

            ! Compute pixels from detector coordinates
            px = px_mid + x_D / diffractometer%cgap
            pz = pz_mid + z_D / diffractometer%agap

            ! Refer pixels to the origin according to data_ordering
            i = index(diffractometer%data_ordering,'top')
            j = index(diffractometer%data_ordering,'right')
            if (i > 0) pz = diffractometer%np_vert - pz - 1
            if (j > 0) px = diffractometer%np_horiz - px - 1

        end if

        ! Restore original values
        diffractometer%np_horiz = diffractometer%np_horiz / f_virtual
        diffractometer%cgap = diffractometer%cgap * f_virtual

    end subroutine psd_convert_new

end module extension_cfml_sxtal_geom