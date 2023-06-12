!-------------------------------------------------------------
! cfml_sxtal_geom
! -------------------------------------------------------------
! This file is part of cfml_sxtal_geom
!
! The cfml_sxtal_geom is distributed under LGPL. In agreement with the
! Intergovernmental Convention of the ILL, this software cannot be used
! in military applications.
!
! cfml_sxtal_geom is based on Elias Rabel work for Forpy, see <https://github.com/ylikx/forpy>.
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

    use cfml_globaldeps
    use cfml_messages
    use cfml_sxtal_geom

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
        call table_sxtal_geom%add_method("ganu_from_xz","gamma and nu values from x,z pixel coordinates",METH_VARARGS,c_funloc(py_ganu_from_xz))
        call table_sxtal_geom%add_method("ubfrqcel","ub-matrix from a q-set and cell parameters",METH_VARARGS,c_funloc(py_ubfrqcel))
        call table_sxtal_geom%add_method("z1frmd","scattering vector for 4C geometry",METH_VARARGS,c_funloc(py_z1frmd))
        call table_sxtal_geom%add_method("z1frnb","scattering vector for NB geometry",METH_VARARGS,c_funloc(py_z1frnb))

        ! Build mod_sxtal_geom
        m = mod_sxtal_geom%init("py_cfml_sxtal_geom","A Python API for CrysFML08",table_sxtal_geom)

    end function Init

    function py_ganu_from_xz(self_ptr,args_ptr) result(resul) bind(c)
        !! author: ILL Scientific Computing Group
        !! date: 23/03/2023
        !! display: public
        !! proc_internals: true
        !! summary: Compute the gamma and nu values from x,z coordinates on a 2D detector
        !
        !! Compute the gamma and nu values from x,z coordinates on a 2D detector
        !!
        !! ARGS_PTR = (px,pz,ga_D,nu_D,ipsd,npix,pisi,dist_samp_detector,det_offsets,origin)
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
        !   npix               ndarray(2,int32)    number of horizontal and vertical pixels
        !   pisi               ndarray(2,float32)  horizontal and vertical pixel sizes
        !   dist_samp_detector float               sample detector distance
        !   det_offsets        ndarray(3,float32)  x, y and z detector offsets
        !   origin             integer             origin for numbering pixels
        !                                          0: top    left
        !                                          1: top    right
        !                                          2: bottom right
        !                                          3: bottom left
        !! RESUL = (ierr,ga_P,nu_P)
        !   --------           -----------         -----------
        !   Variable           Python type         Description
        !   --------           -----------         -----------
        !   ierr               integer             if ierr /= 0, an error occurred
        !   err_cfml%msg       string              error message
        !   ga_P               float               gamma value in degrees
        !   nu_P               float               nu    value in degrees

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

        ! Variables in resul
        integer :: ierr                      !! if ierr /= 0, an error ocurred
        real    :: ga_P                      !! gamma value in degrees
        real    :: nu_P                      !! nu    value in degrees

        ! Local variables
        integer :: ierror
        integer, dimension(:), pointer :: p_npix
        real :: x_D,z_D
        real, dimension(:), pointer :: p_pisi,p_det_offsets

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

        ! Compute ga_P and nu_P
        if (ierror == 0) call ganu_from_xz(px,pz,ga_D,nu_D,ipsd,p_npix,p_pisi,dist_samp_detector,p_det_offsets,origin,ga_P,nu_P)
        if (ierror == 0) ierror = err_cfml%ierr

        ! Return tuple
        if (ierror == 0) then
            ierror = tuple_create(ret,4)
            ierror = ret%setitem(0,0)
            ierror = ret%setitem(1,trim(err_cfml%msg))
            ierror = ret%setitem(2,ga_P)
            ierror = ret%setitem(3,nu_P)
        else
            ierr = ierror
            ierror = tuple_create(ret,2)
            ierror = ret%setitem(0,ierr)
            ierror = ret%setitem(1,trim(err_cfml%msg))
        end if
        resul = ret%get_c_ptr()

    end function py_ganu_from_xz

    function py_ubfrqcel(self_ptr,args_ptr) result(resul) bind(c)
        !! author: ILL Scientific Computing Group
        !! date: 23/03/2023
        !! display: public
        !! proc_internals: true
        !! summary: Search an UB-matrix from a set of scattering vectors and fixed cell
        !
        !!  Generate a list of reflections for a given space group.
        !!
        !!  ARGS_PTR = (nd_q,nd_cell,npairs_max,angle_min,rtol,rfac_max)
        !   --------           -----------           -----------
        !   Variable           Python type           Description
        !   --------           -----------           -----------
        !   spg_id             string                space group (number || symbol)
        !   nd_q               ndarray(nq,3;float32) set of scattering vectors
        !   nd_cell            ndarray(6;float32)    cell parameters (a,b,c,alpha,beta,gamma)
        !   nrefs_max          integer               maximum number of reflections used for finding pairs
        !   npairs_max         integer               maximum number of pairs to be tested
        !   angle_min          float                 minimum angle between reflections for testing
        !   rtol               float                 tolerance in reciprocal space
        !   rfac_max           float                 maximum allowed rfac
        !   output_file        str                   Full path of the output file
        !
        !!  RESUL = (ierr,msg,nd_ub,nd_rfac)
        !   --------           -----------         -----------
        !   Variable           Python type         Description
        !   --------           -----------         -----------
        !   ierr               integer             if ierr /= 0, an error occurred
        !   err_cfml%msg       string              error message
        !   nd_ub              ndarray(nub,3,3)    ub-matrices
        !                      np.float32
        !                      order = F
        !   nd_rfac            ndarray(nub)        r-factors
        !                      np.float32

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
        type(ndarray) :: nd_ub                       !! ub-matrices, dim = [nub,3,3;np.float32)]
        type(ndarray) :: nd_rfac                     !! r-factors dim = [nub6;np.float32]
        character(len=:), allocatable :: output_file !! full path of the output file

        ! Variables in resul
        integer       :: ierr       !! if ierr /= 0, an error ocurred

        ! Local variables
        integer :: ierror,i ! Flag error
        real, dimension(:), pointer :: p_cell
        real, dimension(:,:), pointer :: p_q
        real, dimension(:,:,:), allocatable :: ub   ! ub-matrices
        real, dimension(:),     allocatable :: rfac ! rfactors for ub-matrices
        type(object) :: item
        type(tuple) :: args,ret

        ! Reset error variable
        ierr   = 0
        ierror = 0
        call clear_error()

        ! In case of exception return C_NULL_PTR
        resul = C_NULL_PTR

        ! Get arguments
        call unsafe_cast_from_c_ptr(args,args_ptr)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) ierror = cast(spg_id,item)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(nd_q,item)
        if (ierror == 0) ierror = nd_q%get_data(p_q,order='C')
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) ierror = cast(nd_cell,item)
        if (ierror == 0) ierror = nd_cell%get_data(p_cell)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) ierror = cast(nrefs_max,item)
        if (ierror == 0) ierror = args%getitem(item,4)
        if (ierror == 0) ierror = cast(npairs_max,item)
        if (ierror == 0) ierror = args%getitem(item,5)
        if (ierror == 0) ierror = cast(angle_min,item)
        if (ierror == 0) ierror = args%getitem(item,6)
        if (ierror == 0) ierror = cast(rtol,item)
        if (ierror == 0) ierror = args%getitem(item,7)
        if (ierror == 0) ierror = cast(rfac_max,item)
        if (ierror == 0) ierror = args%getitem(item,8)
        if (ierror == 0) ierror = cast(output_file,item)
        if (ierror /= 0) then
            err_cfml%ierr = -1
            err_cfml%msg = 'py_ubfrqcel: Error getting arguments'
        end if

        ! Compute ga_P and nu_P
        if (ierror == 0) call ubfrqcel(spg_id,p_q,p_cell,ub,rfac,nq_max_user=nrefs_max,npairs_max_user=npairs_max,angle_min_user=angle_min,rtol_user=rtol,rfac_max_user=rfac_max,output_file=output_file)
        if (ierror == 0) ierror = err_cfml%ierr

        ! Return tuple
        if (ierror == 0) then
            ierror = tuple_create(ret,4)
            ierror = ret%setitem(0,0)
            ierror = ret%setitem(1,trim(err_cfml%msg))
            ierror = ndarray_create(nd_ub,ub)
            ierror = ret%setitem(2,nd_ub)
            ierror = ndarray_create(nd_rfac,rfac)
            ierror = ret%setitem(3,nd_rfac)
        else
            ierr = ierror
            ierror = tuple_create(ret,2)
            ierror = ret%setitem(0,ierr)
            ierror = ret%setitem(1,trim(err_cfml%msg))
        end if
        resul = ret%get_c_ptr()

    end function py_ubfrqcel

    function py_z1frmd(self_ptr,args_ptr) result(resul) bind(c)
        !! author: ILL Scientific Computing Group
        !! date: 24/03/2023
        !! display: public
        !! proc_internals: true
        !! summary: Wrapper for function z1frmd.
        !! summary: Compute the scattering vector from angles and wavelength for 4-circle geometry.
        !!
        !! ARGS_PTR = (wave,ch,ph,ga,om,nu)
        !!
        !! RESUL = (ierr,err_cfml%msg,nd_z1)
        !  --------           -----------         -----------
        !  Variable           Python type         Description
        !  --------           -----------         -----------
        !  wave               float               wavelength
        !  ch                 float               chi (degrees)
        !  ph                 float               phi (degrees)
        !  ga                 float               gamma (degrees)
        !  om                 float               omega (degrees)
        !  nu                 float               nu (degrees)
        !  ierr               integer             if ierr /= 0, an error occurred
        !  err_cfml%msg       string              error message
        !  nd_z1              ndarray(3;float32)  scattering vector

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Python / Fortran interface variables
        real(kind=cp) :: wave   !! wavelength
        real(kind=cp) :: ch     !! chi angle (degrees)
        real(kind=cp) :: ph     !! phi angle (degrees)
        real(kind=cp) :: ga     !! gamma angle (degrees)
        real(kind=cp) :: om     !! omega angle (degrees)
        real(kind=cp) :: nu     !! nu angle (degrees)
        integer       :: ierr   !! error flag
        type(ndarray) :: nd_z1  !! scattering vector

        ! Local variables
        integer :: ierror
        real(kind=cp), dimension(3) :: z1 ! scattering vector
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
        if (ierror == 0) ierror = cast(wave,item)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(ch,item)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) ierror = cast(ph,item)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) ierror = cast(ga,item)
        if (ierror == 0) ierror = args%getitem(item,4)
        if (ierror == 0) ierror = cast(om,item)
        if (ierror == 0) ierror = args%getitem(item,5)
        if (ierror == 0) ierror = cast(nu,item)
        if (ierror /= 0) then
            err_cfml%ierr = -1
            err_cfml%msg = 'py_z1frmd: Error getting arguments'
        end if

        ! Call CrysFML procedure
        if (ierror == 0) z1 = z1frmd(wave,ch,ph,ga,om,nu)
        if (ierror == 0) ierror = err_cfml%ierr

        ! Return tuple
        if (ierror == 0) then
            ierror = tuple_create(ret,3)
            ierror = ret%setitem(0,ierr)
            ierror = ret%setitem(1,trim(err_cfml%msg))
            ierror = ndarray_create(nd_z1,z1)
            ierror = ret%setitem(2,nd_z1)
        else
            ierr   = ierror
            ierror = tuple_create(ret,2)
            ierror = ret%setitem(0,ierr)
            ierror = ret%setitem(1,trim(err_cfml%msg))
        end if
        resul = ret%get_c_ptr()

    end function py_z1frmd

    function py_z1frnb(self_ptr,args_ptr) result(resul) bind(c)
        !! author: ILL Scientific Computing Group
        !! date: 24/03/2023
        !! display: public
        !! proc_internals: true
        !! summary: Wrapper for function z1frnb.
        !! summary: Compute the scattering vector from angles and wavelength for normal-beam geometry
        !!
        !! ARGS_PTR = (wave,ga,om,nu)
        !!
        !! RESUL = (ierr,err_cfml%msg,nd_z1)
        !  --------           -----------         -----------
        !  Variable           Python type         Description
        !  --------           -----------         -----------
        !  wave               float               wavelength
        !  ga                 float               gamma (degrees)
        !  om                 float               omega (degrees)
        !  nu                 float               nu (degrees)
        !  ierr               integer             if ierr /= 0, an error occurred
        !  err_cfml%msg       string              error message
        !  nd_z1              ndarray(3;float32)  scattering vector

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Python / Fortran interface variables
        real          :: wave  !! wavelength
        real          :: ga    !! gamma angle (degrees)
        real          :: om    !! omega angle (degress)
        real          :: nu    !! nu angle (degrees)
        integer       :: ierr  !! error flag
        type(ndarray) :: nd_z1 !! Scattering vector

        ! Other local variables
        integer :: ierror
        real(kind=cp), dimension(3) :: z1 ! scattering vector
        type(object) :: item
        type(tuple) :: args,ret

        ! Reset error variable
        ierr = 0
        ierror = 0
        call clear_error()

        ! In case of exception return C_NULL_PTR
        resul = C_NULL_PTR

        ! Unwrap_arguments
        call unsafe_cast_from_c_ptr(args,args_ptr)
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) ierror = cast(wave,item)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) ierror = cast(ga,item)
        if (ierror == 0) ierror = args%getitem(item,2)
        if (ierror == 0) ierror = cast(om,item)
        if (ierror == 0) ierror = args%getitem(item,3)
        if (ierror == 0) ierror = cast(nu,item)
        if (ierror /= 0) then
            err_cfml%ierr = -1
            err_cfml%msg = 'py_z1frnb: Error getting arguments'
        end if

        ! Call CrysFML procedure
        if (ierror == 0) z1 = z1frnb(wave,ga,om,nu)
        if (ierror == 0) ierror = err_cfml%ierr

        ! Return tuple
        if (ierror == 0) then
            ierror = tuple_create(ret,3)
            ierror = ret%setitem(0,ierr)
            ierror = ret%setitem(1,trim(err_cfml%msg))
            ierror = ndarray_create(nd_z1,z1)
            ierror = ret%setitem(2,nd_z1)
        else
            ierr   = ierror
            ierror = tuple_create(ret,2)
            ierror = ret%setitem(0,ierr)
            ierror = ret%setitem(1,trim(err_cfml%msg))
        end if
        resul = ret%get_c_ptr()

    end function py_z1frnb

end module py_cfml_sxtal_geom
