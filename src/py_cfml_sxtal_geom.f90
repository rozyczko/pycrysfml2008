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

module py_cfml_sxtal_geom

    use forpy_mod
    use iso_c_binding

    use cfml_globaldeps
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

        type(c_ptr) :: m

        m = Init()

    end function PyInit_py_cfml_sxtal_geom

    function Init() result(m)

        ! Local variables
        type(c_ptr) :: m
        integer :: ierror

        ierror = Forpy_Initialize()

        ! Build method table
        call table_sxtal_geom%init(2)
        call table_sxtal_geom%add_method("z1frmd",&
            "z1frmd",METH_VARARGS,&
            c_funloc(py_z1frmd))
        call table_sxtal_geom%add_method("z1frnb",&
            "z1frnb",METH_VARARGS,&
            c_funloc(py_z1frnb))

        ! Build mod_sxtal_geom
        m = mod_sxtal_geom%init("py_cfml_sxtal_geom","Python wrapper of module CFML_Sxtal_Geom of CrysFML08",table_sxtal_geom)

    end function Init

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
        !! RESUL = (íerr,err_cfml%msg,nd_z1)
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

        ! Call CrysFML procedure
        if (ierror == 0) then
            z1 = z1frmd(wave,ch,ph,ga,om,nu)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"z1frmd: "//trim(err_cfml%msg))
            end if
        else
            ierror = EXCEPTION_ERROR
            call raise_exception(RuntimeError,"z1frmd: Error reading arguments")
        end if

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

        ! Call CrysFML procedure
        if (ierror == 0) then
            z1 = z1frnb(wave,ga,om,nu)
            if (err_cfml%flag) then
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,"z1frnb: "//trim(err_cfml%msg))
            end if
        else
            ierror = EXCEPTION_ERROR
            call raise_exception(RuntimeError,"z1frnb: Error reading arguments")
        end if

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

!    function py_ub_from_q_and_cell(self_ptr,args_ptr) result(resul) bind(c)
!        !! author: ILL Scientific Computing Group
!        !! date: 23/03/2023
!        !! display: public
!        !! proc_internals: true
!        !! summary: Search an UB-matrix from a set of scattering vectors and fixed cell
!        !
!        !!  Generate a list of reflections for a given space group.
!        !!
!        !!  ARGS_PTR = (nd_q,nd_cell,rtol,npairs_max,angle_min,rfac_max)
!        !   --------           -----------           -----------
!        !   Variable           Python type           Description
!        !   --------           -----------           -----------
!        !   nd_q               ndarray(nq,3;float32) set of scattering vectors
!        !   nd_cell            ndarray(6;float32)    cell parameters (a,b,c,alpha,beta,gamma)
!        !   rtol               float                 tolerance in reciprocal space
!        !   npairs_max         integer               maximum number of pairs to be tested
!        !   angle_min          float                 minimum angle between reflections for testing
!        !   rfac_max           float                 maximum allowed rfac
!        !
!        !
!        !!  RESUL = (ierr,msg)
!        !   --------           -----------         -----------
!        !   Variable           Python type         Description
!        !   --------           -----------         -----------
!        !   ierror             integer             if ierror /= 0, an error occurred
!        !   err_cfml%msg       string              error message
!
!        ! Arguments
!        type(c_ptr), value :: self_ptr
!        type(c_ptr), value :: args_ptr
!        type(c_ptr)        :: resul
!
!        ! Variables in args_ptr
!        type(ndarray) :: nd_q       !! scattering vectors, dim = [nq,3;np.float32)]
!        type(ndarray) :: nd_cell    !! cell parameters (a,b,c,alpha,beta,gamma) dim = [6;np.float32]
!        real          :: rtol       !! tolerance in reciprocal space
!        integer       :: npairs_max !! maximum number of pairs to be tested
!        real          :: angle_min  !! minimum angle between reflections for testing
!        real          :: rfac_max   !! maximum allowed value for R-factor
!
!        ! Variables in resul
!
!        ! Local variables
!        !integer, parameter :: NCELLS_MAX = 10  ! Maximum number of tested cells
!        !integer, parameter :: NQ_MAX = 50      ! Maximum number of scattering vectors
!        integer :: ierror
!        !integer :: i,j,n,m,ip                  ! Iteration index
!        !integer :: ierror                      ! Flag error
!        !integer :: nq,ncandidates,npairs,ncells
!        !integer, dimension(:,:), allocatable :: candidates,pairs
!        !real :: stlmin,stlmax                  ! sin(theta) / lambda limits
!        !real :: angle
!        !real, dimension(NCELLS_MAX) :: rfac
!        !real, dimension(3) :: h1,h2            ! Miller index
!        !real, dimension(3) :: q1,q2            ! Scattering vectors in cartesian coordinates
!        !real, dimension(:), allocatable :: s
!        real, dimension(:), pointer :: p_cell
!        real, dimension(:,:), pointer :: p_q
!        !real, dimension(3,3,NCELLS_MAX) :: ub_inv
!        !type(Cell_G_Type) :: cell_fixed
!        !type(SPG_Type) :: spg
!        !type(RefList_Type) :: hkls
!        type(object) :: item
!        type(tuple) :: args,ret
!
!        call Clear_Error()
!        ierror = 0
!
!        ! Get arguments
!        call unsafe_cast_from_c_ptr(args,args_ptr)
!        if (ierror == 0) ierror = args%getitem(item,0)
!        if (ierror == 0) ierror = cast(nd_q,item)
!        if (ierror == 0) ierror = nd_cell%get_data(p_q,order='C')
!        if (ierror == 0) ierror = args%getitem(item,1)
!        if (ierror == 0) ierror = cast(nd_cell,item)
!        if (ierror == 0) ierror = nd_cell%get_data(p_cell)
!        if (ierror == 0) ierror = args%getitem(item,2)
!        if (ierror == 0) ierror = cast(rtol,item)
!        if (ierror == 0) ierror = args%getitem(item,3)
!        if (ierror == 0) ierror = cast(npairs_max,item)
!        if (ierror == 0) ierror = args%getitem(item,4)
!        if (ierror == 0) ierror = cast(angle_min,item)
!        if (ierror == 0) ierror = args%getitem(item,5)
!        if (ierror == 0) ierror = cast(rfac_max,item)
!
!        ! Call CrysFML procedure
!        if (ierror == 0) then
!            call clear_error()
!            call ub_from_q_and_cell(p_q,p_cell,rtol,npairs_max,angle_min,rfac_max)
!            if (err_cfml%flag) then
!                ierror = EXCEPTION_ERROR
!                call raise_exception(RuntimeError,"py_ub_from_q_and_cell: "//trim(err_cfml%msg))
!            end if
!        else
!            ierror = EXCEPTION_ERROR
!            call raise_exception(RuntimeError,"py_ub_from_q_and_cell: Error reading arguments")
!        end if
!
!        ! Return tuple
!        if (ierror == 0) then
!            ierror = tuple_create(ret,2)
!            ierror = ret%setitem(0,0)
!            ierror = ret%setitem(1,trim(err_cfml%msg))
!        else
!            ierror = tuple_create(ret,2)
!            ierror = ret%setitem(0,-1)
!            ierror = ret%setitem(1,trim(err_cfml%msg))
!        end if
!        resul = ret%get_c_ptr()
!
!    end function py_ub_from_q_and_cell
!
!    subroutine ub_from_q_and_cell(q,cell,rtol,npairs_max,angle_min,rfac_max)
!
!        ! Arguments
!        real, dimension(:,:), intent(in) :: q
!        real, dimension(6),   intent(in) :: cell
!        real,                 intent(in) :: rtol
!        integer,              intent(in) :: npairs_max
!        real,                 intent(in) :: angle_min
!        real,                 intent(in) :: rfac_max
!
!
!
!
!
!!        if (ierror /= 0) then
!!            ierror = EXCEPTION_ERROR
!!            call raise_exception(RuntimeError,'py_ub_from_q_and_cell: Error getting arguments')
!!        end if
!!        if (ierror == 0) then
!!            nq = size(p_q,2)
!!            if (nq < 2) then
!!                ierror = EXCEPTION_ERROR
!!                call raise_exception(RuntimeError,'py_ub_from_q_and_cell: Number of scattering vectors !less than two')
!!            else if (nq > NQ_MAX) then
!!                nq = NQ_MAX
!!            end if
!!        end if
!!        if (ierror == 0) then
!!            ! Search for q-limits
!!            stlmin = 100000.0
!!            stlmax = 0.0
!!            allocate(s(nq))
!!            do i = 1 , nq
!!                s(i) = sqrt(p_q(1,i)**2 + p_q(2,i)**2  + p_q(3,i)**2) * 0.5
!!                if (s(i) > stlmax) stlmax = s(i)
!!                if (s(i) < stlmin) stlmin = s(i)
!!            end do
!!            stlmin = sqrt(stlmin)
!!            stlmax = sqrt(stlmax)
!!            stlmin = stlmin - rtol
!!            stlmax = stlmax + rtol
!!            ! Set crystal cell
!!            if (ierror == 0) call Set_Crystal_Cell(p_cell(1:3),p_cell(4:6),cell_fixed)
!!            if (ierror == 0) call check_error('py_ub_from_q_and_cell',ierror)
!!            ! Set space group P 1
!!            if (ierror == 0) call Set_SpaceGroup('P 1',spg)
!!            if (ierror == 0) call check_error('py_ub_from_q_and_cell',ierror)
!!            ! Compute reflections
!!            if (ierror == 0) call HKL_Gen_Sxtal(cell_fixed,spg,stlmin,stlmax,hkls)
!!            if (ierror == 0) call check_error('py_ub_from_q_and_cell',ierror)
!!        end if
!!        if (ierror == 0) then
!!            ! Find reflections that can be indexed
!!            allocate(candidates(0:nq,1:nq))
!!            do i = 1 , nq
!!                candidates(0,i) = 0
!!                do j = 1 , hkls%nref
!!                    if (abs(s(i) - hkls%ref(j)%s) > rtol) cycle
!!                    ! Candidate
!!                    candidates(0,i) = candidates(0,i) + 1
!!                    candidates(candidates(0,i),i) = j
!!                end do
!!                if (candidates(0,i) > 0) ncandidates = ncandidates + 1
!!            end do
!!            if (ncandidates < 2) then
!!                ierror = EXCEPTION_ERROR
!!                err_cfml%msg = 'Number of indexed reflections less than two. UB matrix cannot be !determined'
!!            end if
!!        end if
!!        if (ierror == 0) then
!!            ! Test cells
!!            ! Select pairs for building the UB matrix. Test n_pairs as maximum.
!!            allocate(pairs(2,npairs_max))
!!            npairs = 0
!!            do i = 1, nq
!!                if (candidates(0,i) > 0) then
!!                    pairs(1,npairs+1) = i
!!                    do j = i + 1 , nq
!!                        if (candidates(0,j) > 0) then
!!                            ! Angle between i,j must be > angle_pair
!!                            angle = acosd(dot_product(p_q(1:3,i),p_q(1:3,j)) / (4 * s(i) * s(j)))
!!                            if (angle > angle_min) then
!!                                pairs(2,npairs+1) = j
!!                                npairs = npairs + 1
!!                                exit
!!                            end if
!!                        end if
!!                    end do
!!                    if (npairs == npairs_max) exit
!!                end if
!!            end do
!!            if (npairs == 0) then
!!                ierror = EXCEPTION_ERROR
!!                err_cfml%msg = 'Unable to find a pair of reflections for testing UB'
!!            end if
!!        end if
!!        if (ierror == 0) then
!!            ! Start testing pairs
!!            ncells = 0
!!!        rfac_max = max_rfac
!!!        B_sp = fixed_cell%BL_M
!!            do ip = 1 , npairs
!!                i = pairs(1,ip)
!!                j = pairs(2,ip)
!!                do n = 1 , candidates(0,i)
!!                    h1 = hkls%ref(candidates(n,i))%h
!!                    q1 = p_q(1:3,i)
!!                    do m = 1 , candidates(0,j)
!!                        h2 = hkls%ref(candidates(m,j))%h
!!                        q2 = p_q(1:3,j)
!!!                    call GenUB(B_sp,h1,h2,h1c,h2c,UB_sp,ierr)
!!!                    if (ierr /= 0) cycle
!!!                    UB = UB_sp
!!!                    UB_inv = Invert_A(UB)
!!!                    call Index_Set(tols,UB_inv,frac_aux,rfac_aux,state_aux)
!!!                    if (rfac_aux < rfac_max) then
!!!                        if (ncells < NCELLS_MAX) then
!!!                            ncells = ncells + 1
!!!                            UBs_inv(:,:,ncells) = UB_inv
!!!                            rfac(ncells) = rfac_aux
!!!                        else
!!!                            k = maxloc(rfac,1)
!!!                            UBs_inv(:,:,k) = UB_inv
!!!                            rfac(k) = rfac_aux
!!!                        end if
!!!                    end if
!!                    end do
!!                end do
!!            end do
!!        end if
!!!        if (ncells == 0) then
!!!            ! Return
!!!            ierror = tuple_create(ret,2)
!!!            ierror = ret%setitem(0,.False.)
!!!            ierror = ret%setitem(1,'Unable to find UB matrix. Increase Rfac')
!!!            r = ret%get_c_ptr()
!!!            return
!!!        end if
!!!
!!!        ! Compute Niggli cells and recompute rfac
!!!        allocate(frac(ncells))
!!!        !allocate(niggli(ncells))
!!!        do i = 1 , ncells
!!!            UB_inv_sp = UBs_inv(:,:,i)
!!!            ! Get Niggli cell
!!!            !call Niggli_Test(transpose(UB_inv_sp),niggli(i),trans_r)
!!!            !if (Err_Crys) then
!!!            !    rfac(i) = max_rfac + 1.
!!!            !    cycle
!!!            !end if
!!!            !UB_inv_sp = matmul(transpose(trans_r),UB_inv_sp)
!!!            UB_inv = UB_inv_sp
!!!            ! Index set with the Niggli cell
!!!            call Index_Set(tols,UB_inv,frac(i),rfac(i),state_aux)
!!!        end do
!!!
!!!        ! Order cells according to rfac
!!!        deallocate(indx)
!!!        allocate(indx(ncells))
!!!        call Sort(rfac(1:ncells),ncells,indx)
!!!
!!!        ! Store cells with rfac < max_rfac
!!!        allocate(bravais(ncells))
!!!        allocate(res(2,ncells))
!!!        allocate(UBs(3,3,ncells))
!!!        allocate(cells(6,ncells))
!!!        n = 0
!!!        do i = 1 , ncells
!!!            if (rfac(indx(i)) > max_rfac) exit
!!!            lattice = 'X'
!!!            ! Get Conventional cell
!!!            !call Get_Twofold_Axes(niggli(indx(i)),3.0,twofold_axis)
!!!            !if (twofold_axis%ntwo > 0) then
!!!            !    call Get_Conventional_Cell(twofold_axis,cell_c,trans_i,mess,ok,!lattice=lattice)
!!!            !    if (.not. ok) cycle
!!!            !    cell_c%cr_orth_cel = matmul(niggli(indx(i))%cr_orth_cel,transpose(trans_i))
!!!            !else ! Triclinic
!!!            !    cell_c = niggli(indx(i))
!!!            !    lattice = 'aP'
!!!            !end if
!!!            ! Check that the conventional cell is not outside limits
!!!            !if (any(cell_c%cell < len_min)) cycle
!!!            !if (any(cell_c%cell > len_max)) cycle
!!!            !if (any(cell_c%ang  < ang_min)) cycle
!!!            !if (any(cell_c%ang  > ang_max)) cycle
!!!            ! Check that this matrix has not been previously stored
!!!            stored = .false.
!!!            UB = Invert_A(UBs_inv(:,:,indx(i)))
!!!            !UB_sp = Invert_A(transpose(cell_c%cr_orth_cel))
!!!            !UB = UB_sp
!!!            do j = 1 , n
!!!                if (compare_UB(UB,UBs(:,:,j))) then
!!!                    stored = .true.
!!!                    exit
!!!                end if
!!!            end do
!!!            if (stored) cycle
!!!            n =  n + 1
!!!            res(1,n) = frac(indx(i))
!!!            res(2,n) = rfac(indx(i))
!!!            bravais(n) = lattice
!!!            UBs(:,:,n) = UB
!!!            !cells(1:3,n) = cell_c%cell(:)
!!!            !cells(4:6,n) = cell_c%ang(:)
!!!        end do
!!!
!!!        ! Build tuple to be returned
!!!        if (n == 0) then
!!!            ierror = tuple_create(ret,2)
!!!            ierror = ret%setitem(0,.False.)
!!!            ierror = ret%setitem(1,'No cells found, please increase Rfac')
!!!        else
!!!            ierror = ndarray_create(py_fits,res(1,1:n))
!!!            ierror = ndarray_create(py_rfac,res(2,1:n))
!!!            ierror = ndarray_create(py_ubs,UBs(:,:,1:n))
!!!            ierror = tuple_create(py_bravais,n)
!!!            do i = 1 , n
!!!                ierror = py_bravais%setitem(i-1,bravais(i))
!!!            end do
!!!            ierror = tuple_create(ret,5)
!!!            ierror = ret%setitem(0,.True.)
!!!            ierror = ret%setitem(1,py_fits)
!!!            ierror = ret%setitem(2,py_rfac)
!!!            ierror = ret%setitem(3,py_ubs)
!!!            ierror = ret%setitem(4,py_bravais)
!!!        end if
!!!
!!!
!!!
!!!
!!!
!!!
!!!        end if
!!
!!        ! Return tuple
!!        if (ierror == 0) then
!!            ierror = tuple_create(ret,2)
!!            ierror = ret%setitem(0,0)
!!            ierror = ret%setitem(1,trim(err_cfml%msg))
!!        !    ierror = ret%setitem(1,di_hkls)
!!        else
!!            ierror = tuple_create(ret,2)
!!            ierror = ret%setitem(0,-1)
!!            ierror = ret%setitem(1,trim(err_cfml%msg))
!!        end if
!!        resul = ret%get_c_ptr()
!!
!    end subroutine ub_from_q_and_cell

!    function py_psd_convert(self_ptr,args_ptr,kwargs_ptr) result(resul) bind(c)
        !! author: ILL Scientific Computing Group
        !! date: 23/03/2023
        !! display: public
        !! proc_internals: true
        !! summary: Wrapper for subroutine psd_convert.
        !! summary: Compute the scattering vector from angles and wavelength for 4-circle geometry.
        !!
        !! ARGS_PTR = (diffractometer,ch,ph,ga,om,nu)
        !!
        !! KWARGS_PTR = (shifts,origin)
        !!
        !! RESUL = (ierror,err_cfml%msg,px,pz,x_D,z_D,ga_P,nu_P)
        !  --------           -----------         -----------
        !  Variable           Python type         Description
        !  --------           -----------         -----------
        !  di_instrument      dict                diffractometer
        !  f_virtual          integer             stretching factor for virtual detectors. 1 otherwise.
        !  conversion         integer             conversion type
        !  ga_D               float               gamma of the detector(degrees)
        !  nu_D               float               nu of the detector (degrees)
        !  px                 float               pixel in x direction
        !  pz                 float               pixel in z direction
        !  x_D                float               x coordinate in the detector system of reference
        !  z_D                float               z coordinate in the detector system of reference
        !  ga_P               float               gamma (degrees)
        !  nu_P               float               nu (degrees)
        !  shifts             bool, optional      applied shifts
        !  origin             integer, optional   origin for pixel numbering
        !  ierror             integer             if ierror /= 0, an error occurred
        !  err_cfml%msg       string              error message
        !  nd_z1              ndarray(3;float32)  scattering vector

!        ! Arguments
!        type(c_ptr), value :: self_ptr
!        type(c_ptr), value :: args_ptr
!        type(c_ptr), value :: kwargs_ptr
!        type(c_ptr)        :: resul
!
!        ! Python / Fortran interface variables
!        type(dict) :: di_instrument  !! diffractometer
!        integer    :: f_virtual      !! stretching factor for virtual detectors. 1 otherwise.
!        integer    :: conversion     !! conversion type
!        real       :: ga_D           !! gamma of the detector(degrees)
!        real       :: nu_D           !! nu of the detector (degrees)
!        real       :: px             !! pixel in x direction
!        real       :: pz             !! pixel in z direction
!        real       :: x_D            !! x coordinate in the detector system of reference
!        real       :: z_D            !! z coordinate in the detector system of reference
!        real       :: ga_P           !! gamma (degrees)
!        real       :: nu_P           !! nu (degrees)
!        logical    :: shifts         !! use horizontal shifts for pixels
!        integer    :: origin         !! origin for pixel numbering
!
!        ! Local variables
!        integer :: ierror                 ! error flag
!        real(kind=cp), dimension(3) :: z1 ! scattering vector
!        type(object) :: item
!        type(tuple) :: args,kwargs,ret
!
!        ! Reset error variable
!        ierror = 0
!
!        ! In case of exception return C_NULL_PTR
!        resul = C_NULL_PTR
!
!        ! Unwrap_arguments
!        call unsafe_cast_from_c_ptr(args,args_ptr)
!        call unsafe_cast_from_c_ptr(kwargs,args_ptr)
!        if (ierror == 0) ierror = args%getitem(item,0)
!        !if (ierror == 0) ierror = cast(wave,item)
!        !if (ierror == 0) ierror = args%getitem(item,1)
!        !if (ierror == 0) ierror = cast(ch,item)
!        !if (ierror == 0) ierror = args%getitem(item,2)
!        !if (ierror == 0) ierror = cast(ph,item)
!        !if (ierror == 0) ierror = args%getitem(item,3)
!        !if (ierror == 0) ierror = cast(ga,item)
!        !if (ierror == 0) ierror = args%getitem(item,4)
!        !if (ierror == 0) ierror = cast(om,item)
!        !if (ierror == 0) ierror = args%getitem(item,5)
!        !if (ierror == 0) ierror = cast(nu,item)
!
!        ! Call CrysFML procedure
!        !if (ierror == 0) then
!        !    call clear_error()
!        !    z1 = z1frmd(wave,ch,ph,ga,om,nu)
!        !    if (err_cfml%flag) then
!        !        ierror = EXCEPTION_ERROR
!        !        call raise_exception(RuntimeError,"z1frmd: "//trim(err_cfml%msg))
!        !    end if
!        !else
!        !    ierror = EXCEPTION_ERROR
!        !    call raise_exception(RuntimeError,"z1frmd: Error reading arguments")
!        !end if
!
!        ! Return tuple
!        !if (ierror == 0) then
!        !    ierror = tuple_create(ret,3)
!        !    ierror = ret%setitem(0,0)
!        !    ierror = ret%setitem(1,trim(err_cfml%msg))
!        !    ierror = ndarray_create(nd_z1,z1)
!        !    ierror = ret%setitem(2,nd_z1)
!        !else
!        !    ierror = tuple_create(ret,2)
!        !    ierror = ret%setitem(0,-1)
!        !    ierror = ret%setitem(1,trim(err_cfml%msg))
!        !end if
!        resul = ret%get_c_ptr()
!
!    end function py_psd_convert

end module py_cfml_sxtal_geom
