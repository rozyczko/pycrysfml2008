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

module extension_cfml_diffpatt

    use forpy_mod
    use iso_c_binding

    use CFML_Atoms, only: AtList_Type,Allocate_Atom_List,Write_Atom_List
    use CFML_DiffPatt, only: DiffPat_Type,DiffPat_E_Type,allocate_pattern
    use CFML_GlobalDeps, only: Clear_Error,Err_CFML,cp,to_Deg
    use CFML_gSpaceGroups, only: SpG_Type,Set_SpaceGroup,Write_SpaceGroup_Info
    use CFML_IOForm, only: Read_Xtal_Structure
    use CFML_Maths, only: locate
    use CFML_Metrics, only: Cell_G_Type,set_Crystal_Cell,write_crystal_cell
    use CFML_Profiles, only: PseudoVoigt
    use CFML_Reflections,only: get_maxnumref,H_uni,Initialize_RefList,RefList_Type,Srefl_type
    use CFML_Strings, only: file_type,u_case,l_case
    use CFML_Structure_Factors,  only: Write_Structure_Factors, Structure_Factors,Init_Structure_Factors

    implicit none

    private

    public :: py_sim

    Type, public :: PowPat_CW_Conditions
        character(len=140) :: title
        integer :: job      ! 0: X-rays, 1: Neutrons
        real    :: Lambda   ! Wavelength
        real    :: U        ! Resolution parameter
        real    :: V        ! Resolution parameter
        real    :: W        ! Resolution parameter
        real    :: X        ! Resolution parameter
        real    :: Ls       ! Lorentzian size
        real    :: Thmin    ! 2theta min
        real    :: Thmax    ! 2theta max
        real    :: step     ! 2theta step
        real    :: scalef   ! Scale factor
        real    :: bkg      ! Background
    End Type PowPat_CW_Conditions

    contains

    function py_sim(self_ptr,args_ptr) result(resul) bind(c)
        !! author: ILL Scientific Computing Group
        !! date: 26/04/2023
        !! display: public
        !! proc_internals: true
        !! summary: Calculation of a diffraction pattern from a dictionary built from a json file.
        !!
        !! ARGS_PTR = (di_json)
        !!
        !! RESUL = (ierr,err_cfml%msg,nd_x,nd_y)
        !  --------           -----------         -----------
        !  Variable           Python type         Description
        !  --------           -----------         -----------
        !  di_json            dictionary          dictionary from a json file
        !  ierr               integer             if ierr /= 0, an error occurred
        !  err_cfml%msg       string              error message
        !  nd_x               ndarray(float32)    scattering angle
        !  nd_y               ndarray(float32)    intensity

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Python / Fortran interface variables
        type(dict)    :: di_json !! dictionary from a json file
        integer       :: ierr    !! error flag
        type(ndarray) :: nd_x    !! scattering angle
        type(ndarray) :: nd_y    !! intensity

        ! Local variables
        integer :: ierror,nat,i,nf,maxnumref,mult
        real(kind=cp), dimension(3) :: vcell,vang
        character(len=:), allocatable :: ph_name,spg_name,ex_name,mystr
        real :: stlmax,tini,tfin,tim
        real, dimension(:), allocatable :: x
        character(len=132) :: powfile
        character(len=8)  :: units="seconds"
        type(Cell_G_Type) :: cell
        type(Spg_Type) :: spg
        type(AtList_Type) :: a
        type(RefList_Type) :: hkl
        type(DiffPat_E_Type) :: Pat
        type(PowPat_CW_Conditions) :: PPC
        type(object) :: item
        type(tuple) :: args,ret
        type(dict) :: di_phs,di_ph,di_at,di_exs,di_ex
        type(list) :: li_keys,li_ph,li_at,li_ex

        ! Reset error variable
        ierr = 0
        ierror = 0
        call clear_error()

        ! In case of exception return C_NULL_PTR
        resul = C_NULL_PTR

        ! Get the dictionary with all the required information for computing a powder pattern
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%getitem(item,0)
        if (ierror == 0) ierror = cast(di_json,item)
        if (ierror /= 0) then
            err_cfml%ierr = -1
            err_cfml%msg = 'py_sim: error reading arguments'
        end if

        ! Get phase dictionary
        if (ierror == 0) ierror = di_json%getitem(item,'phases')
        if (ierror == 0) ierror = cast(li_ph,item)
        if (ierror == 0) ierror = li_ph%getitem(item,0)
        if (ierror == 0) ierror = cast(di_phs,item)
        if (ierror == 0) ierror = di_phs%keys(li_keys)

        ! Get the first phase
        if (ierror == 0) ierror = li_keys%getitem(item,0)
        if (ierror == 0) ierror = cast(ph_name,item)
        if (ierror == 0) ierror = di_phs%getitem(item,ph_name)
        if (ierror == 0) ierror = cast(di_ph,item)

        ! Get the unit cell
        if (ierror == 0) ierror = di_ph%getitem(vcell(1),'_cell_length_a')
        if (ierror == 0) ierror = di_ph%getitem(vcell(2),'_cell_length_b')
        if (ierror == 0) ierror = di_ph%getitem(vcell(3),'_cell_length_c')
        if (ierror == 0) ierror = di_ph%getitem(vang(1),'_cell_angle_alpha')
        if (ierror == 0) ierror = di_ph%getitem(vang(2),'_cell_angle_beta')
        if (ierror == 0) ierror = di_ph%getitem(vang(3),'_cell_angle_gamma')
        if (ierror /= 0) then
            err_cfml%ierr = -1
            err_cfml%msg = 'py_sim: error reading phases'
        end if
        if (ierror == 0) then
            call set_crystal_cell(vcell,vang,cell)
            if (err_cfml%ierr /= 0) ierror = -1
        end if

        ! Get the space group
        if (ierror == 0) ierror = di_ph%getitem(item,'_space_group_name_H-M_alt')
        if (ierror == 0) ierror = cast(spg_name,item)
        if (ierror /= 0) then
            err_cfml%ierr = -1
            err_cfml%msg = 'py_sim: error parsing space group'
        end if
        if (ierror == 0) then
            call set_spacegroup(spg_name,spg)
            if (err_cfml%ierr /= 0) ierror = -1
        end if

        ! Get the number of atoms
        if (ierror == 0) ierror = di_ph%getitem(item,'_atom_site')
        if (ierror == 0) ierror = cast(li_at,item)
        if (ierror == 0) ierror = li_at%len(nat)
        if (ierror /= 0) then
            err_cfml%ierr = -1
            err_cfml%msg = 'py_sim: error getting number of atoms'
        end if
        if (ierror == 0) then
            call Allocate_Atom_List(nat,a,'atm_type',0)
            do i = 1 , nat
                if (ierror == 0) ierror = li_at%getitem(item,i-1)
                if (ierror == 0) ierror = cast(di_at,item)
                if (ierror == 0) ierror = di_at%getitem(a%atom(i)%x(1),'_fract_x')
                if (ierror == 0) ierror = di_at%getitem(a%atom(i)%x(2),'_fract_y')
                if (ierror == 0) ierror = di_at%getitem(a%atom(i)%x(3),'_fract_z')
                if (ierror == 0) ierror = di_at%getitem(a%atom(i)%occ,'_occupancy')
                if (ierror == 0) ierror = di_at%getitem(item,'_label')
                if (ierror == 0) ierror = cast(mystr,item)
                if (ierror == 0) a%atom(i)%lab = mystr
                if (ierror == 0) ierror = di_at%getitem(item,'_type_symbol')
                if (ierror == 0) ierror = cast(mystr,item)
                if (ierror == 0) a%atom(i)%chemsymb = mystr
                if (ierror == 0) a%atom(i)%sfacsymb = mystr
                if (ierror == 0) ierror = di_at%getitem(a%atom(i)%u_iso,'_B_iso_or_equiv')
            end do
            if (ierror /= 0) then
                err_cfml%ierr = -1
                err_cfml%msg = 'py_sim: error building atom list'
            end if
        end if

        ! Calculate a default Powder Diffraction Pattern
        if (ierror == 0) then
            stlmax     = 0.6
            PPC%Title  = "Default Powder Pattern"
            PPC%U      = 0.0002
            PPC%V      =-0.0002
            PPC%W      = 0.012
            PPC%lambda = 1.54056 ! Wavelength
            PPC%X      = 0.0015
            PPC%Thmin  = 1.00
            PPC%step   = 0.05
            PPC%Thmax  = int(2.0*asind(stlmax*1.54056))
            PPC%job    = 0
            PPC%Ls     = 1900.0
            PPC%bkg    = 50.0
            nf         = 30
            powfile    = "powder_pattern.dat"
            units      = " seconds"
            tim        = 0.0

            ! Get instrumental information
            if (ierror == 0) ierror = di_json%getitem(item,'experiments')
            if (ierror == 0) ierror = cast(li_ex,item)
            if (ierror == 0) ierror = li_ex%getitem(item,0)
            if (ierror == 0) ierror = cast(di_exs,item)
            if (ierror == 0) ierror = di_exs%keys(li_keys)
            if (ierror == 0) ierror = li_keys%getitem(item,0)
            if (ierror == 0) ierror = cast(ex_name,item)
            if (ierror == 0) then
                if (l_case(ex_name) == 'npd') then
                    PPC%job = 1 ! neutrons
                    if (ierror == 0) ierror = di_exs%getitem(item,ex_name)
                    if (ierror == 0) ierror = cast(di_ex,item)
                    if (ierror == 0) ierror = di_ex%getitem(ppc%u,'_pd_instr_resolution_u')
                    if (ierror == 0) ierror = di_ex%getitem(ppc%v,'_pd_instr_resolution_v')
                    if (ierror == 0) ierror = di_ex%getitem(ppc%w,'_pd_instr_resolution_w')
                    if (ierror == 0) ierror = di_ex%getitem(ppc%x,'_pd_instr_resolution_x')
                    if (ierror == 0) ierror = di_ex%getitem(ppc%lambda,'_diffrn_radiation_wavelength')
                    if (ierror == 0) ierror = di_ex%getitem(ppc%thmin,'_pd_meas_2theta_range_min')
                    if (ierror == 0) ierror = di_ex%getitem(ppc%thmax,'_pd_meas_2theta_range_max')
                    if (ierror == 0) ierror = di_ex%getitem(ppc%step,'_pd_meas_2theta_range_inc')
                    if (ierror /= 0) then
                        err_cfml%ierr = -1
                        err_cfml%msg = 'py_sim: error reading experimental section'
                    end if
                else
                    ierror = -1
                    err_cfml%ierr = -1
                    err_cfml%msg = 'py_sim: this program only runs for NPD'
                end if
            else
                err_cfml%ierr = -1
                err_cfml%msg = 'py_sim: error reading experimental section'
            end if
            if (ierror == 0) then
                PPC%Ls  = 1900.0
                PPC%bkg =   20.0

                ! Calculate sinTheta/Lambda max from 2Thetamax
                stlmax = sind(min((PPC%thmax+10.0)*0.5,90.0)) / PPC%lambda

                ! Now calculate a powder diffraction pattern
                ! First generate reflections and calculate structure factors
                Mult = 2 * SpG%NumOps
                MaxNumRef = get_maxnumref(stlmax,Cell%Vol,mult=Mult)
                if (err_cfml%ierr == 0) call Initialize_RefList(MaxNumRef, hkl, "srefl")
                if (err_cfml%ierr == 0) call cpu_time(tini)
                if (err_cfml%ierr == 0) call H_Uni(Cell,SpG,.true.,0.0,stlmax,"s",MaxNumRef,hkl)
                if (err_cfml%ierr == 0) call cpu_time(tfin)
                if (err_cfml%ierr == 0) tim = tim + tfin-tini
                if (err_cfml%ierr == 0) then
                    if (PPC%job == 1) then      ! Neutrons
                        call Init_Structure_Factors(hkl,A,Spg,mode="NUC")
                    else if(PPC%job == 0) then  ! Xrays
                        call Init_Structure_Factors(hkl,A,Spg,mode="XRA",lambda=PPC%lambda)
                    end if
                end if
                if (err_cfml%ierr == 0) then
                    call cpu_time(tini)
                    if (PPC%job == 1) then      !Neutrons
                        call Structure_Factors(hkl,A,SpG,mode="NUC")
                    else if(PPC%job == 0) then !X-rays
                        call Structure_Factors(hkl,A,SpG,mode="XRA",lambda=PPC%lambda)
                    end if
                end if
                if (err_cfml%ierr == 0) call cpu_time(tfin)
                if (err_cfml%ierr == 0) tim = tim + tfin-tini
                if (err_cfml%ierr == 0) then
                    call cpu_time(tini)
                    PPC%Scalef = cell%RVol
                    call calc_powder_pattern(PPC,hkl,Pat)
                    call cpu_time(tfin)
                    tim = tim + tfin - tini
                end if
                if (err_cfml%ierr /= 0) ierror = -1
            end if
        end if

        ! Return tuple
        if (ierror == 0) then
            allocate(x(0:size(Pat%ycalc)-1))
            do i = 0 , size(Pat%ycalc)-1
                x(i) = Pat%xmin + i * Pat%step
            end do
            ierror = ndarray_create(nd_x,x)
            if (ierror == 0) ierror = ndarray_create(nd_y,Pat%ycalc+PPC%bkg)
            if (ierror /= 0) then
                err_cfml%ierr = -1
                err_cfml%msg = 'py_sim: error building numpy array with the pattern'
            end if
        end if
        if (ierror == 0) then
            ierror = tuple_create(ret,4)
            ierror = ret%setitem(0,ierr)
            ierror = ret%setitem(1,trim(err_cfml%msg))
            ierror = ret%setitem(2,nd_x)
            ierror = ret%setitem(3,nd_y)
            resul = ret%get_c_ptr()
        else
            ierr = ierror
            ierror = tuple_create(ret,2)
            ierror = ret%setitem(0,ierr)
            ierror = ret%setitem(1,trim(err_cfml%msg))
        end if

    end function py_sim

    pure subroutine TCH(Hg,Hl,Fwhm,Eta)
        !---- Arguments ----!
        real, intent(in)  :: hg
        real, intent(in)  :: hl
        real, intent(out) :: fwhm
        real, intent(out) :: eta

        !---- Variables ----!
        real, parameter :: o1= 2.69269, o2=2.42843, o3=4.47163, o4= 0.07842
        real, parameter :: e1= 1.36603, e2=0.47719, e3=0.11116
        real            :: ctl, tlr

        ! There is no exception handling because it is supposed to be
        ! perfomed before calling TCH
        ctl=hg**5.0+o1*hg**4.0*hl+o2*hg**3.0*hl**2.0+o3*hg**2.0*hl**3.0+  &
            o4*hg*hl**4.0+hl**5.0
        fwhm=ctl**0.2
        tlr = hl/fwhm
        eta = max(1.0e-06, e1*tlr-e2*tlr*tlr+e3*tlr**3.0)  !eta
    end subroutine TCH

    subroutine Calc_Powder_Pattern(Ppc,Hkl,Pat)
        !---- Argument ----!
        Type(PowPat_CW_Conditions),     intent(in)  :: PPC
        type(RefList_Type),             intent(in)  :: hkl
        Type(DiffPat_E_Type),           intent(out) :: Pat

        !--- Local Variables ----!
        integer :: i,j,npts,i1,i2
        real    :: Intens,Bragg,Hl,Hg, ss,cs,tt,th1,th2,LorentzF, Y,eta,fwhm,chw

        npts=(PPC%Thmax-PPC%Thmin)/PPC%step + 1.02
        call allocate_pattern(Pat,npts)
        Pat%Title=adjustl(Trim(PPC%title))
        i=len_trim(Pat%Title)
        write(unit=Pat%Title(i+2:),fmt="(a,f7.4,f7.1)") " => lambda,Ls: ", &
                   PPC%Lambda,PPC%Ls

        Pat%ScatVar="2-Theta"
        Pat%instr="Calculated Pattern"
        Pat%xmin= PPC%Thmin
        Pat%xmax= PPC%Thmax
        Pat%ymin= 0.0
        Pat%ymax=0.0
        !Pat%scal=1.0
        Pat%monitor=0.0
        Pat%step=PPC%step
        Pat%Tsample=300.0
        Pat%Tset=300.0
        Pat%npts=npts
        Pat%ct_step=.true.
        Pat%wave=[PPC%Lambda,PPC%Lambda,0.0,0.0,0.0]
        chw=15.0
        do i=1,npts
            Pat%x(i)=Pat%xmin+real(i-1)*Pat%step
        end do

        Y= to_deg*PPC%Lambda/PPC%Ls
        select type(ref => hkl%ref)
            type is (Srefl_type)
            do i=1,hkl%nref
                ss=PPC%Lambda*ref(i)%S
                cs=sqrt(abs(1.0-ss*ss))
                tt=ss/cs
                LorentzF=0.5/(ss*ss*cs)
                Bragg=2.0*asind(ss)
                HG=sqrt(tt*(PPC%U*tt+PPC%V)+PPC%W)
                HL=PPC%X*tt + Y/cs
                call TCH(hg,hl,fwhm,eta)
                select case(nint(eta*10.0))
                    case(:2)
                       chw=25.0
                    case(3:5)
                       chw=45.0
                    case(6:7)
                       chw=60.0
                    case(8:)
                       chw=90.0
                end select
                th1=Bragg-chw*fwhm
                th2=Bragg+chw*fwhm
                i1=Locate(Pat%x,th1,npts)
                i2=Locate(Pat%x,th2,npts)
                i1=max(i1,1)
                i2=min(i2,npts)
                Intens= LorentzF *ref(i)%mult * ref(i)%Fc**2 * PPC%Scalef
                do j=i1,i2
                    Pat%ycalc(j)=Pat%ycalc(j)+ PseudoVoigt( Pat%x(j)-Bragg, (/fwhm,eta /) ) * Intens
                end do
            end do
        end select

    end subroutine Calc_Powder_Pattern

end module extension_cfml_diffpatt