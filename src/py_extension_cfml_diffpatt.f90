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

module extension_cfml_diffpatt

    !<
    ! --------------------------------
    ! Functions accesibles from Python
    ! --------------------------------
    ! function py_diffpatt_sim(self_ptr,args_ptr) result(resul) bind(c)
    !
    ! -------------------
    ! Internal procedures
    ! -------------------
    ! pure subroutine TCH(Hg,Hl,Fwhm,Eta)
    ! subroutine Calc_Powder_Pattern(Ppc,Hkl,Pat)
    !>

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

    public :: py_diffpatt_sim

    Type, public :: PowPat_CW_Conditions
        character(len=140) :: title
        integer :: job
        real    :: Lambda, U, V, W, X, Ls
        real    :: Thmin, Thmax, step
        real    :: scalef,bkg
    End Type PowPat_CW_Conditions

    contains

    function py_diffpatt_sim(self_ptr,args_ptr) result(resul) bind(c)

        !> Compute a powder diffraction pattern from a dictionary
        !  obtained from a json file

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Local variables
        integer :: ierror,nat,i,nf,ier,maxnumref,mult
        real(kind=cp), dimension(3) :: vcell,vang
        character(len=:), allocatable :: ph_name,spg_name,ex_name,mystr
        integer :: lun=11,lp=21
        real :: stlmax,tini,tfin,tim,ftim=1.0
        real, dimension(:), allocatable :: x
        character(len=132) :: line,powfile
        character(len=3)  :: mode
        character(len=8)  :: units="seconds",radiation
        type(Cell_G_Type) :: cell
        type(Spg_Type) :: spg
        type(AtList_Type) :: a
        Type(RefList_Type) :: hkl
        Type(DiffPat_E_Type) :: Pat
        Type(PowPat_CW_Conditions) :: PPC
        type(nonetype) :: nret
        type(object) :: item
        type(tuple) :: args,ret
        type(ndarray) :: nd_x,nd_y
        type(dict) :: json,di_phs,di_ph,di_at,di_exs,di_ex
        type(list) :: li_keys,li_ph,li_at,li_ex

        call Clear_Error()

        ! Get the dictionary with all the required information for computing a powder pattern
        call unsafe_cast_from_c_ptr(args,args_ptr)
        ierror = args%getitem(item,0)
        if (ierror == 0) ierror = cast(json,item)
        if (ierror /= 0) then
            ierror = EXCEPTION_ERROR
            call raise_exception(RuntimeError,'py_diffpatt_sim: error reading phases')
        end if

        ! Get phase dictionary
        if (ierror == 0) ierror = json%getitem(item,'phases')
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
            ierror = EXCEPTION_ERROR
            call raise_exception(RuntimeError,'py_diffpatt_sim: error reading phases')
        end if
        if (ierror == 0) call set_crystal_cell(vcell,vang,cell)
        if (ierror == 0 .and. err_cfml%flag) then
            ierror = EXCEPTION_ERROR
            call raise_exception(RuntimeError,'py_diffpatt_sim: '//trim(err_cfml%msg))
        end if
        if (ierror == 0) call write_crystal_cell(cell,6)

        ! Get the space group
        if (ierror == 0) ierror = di_ph%getitem(item,'_space_group_name_H-M_alt')
        if (ierror == 0) ierror = cast(spg_name,item)
        if (ierror /= 0) then
            ierror = EXCEPTION_ERROR
            call raise_exception(RuntimeError,'py_diffpatt_sim: error parsing space group')
        end if
        if (ierror == 0) call set_spacegroup(spg_name,spg)
        if (err_cfml%flag) then
            ierror = EXCEPTION_ERROR
            call raise_exception(RuntimeError,'py_diffpatt_sim: '//trim(err_cfml%msg))
        end if
        if (ierror == 0) call write_spacegroup_info(spg,6)

        ! Get the number of atoms
        if (ierror == 0) ierror = di_ph%getitem(item,'_atom_site')
        if (ierror == 0) ierror = cast(li_at,item)
        if (ierror == 0) ierror = li_at%len(nat)
        if (ierror /= 0) then
            ierror = EXCEPTION_ERROR
            call raise_exception(RuntimeError,'py_diffpatt_sim: error getting number of atoms')
        end if
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
            ierror = EXCEPTION_ERROR
            call raise_exception(RuntimeError,'py_diffpatt_sim: error building atom list')
        end if

        ! Calculate a default Powder Diffraction Pattern
        if (.not. err_cfml%flag) then
            open(unit=lun,file=trim(ph_name)//".powder",status="replace",action="write")
            write(unit=lun,fmt="(/,/,6(a,/))")                                            &
            "            ------ PROGRAM SIMPLE POWDER PATTERN CALCULATION  ------"        , &
            "                    ---- Version 0.1 April-2009----"                         , &
            "    **********************************************************************"  , &
            "    * Calculates powder diffraction pattern from a *.CFL or a *.CIF file *"  , &
            "    **********************************************************************"  , &
            "                          (JRC- April 2009 )"
            stlmax=0.6; PPC%Title="Default Powder Pattern"
            PPC%U=0.0002; PPC%V=-0.0002; PPC%W=0.012; PPC%LAMBDA=1.54056; PPC%X=0.0015
            PPC%Thmin=1.00; PPC%step=0.05;  PPC%Thmax= int(2.0*asind(stlmax*1.54056)); PPC%job=0
            PPC%Ls=1900.0;  nf=30; PPC%bkg=50.0
            powfile="powder_pattern.dat"
            units=" seconds"
            tim=0.0

            ! Write initial structure information in the .powder file
            call Write_Crystal_Cell(Cell,lun)
            call Write_SpaceGroup_Info(SpG,lun)
            call Write_Atom_List(A,Iunit=lun)

            ! Get instrumental information
            if (ierror == 0) ierror = json%getitem(item,'experiments')
            if (ierror == 0) ierror = cast(li_ex,item)
            if (ierror == 0) ierror = li_ex%getitem(item,0)
            if (ierror == 0) ierror = cast(di_exs,item)
            if (ierror == 0) ierror = di_exs%keys(li_keys)
            if (ierror == 0) ierror = li_keys%getitem(item,0)
            if (ierror == 0) ierror = cast(ex_name,item)
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
                    ierror = EXCEPTION_ERROR
                    call raise_exception(RuntimeError,'py_diffpatt_sim: error reading experimental section')
                end if
            else
                ierror = EXCEPTION_ERROR
                call raise_exception(RuntimeError,'py_diffpatt_sim: this program only runs for NPD')
            end if
            if (ierror == 0) then
                PPC%Ls=1900.0;powfile="powder_pattern.dat";PPC%bkg=20.0

                ! Calculate sinTheta/Lambda max from 2Thetamax     PPC%Thmax= int(2.0*asind(stlmax*1.!56))
                stlmax = sind(min((PPC%Thmax+10.0)*0.5,90.0)) / PPC%lambda

                if (PPC%job == 0) then      !X-rays
                    write(unit=lun,fmt="(/,a)")  " => CALCULATION OF X-RAY POWDER DIFFRACTION PATTERN "
                    PPC%title=Trim(PPC%title)//"; X-RAYS: "
                else
                    write(unit=lun,fmt="(/,a)")  " => CALCULATION OF NEUTRON POWDER DIFFRACTION PATTERN"
                    PPC%title=Trim(PPC%title)//"; NEUTRONS: "
                end if
                write(unit=lun,fmt="(  a,4f10.5)")  " => Resolution parameters UVWX: ",PPC%U,PPC%V,PPC%W,PPC%X
                write(unit=lun,fmt="(  a, f10.5)")  " => Lambda: ",PPC%lambda
                write(unit=lun,fmt="(  a, f10.5,a)")" => Background level: ",PPC%bkg," counts"
                write(unit=lun,fmt="(  a,2f10.2)")  " => Lorentzian size: ",PPC%Ls
                write(unit=lun,fmt="(  a,3f10.5)")  " => 2Theta range and step: ",PPC%Thmin,PPC%step,PPC%Thmax
                write(unit=lun,fmt="(  a,3f10.5)")  " => Maximum sin(Theta)/Lambda (for generating reflections): ",stlmax

                ! Now calculate a powder diffraction pattern
                ! First generate reflections and calculate structure factors
                Mult=2*SpG%NumOps
                MaxNumRef = get_maxnumref(stlmax,Cell%Vol,mult=Mult)
                if (.not. err_cfml%flag) call Initialize_RefList(MaxNumRef, hkl, "srefl")
                if (.not. err_cfml%flag) call cpu_time(tini)
                if (.not. err_cfml%flag) call H_Uni(Cell,SpG,.true.,0.0,stlmax,"s",MaxNumRef,hkl)
                if (.not. err_cfml%flag) call cpu_time(tfin)
                if (.not. err_cfml%flag) tim=tim + tfin-tini
                if (.not. err_cfml%flag) write(unit=lun,fmt="(a,i9)") " => Total number of generated reflections is ",hkl%nref
                if (.not. err_cfml%flag) then
                    if (PPC%job == 1) then      !Neutrons
                        call Init_Structure_Factors(hkl,A,Spg,mode="NUC",lun=lun)
                    else if(PPC%job == 0) then !Xrays
                        call Init_Structure_Factors(hkl,A,Spg,mode="XRA",lambda=PPC%lambda,lun=lun)
                    end if
                end if
                if (.not. err_cfml%flag) then
                    call cpu_time(tini)
                    if (PPC%job == 1) then      !Neutrons
                        call Structure_Factors(hkl,A,SpG,mode="NUC")
                    else if(PPC%job == 0) then !X-rays
                        call Structure_Factors(hkl,A,SpG,mode="XRA",lambda=PPC%lambda)
                    end if
                end if
                if (.not. err_cfml%flag) call cpu_time(tfin)
                if (.not. err_cfml%flag) tim = tim + tfin-tini
                if (.not. err_cfml%flag) write(unit=lun,fmt="(a,f15.3,a)") " => CPU-time used for Structure_Factors: ",(tfin-tini)*ftim,units
                if (.not. err_cfml%flag) then
                    if (radiation(1:1) == "N") then
                        call Write_Structure_Factors(hkl,lun,mode="NUC")
                    else
                        call Write_Structure_Factors(hkl,lun,mode="XRA")
                    end if
                end if
                if (.not. err_cfml%flag) then
                    call cpu_time(tini)
                    PPC%Scalef=cell%RVol
                    call Calc_powder_pattern(PPC,hkl,Pat)
                    call cpu_time(tfin)
                    tim=tim+ tfin-tini
                    write(unit=lun,fmt="(a,f15.3,a)") " => CPU-time used for Calc_powder_pattern: ",(tfin-tini)*ftim,units
                    write(unit=lun,fmt="(a,f15.3,a)") " => CPU-time for all calculations: ",tim*ftim,units
                    open(unit=lp,file=trim(powfile),status="replace",action="write")
                        write(unit=lp,fmt="(a)") "!"//trim(Pat%title)
                        write(unit=lp,fmt="(3f10.4)") Pat%xmin,Pat%step,Pat%xmax
                        write(unit=lp,fmt="(8f16.4)") Pat%ycalc+PPC%bkg
                    close(unit=lun)
                end if
                if (err_cfml%flag) then
                    write(unit=lun,fmt="(a)") " => Error in calculating the diffraction pattern"
                    write(unit=lun,fmt="(a)") " => "//trim(ERR_CFML%Msg)
                    ierror = EXCEPTION_ERROR
                    call raise_exception(RuntimeError,'py_diffpatt_sim: '//trim(ERR_CFML%Msg))
                end if
            end if
        end if

        ! Return tuple
        if (.not. err_cfml%flag) then
            allocate(x(0:size(Pat%ycalc)-1))
            do i = 0 , size(Pat%ycalc)-1
                x(i) = Pat%xmin + i * Pat%step
            end do
            ierror = tuple_create(ret,2)
            ierror = ndarray_create(nd_x,x)
            ierror = ndarray_create(nd_y,Pat%ycalc+PPC%bkg)
            ierror = ret%setitem(0,nd_x)
            ierror = ret%setitem(1,nd_y)
            resul = ret%get_c_ptr()
        else
            ierror = nonetype_create(nret)
            resul = nret%get_c_ptr()
        end if

    end function py_diffpatt_sim

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