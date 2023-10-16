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

module py_cfml_ioform

    use forpy_mod
    use iso_c_binding

    use cfml_atoms, only: atlist_type,matom_list_type
    use cfml_globaldeps, only: err_cfml,clear_error
    use cfml_gSpaceGroups, only: get_orbit,point_orbit,spg_type
    use cfml_ioform, only: read_xtal_structure
    use cfml_kvec_Symmetry, only: magsymm_k_type,magnetic_domain_type
    use cfml_metrics, only: cell_g_type
    use cfml_python, only: check_number_of_arguments,get_var_from_item,unwrap_dict_item,wrap_atlist_type,wrap_cell_type,wrap_group_type

    implicit none

    type(PythonModule), save :: mod_ioform
    type(PythonMethodTable), save :: table_ioform

    contains

    ! Initialization function for Python 3
    ! Called when importing module
    ! Must use bind(c, name="PyInit_<module name>")
    ! Return value must be type(c_ptr),
    ! use the return value of PythonModule%init
    function PyInit_py_cfml_ioform() bind(c,name="PyInit_py_cfml_ioform") result(m)
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_ioform

        ! Local variables
        type(c_ptr) :: m

        m = Init()

    end function PyInit_py_cfml_ioform

    function Init() result(m)

        ! Local variables
        type(c_ptr) :: m
        integer :: ierror

        ierror = Forpy_Initialize()

        ! Build method table
        call table_ioform%init(1)
        call table_ioform%add_method("read_xtal_structure","py_read_xtal_structure",METH_VARARGS,c_funloc(py_read_xtal_structure))

        ! Build mod_ioform
        m = mod_ioform%init("py_cfml_ioform","A Python API for CrysFML08",table_ioform)

    end function Init

    function py_read_xtal_structure(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        character(len=:), allocatable :: filename   !! Name of the file
        type(dict)                    :: di_kwargs  !! Optional arguments
        character(len=:), allocatable :: atm_typ    !! Atom type (optional)
        integer                       :: iphase     !! Number of the phase

        ! Local variables
        integer, parameter :: NMANDATORY = 1
        integer :: ierror,narg
        logical :: is_kwargs,is_atm_typ,is_mgp,is_matm,is_mag_dom,is_iphase,is_orbits
        type(dict) :: di_cell
        type(dict) :: di_spg
        type(dict) :: di_atm
        type(dict) :: di_mgp
        type(dict) :: di_matm
        type(dict) :: di_mag_dom
        type(list) :: li_orb
        type(magsymm_k_type) :: mgp
        type(matom_list_type) :: matm
        type(magnetic_domain_type) :: mag_dom
        type(object) :: item
        type(tuple) :: args,ret
        class(cell_g_type), allocatable :: cell
        class(spg_type), allocatable :: spg
        type(atlist_type) :: atm

        ierror = 0
        is_kwargs = .false.
        is_atm_typ = .false.
        is_mgp = .false.
        is_matm = .false.
        is_mag_dom = .false.
        is_iphase = .false.
        is_orbits = .false.
        ierror = dict_create(di_cell)
        ierror = dict_create(di_spg)
        ierror = dict_create(di_atm)
        ierror = dict_create(di_mgp)
        ierror = dict_create(di_matm)
        ierror = dict_create(di_mag_dom)
        ierror = list_create(li_orb)
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Check the number of items
        call check_number_of_arguments('py_read_xtal_structure',args,NMANDATORY,narg,ierror)
        if (ierror == 0) ierror = args%getitem(item,0)

        ! Get arguments
        if (ierror == 0) call get_var_from_item('py_read_xtal_structure','filename',item,filename,ierror)
        if (ierror == 0 .and. narg > NMANDATORY) then
            ierror = args%getitem(item,1)
            if (ierror == 0) call get_var_from_item('py_read_xtal_structure','kwargs',item,di_kwargs,ierror)
            if (ierror == 0) then
                call unwrap_dict_item('py_read_xtal_structure','atm_typ',di_kwargs,atm_typ,ierror)
                if (ierror /= 0) then
                    call err_clear()
                    call clear_error()
                    ierror = 0
                else
                    is_atm_typ = .true.
                end if
                ierror = di_kwargs%getitem(item,'mgp')
                if (ierror /= 0) then
                    call err_clear()
                    call clear_error()
                    ierror = 0
                else
                    is_mgp = .true.
                end if
                ierror = di_kwargs%getitem(item,'matm')
                if (ierror /= 0) then
                    call err_clear()
                    call clear_error()
                    ierror = 0
                else
                    is_matm = .true.
                end if
                ierror = di_kwargs%getitem(item,'mag_dom')
                if (ierror /= 0) then
                    call err_clear()
                    call clear_error()
                    ierror = 0
                else
                    is_mag_dom = .true.
                end if
                ierror = di_kwargs%getitem(item,'orbits')
                if (ierror /= 0) then
                    call err_clear()
                    call clear_error()
                    ierror = 0
                else
                    is_orbits = .true.
                end if
                call unwrap_dict_item('py_read_xtal_structure','iphase',di_kwargs,iphase,ierror)
                if (ierror /= 0) then
                    call err_clear()
                    call clear_error()
                    ierror = 0
                else
                    is_iphase = .true.
                end if
            end if
        end if
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_read_xtal_structure: error parsing arguments'
        end if

        ! Call Fortran procedure
        if (ierror == 0) then
            if (narg == NMANDATORY) then
                call read_xtal_structure(filename,cell,spg,atm)
            else if (is_iphase) then
                if (is_atm_typ) then
                    call read_xtal_structure(filename,cell,spg,atm,atm_typ,iphase=iphase)
                else
                    call read_xtal_structure(filename,cell,spg,atm,iphase=iphase)
                end if
            else if (is_mgp .and. is_matm) then
                if (is_mag_dom) then
                    call read_xtal_structure(filename,cell,spg,atm,mgp=mgp,matm=matm,mag_dom=mag_dom)
                else
                    call read_xtal_structure(filename,cell,spg,atm,mgp=mgp,matm=matm)
                end if
            else if (is_orbits) then
                call read_xtal_structure(filename,cell,spg,atm)
            end if
        end if

        ! Orbits
        if (is_orbits .and. ierror == 0) then
                call get_orbits_cartesian(cell,atm,spg,li_orb,ierror)
                if (ierror /= 0) then
                    err_cfml%ierr = ierror
                    err_cfml%msg = 'py_read_xtal_structure: error getting orbits'
                end if
        end if

        ! Wrapping
        if (ierror == 0) call wrap_cell_type(cell,di_cell,ierror)
        if (ierror == 0) call wrap_atlist_type(atm,di_atm,ierror)
        if (ierror == 0) call wrap_group_type(spg,di_spg,ierror)

        ! Return
        if (ierror /= 0) call err_clear()
        if (is_orbits) then
            ierror = tuple_create(ret,9)
        else
            ierror = tuple_create(ret,8)
        end if
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,di_cell)
        ierror = ret%setitem(3,di_spg)
        ierror = ret%setitem(4,di_atm)
        ierror = ret%setitem(5,di_mgp)
        ierror = ret%setitem(6,di_matm)
        ierror = ret%setitem(7,di_mag_dom)
        if (is_orbits) ierror = ret%setitem(8,li_orb)
        resul = ret%get_c_ptr()

    end function py_read_xtal_structure

    subroutine get_orbits_cartesian(cell,asu,spg,li_orb,ierror)

        ! Arguments
        type(Cell_G_Type),            intent(in)    :: cell
        type(AtList_Type),            intent(in)    :: asu
        class(Spg_Type),              intent(in)    :: spg
        type(list),                   intent(inout) :: li_orb
        integer,                      intent(inout) :: ierror

        ! Local variables
        integer :: i,j
        real :: a
        real, dimension(3,3) :: U ! Matrix used for computing magnetic moment in cartesian coordinates
        type(Point_Orbit) :: orbit
        type(dict), dimension(:), allocatable :: di_orb

        ierror = 0
        allocate(di_orb(asu%natoms))
        do i = 1 , 3
            a = 0.0
            do j = 1 , 3
                a = a + cell%cr_orth_cel(j,i)**2
            end do
            U(:,i) = cell%cr_orth_cel(:,i) / sqrt(a)
        end do
        do i = 1 , asu%natoms
            if (ierror == 0) call Get_Orbit(asu%atom(i)%x,spg,orbit,asu%atom(i)%moment)
            if (ierror == 0) ierror = err_cfml%ierr
            if (ierror == 0) ierror = dict_create(di_orb(i))
            if (ierror == 0) call wrap_orbit_cartesian(U,cell,orbit,di_orb(i),ierror)
            if (ierror == 0) ierror = li_orb%append(di_orb(i))
        end do

    end subroutine get_orbits_cartesian

    subroutine wrap_orbit_cartesian(U,cell,orbit,di_orb,ierror)

        ! Arguments
        real, dimension(3,3), intent(in)    :: U
        type(Cell_G_Type),    intent(in)    :: cell
        type(Point_Orbit),    intent(in)    :: orbit
        type(dict),           intent(inout) :: di_orb
        integer,              intent(inout) :: ierror

        ! Local variables
        type(ndarray) :: pos,pos_c,mom,mom_c

        ierror = di_orb%setitem('mult',orbit%mult)
        if (ierror == 0) ierror = ndarray_create(pos,orbit%pos)
        if (ierror == 0) ierror = di_orb%setitem('pos',pos)
        if (ierror == 0) ierror = ndarray_create(pos_c,matmul(cell%cr_orth_cel,orbit%pos))
        if (ierror == 0) ierror = di_orb%setitem('pos_c',pos_c)
        if (ierror == 0) ierror = ndarray_create(mom,orbit%mom)
        if (ierror == 0) ierror = di_orb%setitem('mom',mom)
        if (ierror == 0) ierror = ndarray_create(mom_c,matmul(U,orbit%mom))
        if (ierror == 0) ierror = di_orb%setitem('mom_c',mom_c)

    end subroutine wrap_orbit_cartesian

end module py_cfml_ioform
