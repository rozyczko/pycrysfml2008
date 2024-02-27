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

module py_cfml_gspacegroups

    use forpy_mod
    use iso_c_binding

    use cfml_globaldeps, only: err_cfml,clear_error
    use cfml_gSpaceGroups
    use cfml_python, only: check_number_of_arguments,get_var_from_item,ndarray_to_pointer,pointer_to_array,pointer_to_array_alloc,&
                           unwrap_spg_type,wrap_group_type,wrap_symm_oper_type
    use cfml_rational, only: real
    use cfml_strings, only: u_case

    implicit none

    type(PythonModule), save :: mod_gspacegroups
    type(PythonMethodTable), save :: table_gspacegroups

    contains

    ! Initialization function for Python 3
    ! Called when importing module
    ! Must use bind(c, name="PyInit_<module name>")
    ! Return value must be type(c_ptr),
    ! use the return value of PythonModule%init
    function PyInit_py_cfml_gspacegroups() bind(c,name="PyInit_py_cfml_gspacegroups") result(m)
#ifdef WIN32
    !DEC$ ATTRIBUTES DLLEXPORT :: PyInit_py_cfml_gspacegroups
#endif

        ! Local variables
        type(c_ptr) :: m

        m = Init()

    end function PyInit_py_cfml_gspacegroups

    function Init() result(m)

        ! Local variables
        type(c_ptr) :: m
        integer :: ierror

        ierror = Forpy_Initialize()

        ! Build method table
        call table_gspacegroups%init(5)
        call table_gspacegroups%add_method("equal_group","py_equal_group",METH_VARARGS,c_funloc(py_equal_group))
        call table_gspacegroups%add_method("get_multip_pos","py_get_multip_pos",METH_VARARGS,c_funloc(py_get_multip_pos))
        call table_gspacegroups%add_method("get_op_from_symb","py_get_op_from_symb",METH_VARARGS,c_funloc(py_get_op_from_symb))
        call table_gspacegroups%add_method("get_symb_from_mat","py_get_symb_from_mat",METH_VARARGS,c_funloc(py_get_symb_from_mat))
        call table_gspacegroups%add_method("set_spacegroup","py_set_spacegroup",METH_VARARGS,c_funloc(py_set_spacegroup))

        ! Build mod_gspacegroups
        m = mod_gspacegroups%init("py_cfml_gspacegroups","A Python API for CrysFML08",table_gspacegroups)

    end function Init

    function py_equal_group(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(dict)    :: di_spg

        ! Local variables
        integer, parameter :: NMANDATORY = 2
        integer :: ierror,narg
        logical :: info
        class(spg_type), allocatable :: spg_1,spg_2
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        info = .false.
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Check the number of items
        call check_number_of_arguments('py_equal_group',args,NMANDATORY,narg,ierror)

        ! Get arguments
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_equal_group','spg',item,di_spg,ierror)
        if (ierror == 0) call unwrap_spg_type(di_spg,spg_1,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_equal_group','spg',item,di_spg,ierror)
        if (ierror == 0) call unwrap_spg_type(di_spg,spg_2,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_equal_group: error parsing arguments'
        end if

        ! Call Fortran procedure
        if (ierror == 0) then
            if (spg_1 == spg_2) info = .true.
        end if
        if (ierror == 0) ierror = err_cfml%ierr

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,info)
        resul = ret%get_c_ptr()

    end function py_equal_group

    function py_get_multip_pos(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(ndarray) :: nd_x
        type(dict)    :: di_spg

        ! Local variables
        integer, parameter :: NMANDATORY = 2
        integer :: ierror,narg,m
        real, dimension(:), pointer :: p_x
        real, dimension(:), allocatable :: x
        class(spg_type), allocatable :: spg
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        m = 0
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Check the number of items
        call check_number_of_arguments('py_get_multip_pos',args,NMANDATORY,narg,ierror)

        ! Get arguments
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_get_multip_pos','x',item,nd_x,ierror)
        if (ierror == 0) call ndarray_to_pointer('py_get_multip_pos','x',nd_x,p_x,ierror)
        if (ierror == 0) call pointer_to_array_alloc('py_get_h_info','x',p_x,x,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_get_multip_pos','spg',item,di_spg,ierror)
        if (ierror == 0) call unwrap_spg_type(di_spg,spg,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_get_multip_pos: error parsing arguments'
        end if

        ! Call Fortran procedure
        if (ierror == 0) m = get_multip_pos(x,spg)
        if (ierror == 0) ierror = err_cfml%ierr

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,m)
        resul = ret%get_c_ptr()

    end function py_get_multip_pos

    function py_get_op_from_symb(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        character(len=:), allocatable :: symb ! symbol of the operator

        ! Local variables
        integer, parameter :: NMANDATORY = 1
        integer :: ierror,narg
        type(symm_oper_type) :: op
        type(object) :: item
        type(dict) :: di_op
        type(tuple) :: args,ret

        ierror = 0
        ierror = dict_create(di_op)
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Check the number of items
        call check_number_of_arguments('py_get_op_from_symb',args,NMANDATORY,narg,ierror)

        ! Get arguments
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_get_op_from_symb','symb',item,symb,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_get_op_from_symb: error parsing arguments'
        end if

        ! Call Fortran procedure
        if (ierror == 0) op = get_op_from_symb(symb)
        if (ierror == 0) ierror = err_cfml%ierr

        ! Wrapping
        if (ierror == 0) call wrap_symm_oper_type(op,di_op,ierror)

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,di_op)
        resul = ret%get_c_ptr()

    end function py_get_op_from_symb

    function py_get_symb_from_mat(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        type(ndarray) :: nd_mat
        type(ndarray) :: nd_tr
        type(dict)    :: di_kwargs

        ! Local variables
        integer, parameter :: NMANDATORY = 2
        integer :: ierror,narg
        integer, dimension(3,3) :: mat_int
        integer, dimension(:,:), pointer :: p_mat_int
        real, dimension(3) :: tr
        real, dimension(3,3) :: mat_real
        real, dimension(:), pointer :: p_tr
        real, dimension(:,:), pointer :: p_mat_real
        character(len=1) :: order
        character(len=:), allocatable :: mystr
        logical :: is_opposite,is_mat_real
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        mystr = ''
        is_mat_real = .true.
        is_opposite = .false.
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Check the number of items
        call check_number_of_arguments('py_get_symb_from_mat',args,NMANDATORY,narg,ierror)

        ! Get arguments
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_get_symb_from_mat','mat',item,nd_mat,ierror)
        if (ierror == 0) ierror = args%getitem(item,1)
        if (ierror == 0) call get_var_from_item('py_get_symb_from_mat','tr',item,nd_tr,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_get_symb_from_mat: error parsing arguments'
        end if
        if (ierror == 0 .and. narg > NMANDATORY) then
            ierror = args%getitem(item,2)
            if (ierror == 0) call get_var_from_item('py_get_symb_from_mat','kwargs',item,di_kwargs,ierror)
            ierror = di_kwargs%getitem(item,'opposite')
            if (ierror /= 0) then
                call err_clear()
                call clear_error()
                ierror = 0
            else
                is_opposite = .true.
            end if
        end if

        ! Unwrap nd_mat
        if (ierror == 0) then
            if (ierror == 0) call ndarray_to_pointer('py_get_symb_from_mat','mat',nd_mat,p_mat_real,ierror,order)
            if (ierror == 0) then
                call pointer_to_array('py_get_symb_from_mat','mat',p_mat_real,mat_real,ierror,order)
            else
                ierror = 0
                call err_clear()
                call clear_error()
                is_mat_real = .false.
                call ndarray_to_pointer('py_get_symb_from_mat','mat',nd_mat,p_mat_int,ierror,order)
                if (ierror == 0) call pointer_to_array('py_get_symb_from_mat','mat',p_mat_int,mat_int,ierror,order)
            end if
        end if

        ! Unwrap nd_tr
        if (ierror == 0) call ndarray_to_pointer('py_get_symb_from_mat','tr',nd_tr,p_tr,ierror)
        if (ierror == 0) call pointer_to_array('py_get_symb_from_mat','tr',p_tr,tr,ierror)

        ! Call Fortran procedure
        if (ierror == 0) then
            if (is_mat_real) then
                if (is_opposite) then
                    mystr = get_symb_from_mat(mat_real,tr,.true.)
                else
                    mystr = get_symb_from_mat(mat_real,tr)
                end if
            else
                if (is_opposite) then
                    mystr = get_symb_from_mat(mat_int,tr,.true.)
                else
                    mystr = get_symb_from_mat(mat_int,tr)
                end if
            end if
            ierror = err_cfml%ierr
        end if

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,mystr)
        resul = ret%get_c_ptr()

    end function py_get_symb_from_mat

    function py_set_spacegroup(self_ptr,args_ptr) result(resul) bind(c)

        ! Arguments
        type(c_ptr), value :: self_ptr
        type(c_ptr), value :: args_ptr
        type(c_ptr)        :: resul

        ! Variables in args_ptr
        character(len=:), allocatable :: generator ! Generator string

        ! Local variables
        integer, parameter :: NMANDATORY = 1
        integer :: ierror,narg,i
        logical :: is_setting
        character(len=5) :: mode
        character(len=180) :: setting
        character(len=:), allocatable :: key
        type(dict) :: di_spg
        class(spg_type), allocatable :: spg
        type(object) :: item
        type(tuple) :: args,ret

        ierror = 0
        mode = ''
        is_setting = .false.
        ierror = dict_create(di_spg)
        call clear_error()

        ! Use unsafe_cast_from_c_ptr to cast from c_ptr to tuple/dict
        call unsafe_cast_from_c_ptr(args,args_ptr)

        ! Check the number of items
        call check_number_of_arguments('py_read_setspacegroup',args,NMANDATORY,narg,ierror)

        ! Get arguments
        if (ierror == 0) ierror = args%getitem(item,0)
        if (ierror == 0) call get_var_from_item('py_read_setspacegroup','generator',item,generator,ierror)
        if (ierror /= 0 .and. err_cfml%ierr == 0) then
            err_cfml%ierr = ierror
            err_cfml%msg = 'py_read_setspacegroup: error parsing arguments'
        end if

        ! Syntax analysis of generator and call to set_spacegroup
        if (ierror == 0) then
            generator = trim(generator)
            generator = adjustl(generator)
            i = index(generator,' ')
            if (i > 0) then
                key = u_case(generator(1:i-1))
                select case (key)
                case ('HALL','MHALL','SPGR','SPACEG')
                    allocate(spg_type :: spg)
                    mode = 'symb'
                    generator = adjustl(generator(i:))
                case ('SHUB')
                    allocate(spg_type :: spg)
                    mode = 'shubn'
                    generator = adjustl(generator(i:))
                case ('UNI')
                    allocate(spg_type :: spg)
                    mode = 'uni' ! Key must be preserved in generator
                case ('SSG','SUPER','SSPG')
                    allocate(superspacegroup_type :: spg)
                    mode = 'super'
                    generator = adjustl(generator(i:))
                case ('GEN')
                    ! Get first generator
                    mode = 'gen'
                    generator = adjustl(generator(i:))
                case default
                    ierror = -1
                    err_cfml%ierr = ierror
                    err_cfml%msg = 'py_read_setspacegroup: error parsing generator. Keyword '//key//' unknown'
                end select
            else
                ierror = -1
                err_cfml%ierr = ierror
                err_cfml%msg = 'py_read_setspacegroup: error parsing generator.'
            end if
        end if
        if (ierror == 0) then
            i = index(generator,'::')
            if (i /= 0) then
                setting = adjustl(trim(generator(i+2:)))
                if (len(setting) > 0) is_setting = .true.
                generator = generator(:i-1)
            end if
        end if

        ! Call Fortran procedure
        if (ierror == 0) then
            select case (trim(mode))
            case ('shubn','super')
                if (is_setting) then
                    call set_spacegroup(generator,mode,spg,setting=setting)
                else
                    call set_spacegroup(generator,mode,spg)
                end if
            case default
                call set_spacegroup(generator,spg)
            end select
            ierror = err_cfml%ierr
        end if

        ! Wrapping
        if (ierror == 0) call wrap_group_type(spg,di_spg,ierror)

        ! Return
        if (ierror /= 0) call err_clear()
        ierror = tuple_create(ret,3)
        ierror = ret%setitem(0,err_cfml%ierr)
        ierror = ret%setitem(1,trim(err_cfml%msg))
        ierror = ret%setitem(2,di_spg)
        resul = ret%get_c_ptr()

    end function py_set_spacegroup

end module py_cfml_gspacegroups
