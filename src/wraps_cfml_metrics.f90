module wraps_cfml_metrics

    use forpy_mod
    use cfml_metrics

    implicit none

    contains

    subroutine wrap_cell_type(for_var,dic_var,ierror)

        ! Arguments
        class(cell_type), intent(in)     :: for_var
        type(dict),       intent(inout) :: dic_var
        integer,          intent(out)   :: ierror

        ! Local variables
        type(ndarray) :: nd_cell,nd_scell,nd_ang,nd_sang

        ierror = 0
        if (ierror == 0) ierror = ndarray_create(nd_cell,for_var%cell)
        if (ierror == 0) ierror = dic_var%setitem('cell',nd_cell)
        if (ierror == 0) ierror = ndarray_create(nd_scell,for_var%scell)
        if (ierror == 0) ierror = dic_var%setitem('scell',nd_scell)
        if (ierror == 0) ierror = ndarray_create(nd_ang,for_var%ang)
        if (ierror == 0) ierror = dic_var%setitem('ang',nd_ang)
        if (ierror == 0) ierror = ndarray_create(nd_sang,for_var%sang)
        if (ierror == 0) ierror = dic_var%setitem('sang',nd_sang)
        if (ierror == 0) ierror = dic_var%setitem('vol',for_var%vol)
        if (ierror == 0) ierror = dic_var%setitem('svol',for_var%svol)

    end subroutine wrap_cell_type

    subroutine wrap_cell_g_type(for_var,dic_var,ierror)

        ! Arguments
        class(cell_g_type), intent(in)    :: for_var
        type(dict),         intent(inout) :: dic_var
        integer,            intent(out)   :: ierror

        ! Local variables
        type(ndarray) :: nd_rcell,nd_rang,nd_GD,nd_GR,nd_cr_orth_cel,nd_orth_cr_cel,nd_bl_m,nd_inv_bl_m

        ierror = 0
        call wrap_cell_type(for_var,dic_var,ierror)
        if (ierror == 0) ierror = ndarray_create(nd_rcell,for_var%rcell)
        if (ierror == 0) ierror = dic_var%setitem('rcell',nd_rcell)
        if (ierror == 0) ierror = ndarray_create(nd_rang,for_var%rang)
        if (ierror == 0) ierror = dic_var%setitem('rang',nd_rang)
        if (ierror == 0) ierror = dic_var%setitem('rvol',for_var%rvol)
        if (ierror == 0) ierror = ndarray_create(nd_GD,for_var%GD)
        if (ierror == 0) ierror = dic_var%setitem('GD',nd_GD)
        if (ierror == 0) ierror = ndarray_create(nd_GR,for_var%GR)
        if (ierror == 0) ierror = dic_var%setitem('GR',nd_GR)
        if (ierror == 0) ierror = ndarray_create(nd_cr_orth_cel,for_var%cr_orth_cel)
        if (ierror == 0) ierror = dic_var%setitem('cr_orth_cel',nd_cr_orth_cel)
        if (ierror == 0) ierror = ndarray_create(nd_orth_cr_cel,for_var%orth_cr_cel)
        if (ierror == 0) ierror = dic_var%setitem('orth_cr_cel',nd_orth_cr_cel)
        if (ierror == 0) ierror = ndarray_create(nd_bl_m,for_var%bl_m)
        if (ierror == 0) ierror = dic_var%setitem('bl_m',nd_bl_m)
        if (ierror == 0) ierror = ndarray_create(nd_inv_bl_m,for_var%inv_bl_m)
        if (ierror == 0) ierror = dic_var%setitem('inv_bl_m',nd_inv_bl_m)
        if (ierror == 0) ierror = dic_var%setitem('carttype',for_var%carttype)

    end subroutine wrap_cell_g_type

end module wraps_cfml_metrics