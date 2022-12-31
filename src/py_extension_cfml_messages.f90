module extension_cfml_messages

    use forpy_mod
    use iso_c_binding

    use CFML_GlobalDeps, only: Err_CFML

    contains

    subroutine check_error(procedure,ierror)

        ! Arguments
        character(len=*), intent(in)    :: procedure
        integer,          intent(inout) :: ierror

        if (err_cfml%flag) then
            ierror = EXCEPTION_ERROR
            call raise_exception(RuntimeError,procedure//': '//trim(err_cfml%msg))
        end if

    end subroutine check_error

end module extension_cfml_messages