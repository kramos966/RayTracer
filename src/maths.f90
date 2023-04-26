module maths
    use, intrinsic :: iso_fortran_env, only: dp=>real64, sp=>real32

    private

contains
    real(sp) pure function norm(a, b)
        real(sp), dimension(3), intent(in) :: a, b
    end function norm
end module maths
