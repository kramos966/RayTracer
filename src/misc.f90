module misc
    use, intrinsic :: iso_fortran_env, only: dp=>real64, sp=>real32
    implicit none

    private

    real(sp), parameter :: pi = acos(-1.0_sp)

    public :: uniform_sphere_vector
    public :: cosine_sphere_vector
    public :: lerpv
contains
    function uniform_sphere_vector() result(vector)
        !TODO: Col·loca en un mòdul a part...
        real(sp) :: vector(3)
        real(sp) :: phi

        call random_number(vector(1))
        call random_number(phi)

        phi = phi * 2 * pi

        vector(1) = 2.0_sp * (.5_sp - vector(1))
        vector(2) = sqrt(1.0_sp - vector(1) * vector(1)) * cos(phi)
        vector(3) = sqrt(1.0_sp - vector(1) * vector(1)) * sin(phi)
    end function uniform_sphere_vector

    function cosine_sphere_vector() result(vector)
        real(sp) :: vector(3)
        real(sp) :: phi, theta
        real(sp) :: u

        call random_number(u)
        call random_number(phi)
        phi = phi * 2 * pi
        theta = acos(sqrt(u))

        vector(1) = cos(phi) * sin(theta)
        vector(2) = sin(phi) * sin(theta)
        vector(3) = cos(theta)
    end function cosine_sphere_vector

    pure function lerpv(v1, v2, t) result(v)
        ! Linear interpolation function for a 3-vector
        real(sp), intent(in) :: v1(3), v2(3), t
        real(sp) :: v(3)
        
        v = v1 + t * (v2 - v1)
    end function lerpv

end module misc
