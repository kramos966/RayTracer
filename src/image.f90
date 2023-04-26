module image
    use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, int32, int64
    use, intrinsic :: iso_c_binding, only: c_char, c_int8_t
    implicit none

    private

    interface image_write
        module procedure image_write_r32
        module procedure image_write_r64
    end interface

    character(kind=c_char, len=*), parameter :: header = "P6"
    character(kind=c_char), parameter :: cr = char(10)
    character(kind=c_char), parameter :: whitespace = char(32)
    character(kind=c_char, len=3), parameter :: maxbit = "255"

    public :: image_write

contains

    subroutine image_write_r32(fname, array)
        ! Save an image to a file
        character(*), intent(in) :: fname
        real(sp), intent(in) :: array(:, :, :)
        integer(kind=c_int8_t), parameter :: mold = 0

        integer(kind=c_int8_t), allocatable :: rgb_image(:, :, :)
        character(kind=c_char, len=16) :: s_width, s_height
        integer :: dims(3), file_unit, i, j, funit
        integer(c_int8_t) :: r, g, b

        dims = shape(array)
        allocate(rgb_image(dims(3), dims(2), dims(1)))
        ! Primerament, convertim l'array en RGB
        do j = 1, dims(1)
            do i = 1, dims(2)
                r = int(array(j, i, 1) * 255, kind=c_int8_t)
                g = int(array(j, i, 2) * 255, kind=c_int8_t)
                b = int(array(j, i, 3) * 255, kind=c_int8_t)
                rgb_image(:, i, j) = [transfer(r, mold), transfer(g, mold), transfer(b, mold)]
            end do
        end do

        ! Obrim un arxiu i escrivim el resultat
        open(file=fname, newunit=funit, action="write", access="stream")
        ! Escrivim la capçalera del PPM
        write(s_width, "(I16)") dims(2)
        write(s_height, "(I16)") dims(1)
        s_width = trim(adjustl(s_width))
        s_height = trim(adjustl(s_height))
        write(funit) header, cr, s_width, whitespace, s_height, whitespace, &
                     maxbit, cr
        do j = 1, dims(1)
            do i = 1, dims(2)
                write(funit) rgb_image(:, i, j)
            end do
        end do

        close(funit)
    end subroutine image_write_r32

    subroutine image_write_r64(fname, array)
        character(*), intent(in) :: fname
        real(dp), intent(in) :: array(:, :, :)
        character(kind=c_char), parameter :: mold = char(0)

        character(kind=c_char), allocatable :: rgb_image(:, :, :)
        character(kind=c_char, len=16) :: s_width, s_height
        integer :: dims(3), file_unit, i, j, r, g, b, funit

        dims = shape(array)
        allocate(rgb_image(dims(3), dims(2), dims(1)))
        ! Primerament, convertim l'array en RGB
        do j = 1, dims(1)
            do i = 1, dims(2)
                r = int(array(j, i, 1) * 255, kind=int32)
                g = int(array(j, i, 2) * 255, kind=int32)
                b = int(array(j, i, 3) * 255, kind=int32)
                rgb_image(:, i, j) = [transfer(r, mold), transfer(g, mold), transfer(b, mold)]
            end do
        end do

        ! Obrim un arxiu i escrivim el resultat
        open(file=fname, newunit=funit, action="write", access="stream")
        ! Escrivim la capçalera del PPM
        write(s_width, "(I16)") dims(2)
        write(s_height, "(I16)") dims(1)
        s_width = trim(adjustl(s_width))
        s_height = trim(adjustl(s_height))
        write(funit) header, cr, s_width, whitespace, s_height, whitespace, &
                     maxbit, cr
        do i = 1, dims(2)
            do j = 1, dims(1)
                write(funit) rgb_image(:, i, j)
            end do
        end do

        close(funit)
    end subroutine image_write_r64

end module image
