module raytracer
    use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64
    use :: optical_elements, only: Ray, Sphere, new_ray, new_sphere, Scene, Entity
    use :: image, only: image_write

    implicit none

    real(sp), parameter :: pi = 4.0 * atan(1.0_sp)
    real(sp), parameter :: inf = huge(1.0_sp)

    type, public :: Camera
        real(sp), private :: pos(3)
        real(sp), private :: direction(3)
        real(sp), private :: aspect_ratio
        integer, private :: width
        integer, private :: height
        real(sp), private :: focal
        real(sp), private :: fov
        real(sp), private :: lx
        real(sp), private :: ly
        real(sp), allocatable, private :: image(:, :, :)
    contains
        procedure :: trace_scene => camera_trace_scene
        procedure :: render => camera_render
        procedure :: save_render => camera_image_save
    end type Camera

contains

    type(Camera) pure function new_camera(pos, direction, aspect_ratio, width, height, focal, fov) 
        ! Camera constructor
        real(sp), intent(in) :: pos(3), direction(3), focal, fov, aspect_ratio
        integer, intent(in) :: width, height

        new_camera%pos = pos
        new_camera%direction = direction
        new_camera%aspect_ratio = aspect_ratio
        new_camera%width = width
        new_camera%height = height
        new_camera%focal = focal
        new_camera%fov = fov

        allocate(new_camera%image(height, width, 3))
        new_camera%image = 0.0_sp   ! Iniciem l'array a zero

        ! Calculem també les dimensions físiques de la pantalla...
        new_camera%lx = focal * tan(fov * pi / 180.0_sp)
        new_camera%ly = new_camera%lx / aspect_ratio
    end function new_camera

    subroutine camera_trace_scene(this, scn, max_bounces)
        class(Camera), intent(inout) :: this
        class(Scene), intent(in) :: scn
        integer, intent(in) :: max_bounces

        integer :: i, j, k, n_entities(1), id
        real(sp) :: x, y, delta_x, delta_y, d, pos(3), direction(3)
        type(Ray) :: light_ray
        real(sp) :: d_min, n, color(3)

        delta_x = 2.0_sp * this%lx / this%width
        delta_y = 2.0_sp * this%ly / this%height

        ! Definim un raig, que utilitzarem en el bucle
        n = 1.0_sp
        direction = [0.0_sp, 0.0_sp, 1.0_sp]
        pos = [0.0_sp, 0.0_sp, -this%focal]     ! Tots els rajos surten a -f de la pantalla de la càmera
        light_ray = new_ray(pos, direction, n)

        !$omp parallel do private(y, x, direction, light_ray, color)
        do i = 1, this%width
            x = -this%lx + delta_x * (i - 1)
            do j = 1, this%height
                y = -this%ly + delta_y * (j - 1)
                ! Intersecta amb cada objecte de l'escena i pinta amb el color del
                ! punt més proper
                direction = [x, y, this%focal]
                direction = direction / sqrt(dot_product(direction, direction))
                light_ray%pos = [0.0_sp, 0.0_sp, -this%focal] + this%pos
                light_ray%direction = direction
                light_ray%n = 1.0_sp
                ! WORK FUNCTION
                call ray_bounce(light_ray, scn, max_bounces, color)
                !$omp atomic
                this%image(this%height - j + 1, i, 1) = this%image(this%height -j + 1, i, 1) +&
                                                        color(1)
                !$omp atomic
                this%image(this%height - j + 1, i, 2) = this%image(this%height -j + 1, i, 2) +&
                                                        color(2)
                !$omp atomic
                this%image(this%height - j + 1, i, 3) = this%image(this%height -j + 1, i, 3) +&
                                                        color(3)
            end do
        end do
        !$omp end parallel do
    end subroutine camera_trace_scene

    subroutine camera_render(this, scn, max_bounces, iterations)
        class(Camera), intent(inout) :: this
        class(Scene), intent(in) :: scn
        integer, intent(in) :: max_bounces
        integer, intent(in) :: iterations

        integer :: i, j, k
        do i = 1, iterations
            call this%trace_scene(scn, max_bounces)
        end do
        this%image = this%image / real(iterations, kind=sp)
        ! Clipping to 1.0_sp
        do concurrent(i = 1:this%width)
            do concurrent(j = 1:this%height)
                do concurrent(k = 1:3)
                    this%image(j, i, k) = min(1.0_sp, this%image(j, i, k))
                end do 
            end do
       end do
        !this%image = this%image / maxval(this%image)
   end subroutine camera_render

   subroutine camera_image_save(this, fname)
       class(Camera), intent(in) :: this
       character(*), intent(in) :: fname

       call image_write(fname, this%image)
   end subroutine camera_image_save

   subroutine ray_bounce(light_ray, scn, max_bounces, color)
       class(Ray), intent(inout) :: light_ray
       class(scene), intent(in) :: scn
       integer, intent(in) :: max_bounces
       real(sp), intent(out) :: color(3)

       real(sp) :: d_min, d, d_total, ray_color(3), emitted(3)
       integer :: idx, k, l

       ray_color = [1.0_sp, 1.0_sp, 1.0_sp]
       color = [0.0_sp, 0.0_sp, 0.0_sp]

       d_total = 0.0_sp

       do l = 1, max_bounces
            d_min = inf
            idx = -1
            do k = 1, scn%n_entities
                ! Propaguem el raig amb cada entitat per a veure si hi ha tall
                d = scn%entity_arr(k)%ent%intersect(light_ray)
                if (d < 0.0_sp) cycle   ! Si no n'hi ha, passem al següent
                ! Guardem la distància de l'objecte intersectat més proper i el seu índex
                if (d < d_min) then
                    d_min = d
                    idx = k
                end if
            end do

            ! Si no hi ha hagut tall amb cap entitat, sortim del bucle
            if (idx < 0) then
                exit
            end if

            ! En cas contrari, propaguem el raig i reflectim només, de moment. Propaguem
            ! i multipliquem pel color de la superfície intersectada
            call light_ray%propagate(d_min)
            ! TODO: Millor forma de decidir si el raig és transmès o reflectit...
            call light_ray%physical_interaction(scn%entity_arr(idx)%ent)
            !call light_ray%reflect(scn%entity_arr(idx)%ent)
            d_total = d_total + d_min
            emitted = scn%entity_arr(idx)%ent%e_color * scn%entity_arr(idx)%ent%emissivity
            color = color + ray_color * emitted
            ray_color = ray_color * scn%entity_arr(idx)%ent%color
       end do
        ! Clipping
        !TODO: Estudia una millor forma de sumar colors
        !color(1) = min(1.0, color(1))
        !color(2) = min(1.0, color(2))
        !color(3) = min(1.0, color(3))
   end subroutine ray_bounce


end module raytracer
