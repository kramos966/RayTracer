program main
    use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64
    use :: optical_elements, only: Sphere, new_sphere, Scene
    use :: raytracer, only: Camera, new_camera
    use :: image, only: image_write
    implicit none

    real(sp), parameter :: pi = 4.0_sp * atan(1.0_sp)
    integer, parameter :: max_bounces = 10
    integer, parameter :: iterations = 256

    type(Scene) :: escena
    type(Sphere) :: esfera, e2, e3, e4, e5, e6
    type(Camera) :: main_camera
    integer :: width = 1920
    integer :: height = 1080

    real(sp) :: pos(3), n, radius, color(3), direction(3), aspect_ratio, fov, focal

    n = 1.5_sp
    radius = 800.0_sp
    pos = [00.0_sp, radius + 30.0_sp, 0.0_sp]
    color = [0.0_sp, 0.0_sp, 0.0_sp]
    esfera = new_sphere(pos, n, radius, color)
    esfera%emissivity = 2.0_sp
    esfera%e_color = 1.0_sp
    call escena%add_entity(esfera)

    e2 = new_sphere(pos, n, radius, color)
    e2%radius = 1000.0_sp
    e2%pos = [0.0_sp, -e2%radius, 0.0_sp]
    e2%color = [0.3_sp, 0.3_sp, 0.3_sp]
    e2%emissivity = 0.0_sp
    e2%e_color = 0.0_sp
    call escena%add_entity(e2)

    e3 = new_sphere(pos, n, radius, color)
    e3%radius = 10.0_sp
    e3%color = [1.0_sp, 1.0_sp, 1.0_sp]
    e3%pos = [-3*e3%radius, e3%radius, 0.0_sp]
    !e3%pos = [3*e3%radius, e3%radius, -20.0_sp]
    e3%emissivity = 0.0
    e3%roughness = 0.2
    e3%transmittance = 1.0_sp
    e3%n = 1.5
    call escena%add_entity(e3)

    e4 = new_sphere(pos, n, radius, color)
    e4%radius = 10.0_sp
    e4%pos = [3*e4%radius, e4%radius, 0.0_sp]
    e4%color =  [1.0_sp, 1.0_sp, 0.0_sp]
    e4%emissivity = 0.0
    e4%roughness = 0.8
    call escena%add_entity(e4)

    e5 = new_sphere(pos, n, radius, color)
    e5%radius = 10.0_sp
    e5%pos = [0.0_sp, e4%radius, 0.0_sp]
    e5%color = [1.0_sp, 1.0_sp, 1.0_sp]
    e5%emissivity = 0.0_sp
    e5%roughness = 0.03_sp
    call escena%add_entity(e5)

    e6%radius = 5.0_sp
    e6%pos = [-30.0_sp, 10.0_sp, 20.0_sp]
    e6%color = [0.0_sp, 1.0_sp, 0.3_sp]
    e6%emissivity = 0.0_sp
    e6%roughness = 1.00_sp
    e6%transmittance = 0.0_sp
    call escena%add_entity(e6)

    pos(3) = .0_sp
    pos(2) = 20.0
    direction = [0.0_sp, 0.0_sp, 1.0_sp] 
    aspect_ratio = 16.0_sp / 9.0_sp
    fov = 45.0_sp 
    focal = 50.0_sp
    main_camera = new_camera(pos, direction, aspect_ratio, width, height, focal, fov)

    call main_camera%render(escena, max_bounces, iterations)
    call main_camera%save_render("escena.ppm")

end program main
