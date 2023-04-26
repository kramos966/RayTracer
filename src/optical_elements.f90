module optical_elements
    use, intrinsic :: iso_fortran_env, only: dp=>real64, sp=>real32
    use :: misc, only: cosine_sphere_vector, lerpv
    implicit none
    private

    real(sp), parameter :: eps = 1e-4
    real(sp), parameter :: pi = acos(-1.0_sp)

    ! Definició de Raig
    type, public :: Ray
        real(sp) :: pos(3)
        real(sp) :: p(3)
        real(sp) :: direction(3)
        real(sp) :: n = 1.0_sp
    contains
        procedure :: propagate => Ray_propagate
        procedure :: reflect => Ray_reflect
        procedure :: diffuse_reflect => Ray_diffuse_reflect
        procedure :: diffuse_refract => Ray_diffuse_refract
        procedure :: physical_interaction => Ray_physical_interaction
    end type Ray

    ! Definició d'Entitat, classe abstracta
    type, public, abstract :: Entity
        real(sp) :: pos(3)
        real(sp) :: n
        real(sp) :: orient(3)
        real(sp) :: color(3)
        real(sp) :: e_color(3) = [0.0_sp, 0.0_sp, 0.0_sp]
        real(sp) :: emissivity = 0.0_sp
        real(sp) :: roughness = 1.0_sp
        real(sp) :: transmittance = 0.0_sp
    contains
        procedure (Entity_intersect), deferred :: intersect
        procedure (Entity_refract), deferred :: refract
        procedure (Entity_get_normal), deferred :: normal
    end type Entity

    abstract interface
        pure function Entity_intersect(this, light_ray)
            import :: Entity, Ray
            class(Entity), intent(in) :: this
            class(Ray), intent(in) :: light_ray
        end function Entity_intersect

        pure function Entity_get_normal(this, point) result(normal)
            use, intrinsic :: iso_fortran_env, only: sp => real32
            import :: Entity
            class(Entity), intent(in) :: this
            real(sp), intent(in) :: point(3)
            real(sp) :: normal(3)
        end function Entity_get_normal

        subroutine Entity_refract(this, light_ray)
            import :: Entity, Ray
            class(Entity), intent(in) :: this
            class(Ray), intent(inout) :: light_ray
        end subroutine Entity_refract
    end interface

    ! ESFERA
    type, public, extends(Entity) :: Sphere 
        real(sp) :: radius
    contains
        procedure :: intersect => Sphere_intersect
        procedure :: refract => Sphere_refract
        procedure :: normal => Sphere_get_normal
    end type Sphere
    public :: new_sphere, new_ray

    ! Col·lecció d'entitats, una escena!
    type :: Entity_holder
        class(Entity), allocatable :: ent
    end type Entity_holder

    type, public :: Scene
        class(Entity_holder), pointer :: entity_arr(:) => null()
        integer :: n_entities = 0
    contains
        procedure :: add_entity => scene_add_entity
    end type Scene

contains
    ! Ray PROCEDURES
    type(Ray) function new_ray(r, direction, n)
        real(sp), dimension(3), intent(in) :: r, direction
        real(sp), intent(in) :: n
        new_ray%pos = r
        new_ray%p = direction *  n
        new_ray%direction = direction
        new_ray%n = n
    end function new_ray

    pure subroutine Ray_propagate(this, z)
        class(Ray), intent(inout) :: this
        real(sp), intent(in) :: z
        this%pos = this%pos + this%direction * z
    end subroutine Ray_propagate

    pure subroutine Ray_reflect(this, ent)
        class(Ray), intent(inout) :: this
        class(Entity), intent(in) :: ent

            real(sp) :: normal(3)

        ! Obtenim la normal de la superfície al punt
        normal = ent%normal(this%pos)

        this%direction = this%direction - 2.0_sp * dot_product(normal, this%direction) * normal

        ! CORRECCIÓ D'ERRORS
        ! Desplacem el raig una quantitat infinitesimal segons la normal per tal que 
        ! no hi hagi dos talls seguits amb la mateixa superfície.
        this%pos = this%pos + eps * normal
    end subroutine Ray_reflect

    subroutine Ray_diffuse_reflect(this, ent, normal)
        ! Reflexió difusa d'un raig respecte l'entitat ent
        class(Ray), intent(inout) :: this
        class(Entity), intent(in) :: ent
        real(sp), intent(inout) :: normal(3)

        ! La direcció final serà una interpolació lineal entre aquest vector i el
        ! de la reflexió directa.
        this%direction = this%direction - 2.0_sp * dot_product(normal, this%direction) * normal
        this%pos = this%pos + eps * normal
    end subroutine Ray_diffuse_reflect

    subroutine Ray_diffuse_refract(this, ent, normal)
        class(Ray), intent(inout) :: this
        class(Entity), intent(in) :: ent
        real(sp), intent(inout) :: normal(3)

        real(sp) :: dot, discriminant, mu

        dot = dot_product(normal, this%direction)
        if (dot < 0.0_sp) then
            ! Fora del medi
            mu = this%n / ent%n
            dot = -dot
            normal = -normal
        else
            ! Dins el medi
            mu = ent%n / this%n
        end if
        discriminant = 1.0_sp - mu * mu * (1.0 - dot * dot)
        if (discriminant < 0.0_sp) then
            ! Cas reflexió total interna
            dot = -dot
            normal = -normal
            this%direction = this%direction - 2.0_sp * dot * normal
        else
            ! Cas refracció
            this%direction = mu * this%direction - normal * (mu * dot - sqrt(discriminant))
        end if
        ! Roughness

        this%pos = this%pos + eps * normal

    end subroutine Ray_diffuse_refract

    subroutine Ray_physical_interaction(this, ent)
        class(ray), intent(inout) :: this
        class(Entity), intent(in) :: ent

        real(sp) :: reflection_probability, diffuse(3), normal(3)

        normal = ent%normal(this%pos)

        call random_number(reflection_probability)

        if (reflection_probability > ent%transmittance) then
            call this%diffuse_reflect(ent, normal)
        else
            call this%diffuse_refract(ent, normal)
        end if
        ! Difusivitat
        diffuse = this%direction + cosine_sphere_vector()
        diffuse = diffuse / sqrt(dot_product(diffuse, diffuse))

        if (dot_product(diffuse, normal) < 0.0_sp) then
            diffuse = -diffuse
        end if
        this%direction = lerpv(this%direction, diffuse, ent%roughness)
        ! Renormalitzem per a reduir errors
        this%direction = this%direction / sqrt(dot_product(this%direction, this%direction))
    end subroutine Ray_physical_interaction

    ! Shere PROCEDURES
    type(Sphere) function new_sphere(pos, n, radius, color)
        real(sp), intent(in) :: pos(3), n, radius, color(3)
        new_sphere%pos = pos
        new_sphere%n = n
        new_sphere%orient = [0.0_sp, 0.0_sp, 0.0_sp]
        new_sphere%radius = radius
        new_sphere%color = color
    end function new_sphere

    pure function Sphere_intersect(this, light_ray) result(d)
        ! Retorna el punt més proper d'intersecció d'un raig amb una esfera.
        ! En cas que no hi hagi intersecció, retornem un valor negatiu. Si
        ! la intersecció es produeix en sentit contrari, el valor retornat és
        ! també negatiu.
        class(Sphere), intent(in) :: this
        class(Ray), intent(in) :: light_ray
        real(sp) :: d

        real(sp) :: center_diff(3), dot_prod, discriminant, d1, d2

        ! Calculem el discriminant, per a saber si hi ha tall
        center_diff = light_ray%pos - this%pos
        dot_prod = dot_product(light_ray%direction, center_diff)
        discriminant = dot_prod * dot_prod - &
                       dot_product(center_diff, center_diff) + this%radius * this%radius
        ! No hi ha intersecció, retornem un nombre extremadament negatiu
        if (discriminant < 0) then
            d = -huge(1.0_sp)
            return
        end if

        d1 = -dot_prod + sqrt(discriminant)
        d2 = -dot_prod - sqrt(discriminant)

        ! Retornem el valor positiu més petit
        d = min(d1, d2)
        ! Si aquest fos negatiu, intentem tornar el més gran
        if (d < eps) then
            d = max(d1, d2)
        end if
    end function Sphere_intersect

    subroutine Sphere_refract(this, light_ray)
        class(Sphere), intent(in) :: this
        class(Ray), intent(inout) :: light_ray
    end subroutine Sphere_refract

    pure function Sphere_get_normal(this, point) result(normal)
        class(Sphere), intent(in) :: this
        real(sp), intent(in) :: point(3)
        real(sp) :: normal(3)

        normal = point - this%pos
        normal = normal / sqrt(dot_product(normal, normal))
    end function Sphere_get_normal

    subroutine scene_add_entity(this, new_ent)
        class(Scene), intent(inout) :: this
        class(Entity), intent(in) :: new_ent

        type(Entity_holder), pointer :: temp(:)
        integer :: i

        ! Si no hi ha elements, n'afegim el primer
        if (.not. associated(this%entity_arr)) then
            ! TODO: No FUNCIONA
            allocate(this%entity_arr(1))
            allocate(this%entity_arr(1)%ent, mold=new_ent)
            this%entity_arr(1)%ent = new_ent
            this%n_entities = 1
            return
        end if
        
        !TODO: Només faig shallow copies després del primer allocate
        allocate(temp(this%n_entities + 1))
        allocate(temp(this%n_entities + 1)%ent, mold=new_ent)
        do concurrent(i = 1:this%n_entities)
            temp(i) = this%entity_arr(i)
            temp(i)%ent = this%entity_arr(i)%ent
        end do
        temp(this%n_entities + 1)%ent = new_ent
        this%n_entities = this%n_entities + 1
        this%entity_arr => temp
        !call move_alloc(this%entity_arr, temp)

    end subroutine scene_add_entity
    
end module optical_elements
