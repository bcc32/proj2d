!!!! proj2d.f03
!!!! 2D projectile motion calculator
!!!! Aaron Zeng <a2z@mit.edu>

program proj2d
    implicit none
    real, parameter :: g = 9.80665      ! from 2010 CODATA reommended values
    real, parameter :: pi = 4 * atan(1.0) ! calculate pi by trigonometry

    call menu
    ! call system('pause')

contains
    subroutine p2r(v, theta, v_x, v_y)
        !! convert polar to rectangular
        implicit none
        real, intent(in)    :: v, theta ! velocity, angle [rad]
        real, intent(out)   :: v_x, v_y ! x, y velocity
        v_x = v * cos(theta)
        v_y = v * sin(theta)
    end subroutine p2r

    subroutine r2p(v_x, v_y, v, theta)
        !! convert rectangular to polar
        implicit none
        real, intent(in)    :: v_x, v_y ! x, y velocity
        real, intent(out)   :: v, theta ! velocity, angle [rad]
        v = sqrt(v_x ** 2 + v_y ** 2)
        theta = atan2(v_y, v_x)
    end subroutine r2p

    recursive subroutine menu
        !! menu for selecting functions
        implicit none
        character(80) :: selection

        print *, 'Enter a selection from the menu below:'
        print *, 'range: Calculate the range of a projectile above ground.'
        print *, 'trace: Trace the trajectory of a projectile under gravity.'
        read (*, *) selection

        select case (selection)
            case ('range')
                call ranger
            case ('trace')
                call trace
            case default
                print *, 'Not an option.  Try again.'
                call menu
        end select
    end subroutine menu

    recursive subroutine ranger
        !! calculate the range of a projectile above ground
        implicit none

        !! user input
        real :: v, theta                ! velocity [m/s], angle [rad]
        real :: dist                    ! range of the projectile [m]

        print *, 'Enter velocity (m/s), angle (rad)'
        read (*, *) v, theta

        if (modulo(theta, 2 * pi).gt.pi) then
            print *, 'Invalid angle.  Try again.'
            call ranger
        end if

        dist = (v ** 2.0 * sin(2 * theta)) / g

        print *, 'Distance: ', dist, 'm'
    end subroutine ranger

    subroutine trace
        !! trace the trajectory of a projectile under simplistic gravitation
        implicit none

        !! variables
        real, allocatable :: x(:), y(:) ! x, y position [m]
        real, allocatable :: time(:)    ! time [s]
        real :: v_x                     ! x velocity [m/s]
        real, allocatable :: v_y(:)     ! y velocity [m/s]
        integer :: err                  ! error message for allocation
        integer :: step                 ! step counter
        !! user input
        real :: v, theta                ! velocity [m/s], angle [rad]
        real :: time_limit              ! final time
        integer :: steps                ! number of steps
        print *, 'Enter velocity (m/s), angle (rad), time (s), steps'
        read (*, *) v, theta, time_limit, steps

        allocate(x(0:steps), y(0:steps), time(0:steps), v_y(0:steps), stat=err)
        if (err.ne.0) then
            print *, "x, y, time, v_y: Allocation request denied"
            return
        end if

        call p2r(v, theta, v_x, v_y(1))

        do step = 0, steps
            time(step) = time_limit * step / steps
        end do

        x = v_x * time
        y = v_y(1) * time - 0.5 * g * time ** 2.0
        v_y = v_y(1) - g * time

        write (*, '(5a15)') 't', 'x', 'y', 'v_x', 'v_y'

        do step = 0, steps
            write (*, '(5f15.6)') time(step), x(step), y(step), v_x, v_y(step)
        end do

        deallocate(x, y, time, v_y, stat=err)
        if (err.ne.0) print *, "array: Deallocation request denied"
    end subroutine trace
end program proj2d
