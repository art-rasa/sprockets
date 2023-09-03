! 
! sprockets
! 
! Bicycle gearing calculation program.
! 
! Input 
!   Number of gears for front/rear cassette.
!   Number of teeth for each of the gears.
! 
! Output: 
!   Gear ratio table showing all gear combinations.
!   Step size table between all gear combinations.
!   Velocity tables for various cadences.
!   Gearing characteristics (progression, range, min/avg/max step).
!  

! Constant values for calculations.
module myParameters
    implicit none
    public
    real, parameter :: PI = 4.0 * atan(1.0)
    integer, parameter :: CADENCE(*) = [40, 60, 80, 100, 120] ! rpm
    integer, parameter :: CRANK_L = 170 ! crank length in mm
    integer, parameter :: WHEEL_D = 700 ! wheel diameter in mm
end module


program main
    use myParameters, only: PI, CADENCE, CRANK_L, WHEEL_D
    implicit none
    
    integer, allocatable :: cogs_front(:), cogs_rear(:)
    real, allocatable :: step_front(:), step_rear(:)
    integer :: num_cogs
    
    print *, 'hello from project sprockets,'
    print *, 'a bicycle gearing calculator program. '
    
    call getNumInput(num_cogs, 'Enter number of chainrings [front]: ')
    allocate(cogs_front(1:num_cogs), source=0)
    
    call getNumInput(num_cogs, 'Enter number of sprockets [rear]: ')
    allocate(cogs_rear(1:num_cogs), source=0)
    
    call getCogset(cogs_front, ':: Chainrings [front] ::')
    call getCogset(cogs_rear, ':: Sprockets [rear] ::')
    
    if ( size(cogs_front) > 1 ) then
        allocate( step_front(1:size(cogs_front)-1), source=0.0 )
    end if
    
    if ( size(cogs_rear) > 1 ) then
        allocate( step_rear(1:size(cogs_rear)-1), source=0.0 )
    end if
    call computeGearSteps(cogs_front, step_front)
    call computeGearSteps(cogs_rear, step_rear)
    
    write(*, '(a)') ':: Gearing overview ::'
    call printGearTable(cogs_front, cogs_rear, step_front, step_rear)
    
    write(*, '(a)') ':: Gear step tables ::'
    call printStepTable(step_front, ':: Chainrings [front] ::')
    call printStepTable(step_rear, ':: Sprockets [rear] :: ')
    
    write(*, '(a)') ':: Gear ratio table ::'
    write(*, '(a)') '  F: Chainring [front], R: Sprocket [rear]'
    call printGearRatios(cogs_front, cogs_rear)
    
    write(*, '(a)') ':: Gear vs. cadence [rpm] vs. speed [km/h] table ::'
    call printSpeedTable(cogs_front, cogs_rear, CADENCE, CRANK_L, WHEEL_D)
contains

    ! Get a numeric value from the user and places it into "num". 
    ! Optional prompt is printed.
    subroutine getNumInput(num, str) 
        integer, intent(inout) :: num
        character(len=*), intent(in), optional :: str
        if (present(str)) then
            write (*,'(a)', advance='no') str
        end if
        read (*,fmt='(i8)') num
    end subroutine
    
    ! Ask the user to enter the number of teeth for each gear in a 
    ! cassette of 1 or more gears.
    subroutine getCogset(cogs, str)
        integer, intent(inout), allocatable :: cogs(:)
        character(len=*), intent(in), optional :: str
        integer :: i, n
        character(len=100) :: i_str
        if (present(str)) then
            write (*,'(a)') str
        end if
        do i = 1, size(cogs)
            write(i_str, '(i4)') i
            call getNumInput(n, ' Enter size for gear #' &
                             // trim(adjustl(i_str)) // ': ')
            cogs(i) = n
        end do
        print*, ''
    end subroutine
    
    ! Compute the average of an array of real values.
    function RealArrayAvg(values) result(avg)
        real, intent(in), allocatable :: values(:)
        real :: avg 
        avg = sum(values) / real(size(values))
    end function
    
    ! Print characteristic information related to gearing.
    subroutine printGearTable(cogs_f, cogs_r, step_f, step_r)
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
        integer, intent(in), allocatable :: cogs_f(:), cogs_r(:)
        real, intent(in), allocatable :: step_f(:), step_r(:)
        integer, parameter :: step_chain = 1, step_sproc = 2
        integer, parameter :: step_min = 1, step_avg = 2, step_max = 3
        character(len=*), parameter :: step_fmt = &
            '(a34,f0.1,'' / '',f0.1,'' / '',f0.1)'
        real :: min_ratio, max_ratio, prog, step(1:2,1:3), t
        
        max_ratio = real(maxval(cogs_r)) / real(minval(cogs_f))
        min_ratio = real(minval(cogs_r)) / real(maxval(cogs_f))
        prog = (max_ratio / min_ratio) * 100.0
        
        step(step_chain, step_max) = ieee_value(t, ieee_quiet_nan)
        step(step_chain, step_min) = ieee_value(t, ieee_quiet_nan)
        step(step_chain, step_avg) = ieee_value(t, ieee_quiet_nan)
        if (size(step_f) > 1) then
            step(step_chain, step_max) = maxval(step_f)
            step(step_chain, step_min) = minval(step_f)
            step(step_chain, step_avg) = RealArrayAvg(step_f)
        end if
        
        step(step_sproc, step_max) = ieee_value(t, ieee_quiet_nan)
        step(step_sproc, step_min) = ieee_value(t, ieee_quiet_nan)
        step(step_sproc, step_avg) = ieee_value(t, ieee_quiet_nan)
        if (size(step_r) > 1) then
            step(step_sproc,  step_max) = maxval(step_r)
            step(step_sproc,  step_min) = minval(step_r)
            step(step_sproc,  step_avg) = RealArrayAvg(step_r)
        end if
        
        print *, ''
        write(*, '(a34,a)') 'Chainrings [front]  ', getGearset(cogs_f)
        write(*, '(a34,a)') 'Sprockets [rear]  ', getGearset(cogs_r)
        write(*, '(a34,i0)') 'Theoretical # of gears  ', size(cogs_f) * size(cogs_r)
        write(*, '(a34,f0.2)') 'Max. gear ratio  ', max_ratio
        write(*, '(a34,f0.2)') 'Min. gear ratio  ', min_ratio
        write(*, '(a34,f0.1,'' %'')') 'Progression  ', prog
        write(*, fmt=step_fmt) 'Chainring step % [min/avg/max]  ', step(step_chain,:)
        write(*, fmt=step_fmt) 'Sprocket step % [min/avg/max]  ', step(step_sproc,:)
        print *, ''
    end subroutine
    
    ! Concatenate all gears of a cassette on one line of text.
    function getGearset(cogs) result(set_str)
        integer, intent(in), allocatable :: cogs(:)
        character(len=:), allocatable :: set_str
        integer :: i
        character(len=5) :: cog_str
        
        do i = 1, size(cogs)
            write(cog_str, '(i5)') cogs(i)
            set_str = set_str // trim(adjustl(cog_str))
            if (i < size(cogs)) then
                set_str = set_str // '-'
            end if
        end do
    end function
    
    ! Compute the relative differences between all adjacent gears 
    ! in a cassette.
    subroutine computeGearSteps(cogs, steps)
        integer, intent(in), allocatable :: cogs(:)
        real, intent(inout), allocatable :: steps(:)
        integer :: i, diff
        if ( (size(cogs) < 2) .or. .not. allocated(steps) ) then
            return
        end if 
        do i = 1, size(cogs) - 1
            diff = cogs(i) - cogs(i + 1)
            steps(i) = real(diff) / real(cogs(i)) * 100.0
        end do
    end subroutine
    
    ! Print a table showing the relative differences between all 
    ! adjacent gears in a cassette.
    subroutine printStepTable(steps, str)
        real, intent(in), allocatable :: steps(:)
        character(len=*), intent(in), optional :: str
        character(len=7) :: i_str
        character(len=:), allocatable :: line
        character(len=50) :: step_str
        integer :: i
        if (present(str)) then
            write (*,'(a)') str
        end if
        
        write(*, '(a)') ''
        if (.not. allocated(steps)) then
            write(*, '(a)') '    n/a (only one gear ratio)'
            write(*, '(a)') ''
            return
        end if
        
        write(*, '(a)', advance='no') '  '
        do i = 1, size(steps)
            write(i_str, '(i0,x,''-'',x,i0)') i, i + 1
            write(*, '(a)', advance='no') '  '
            write(*, '(a)', advance='no') adjustl(i_str) 
        end do
        write(*, '(a)') ''
        
        line = ''
        do i = 1, size(steps)
            write(step_str, '(f6.1,x,''%'')') steps(i)
            line = line // ' ' // trim(step_str)
        end do
        write(*, '(a)') line // ' '
        write(*, '(a)') ''
    end subroutine
    
    ! Calculate the gear ratio between drive and driven gears.
    function calcGearRatio(drive, driven) result(ratio)
        integer,intent(in) :: drive, driven
        real :: ratio
        ratio = real(driven) / real(drive)
    end function
    
!~     function getGainRatio(drive, driven, crlen, whldiam) result(ratio)
!~         integer, intent(in) :: drive, driven, crlen, whldiam
!~         real :: ratio, tmp
!~         tmp = real(crlen) / real(whldiam)
!~         ratio = tmp * getGearRatio(drive, driven)
!~     end function
    
    ! Calculate the velocity from cadence, wheel size and gear ratio.
    function calcVelocity(cad, whlcirc, ratio) result(vel_km_h)
        integer, intent(in) :: cad  
        real, intent(in) :: whlcirc, ratio
        real :: omega_whl, whlcirc_m, vel_m_h, vel_km_h
        whlcirc_m = real(whlcirc) / 1000.0
        omega_whl = real(cad) / ratio
        vel_m_h = (whlcirc_m * omega_whl) * 60.0 
        vel_km_h = vel_m_h / 1000.0
    end function
    
    ! Print velocity tables for all gear ratios at varying cadences.
    subroutine printSpeedTable(cogs_f, cogs_r, cad, crlen, whldiam)
        integer, intent(in), allocatable :: cogs_f(:), cogs_r(:)
        integer, intent(in) :: crlen, whldiam, cad(:)
        integer :: i, j, k
        real :: gratio, whlcirc, vel
        character(len=5) :: cad_str, chring_str, sprock_str, vel_str, &
                            i_str, j_str
        character(len=:), allocatable :: line
        whlcirc = PI * real(whldiam)
        
        ! header
        write(*, '(a)') ''
        write(*, '(a)') '                              cadence'
        line = 'chainring' // '      '  // 'sprocket '
        do i = 1, size(cad)
            write(cad_str, '(i5)') cad(i)
            line = line // '   ' // cad_str !// ' rpm'
        end do
        write(*, '(a)') line
        write(*, '(a)') repeat('-', len_trim(line))
        
        ! speed rows
        do i = 1, size(cogs_f) 
            do j = 1, size(cogs_r)
                write(i_str, '(i5)') i
                write(j_str, '(i5)') j
                write(chring_str, '(i5)') cogs_f(i)
                write(sprock_str, '(i5)') cogs_r(j)
                line = 'F #' // trim(adjustl(i_str)) // ' (' // trim(adjustl(chring_str)) // 'T)'
                line = line // '     ' // 'R #' // trim(adjustl(j_str)) // ' (' // trim(adjustl(sprock_str)) // 'T)'
                gratio = calcGearRatio( cogs_f(i), cogs_r(j) )
                do k = 1, size(cad)
                    vel = calcVelocity( cad(k), whlcirc, gratio )
                    write(vel_str, '(f5.1)') vel
                    line = line // '   ' // vel_str !// ' km/h'
                end do
                write(*, '(a)') line
            end do
            write(*, '(a)') repeat('-', len_trim(line))
        end do
        write(*, '(a)') ''
    end subroutine
    
    ! Print a table of all possible gear ratios.
    subroutine printGearRatios(cogs_f, cogs_r)
        integer, intent(in), allocatable :: cogs_f(:), cogs_r(:)
        integer :: i, j
        real :: ratio
        character(len=50) :: cogs_f_str, cogs_r_str, ratio_str, i_str 
        character(len=2) :: j_str
        character(len=:), allocatable :: line
        
        write(*, '(a)') ''
        write(*, '(5x,a,1x)', advance='no') ' '
        do j = 1, size(cogs_r)
            write(j_str, '(i2)') j
            write(*, '(a)', advance='no') ' R #' // adjustl(j_str) // '  '
        end do
        write(*, '(a)') ''
        
        do i = 1, size(cogs_f)
            write(i_str, '(i3)') i
            line = 'F #' // trim(adjustl(i_str))
            
            do j = 1, size(cogs_r)
                ratio = calcGearRatio(cogs_f(i), cogs_r(j))
                write(ratio_str, '(f5.2)') ratio
                line = line // '   ' // trim(ratio_str)
            end do
            write(*, '(a)') line // '  '
        end do
        
        write(*, '(a)', advance='no') repeat(' ', 6)
        write(*, '(a)') repeat( repeat(' ', 8), size(cogs_r) )
    end subroutine
    
    ! Print optimal gear shifts
    ! TODO
    subroutine printShifts(cogs_f, cogs_r, strategy)
        integer, intent(in), allocatable :: cogs_f(:), cogs_r(:)
        character(len=1), intent(in), optional :: strategy

        
    end subroutine
end program main

