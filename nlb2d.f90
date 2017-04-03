! New leo's Bru 2D code (nlb2d). !
! Author: Leonardo Motta
! Contributions: Edson, Andre.

! TO DO: Get a better way to tecplot all.

program nlb2d

    use shared
    implicit none

    integer(kind=8) :: c1,c2,cr
    real(kind=8) :: rate
    real(kind=8) :: elapse_time

    write(*,'(A)') ""
    write(*,'(A)') "-----------------------------------------------------------"
    write(*,'(A)') "              ++ Euler Unstructured Code ++                "
    write(*,'(A)') "-----------------------------------------------------------"

    ! ----------- Time ------------- 
    call system_clock(count_rate=cr)
    rate = real(cr)
    elapse_time = 0.0d0
    ! ------------------------------

    !
    ! PRE-PROCESSING...
    !

    write(*,'(A)') ""
    write(*,'(A)') "-----------------------------------------------------------"
    write(*,'(A)') " + PERFORMING PRE-PROCESSING."
    write(*,'(A)') ""
    
    call system_clock(c1)

    call indat

    call basic_ds

    call hc_faces

    call system_clock(c2)

    elapse_time = real(c2-c1,kind=8)/rate


    write(*,*) ""
    write(*,'(A,F10.6,A)') " + Elapse time: ", elapse_time, " [s]"
    write(*,'(A)') "-----------------------------------------------------------"

    !
    ! BUILDING DERIVED DATA...
    !

    ! ----------- Time ------------- 
    call system_clock(count_rate=cr)
    rate = real(cr)
    elapse_time = 0.0d0
    ! ------------------------------

    write(*,'(A)') ""
    write(*,'(A)') "-----------------------------------------------------------"
    write(*,'(A)') " + BUILDING DERIVED DATA FOR FVM METHOD."
    write(*,'(A)') ""

    call system_clock(c1)

    call init
    call calc_cell_area
    call initial_condition

    call system_clock(c2)

    elapse_time = real(c2-c1,kind=8)/rate

    write(*,*) ""
    write(*,'(A,F10.6,A)') " + Elapse time: ", elapse_time, " [s]"
    write(*,'(A)') "-----------------------------------------------------------"

    !
    ! OUTPUT DATA.
    !

    ! ----------- Time ------------- 
    call system_clock(count_rate=cr)
    rate = real(cr)
    elapse_time = 0.0d0
    ! ------------------------------

    write(*,'(A)') ""
    write(*,'(A)') "-----------------------------------------------------------"
    write(*,'(A)') " + OUTPUTING SOLUTION TO TECPLOT."
    write(*,'(A)') ""

    call system_clock(c1)

    call tecplot

    call system_clock(c2)

    elapse_time = real(c2-c1,kind=8)/rate

    write(*,*) ""
    write(*,'(A,F10.6,A)') " + Elapse time: ", elapse_time, " [s]"
    write(*,'(A)') "-----------------------------------------------------------"

end program nlb2d
