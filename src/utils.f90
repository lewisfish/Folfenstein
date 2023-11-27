module utils

    use, intrinsic :: iso_fortran_env, only : real64

    implicit none

    type :: pair
        real(kind=real64) :: first
        integer :: second
    end type pair

    interface operator(<)
        module procedure lt_pair
    end interface

    private
    public :: pair, sort_pairs

contains
    
    logical function lt_pair(a, b)

        type(pair), intent(in) :: a, b

            lt_pair = .false.
            if(a%first < b%first)lt_pair = .true.

    end function lt_pair

    subroutine sort_pairs(pairs)
    !! sort pair type based upon first
    !! isort3 J. Bentley 2000
    !! https://books.google.com/books?id=kse_7qbWbjsC&pg=PA116
        type(pair) :: pairs(:)

        integer :: i, j
        type(pair) :: t

        do i = 1, size(pairs)
            t = pairs(i)
            do j = i, 2,-1
                if(pairs(j-1) < t)exit
                pairs(j) = pairs(j-1)
            end do
            pairs(j) = t
        end do
    end subroutine sort_pairs
end module utils