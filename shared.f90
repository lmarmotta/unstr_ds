module shared

    implicit none 


    !
    ! BASIC DATASTRUCTURES
    !

    ! nnode: Number of nodes in an element.
    integer(kind=4) :: nnode = 4

    ! nelem: Number of elements in a mesh.
    integer(kind=4) :: nelem

    ! npoin: Number of points in a mesh.
    integer(kind=4) :: npoin

    ! nghos: Number of boundary faces.
    integer(kind=4) :: nghos

    ! Connectivit matrix inpoel(1:nnode, 1:nelem).
    real(kind=8), allocatable, dimension(:,:) :: inpoel

    ! Coordinate points coord(1:ndimn, 1:npoin).
    real(kind=8), allocatable, dimension(:,:) :: coord


    !
    ! FACE-BASED DATA STRUCTURES CREATION
    !

    ! Faces vector.
    ! face(i,1): Vertex number one of the face.
    ! face(i,2): Vertex number two of the face.
    ! face(i,3): Element to the left.
    ! face(i,4): Element to the right.
    integer(kind=4), allocatable, dimension(:,:) :: face

    ! Ghost vector (Read).
    integer(kind=4), allocatable, dimension(:,:) :: ghost

    ! Ghost vector (use).
    integer(kind=4), allocatable, dimension(:,:) :: ighost


    contains

        !
        ! PUBLIC FUNCTIONS
        !

        ! Hash function #1: Summation of the two values.
        !
        integer(kind=4) function hash_a(val_1, val_2)

            implicit none

            integer(kind=4) val_1, val_2

            hash_a = val_1 + val_2 

        end function hash_a

        ! Hash function #2: Andre's proposal.
        !
        integer(kind=4) function hash_b(val_1, val_2)

            implicit none

            integer(kind=4) val_1, val_2
            integer(kind=4) x, y, n, t

            ! Order the nodes.
            if (val_1 < val_2) then
                x = val_1
                y = val_2
            else
                x = val_2
                y = val_1
            end if

            ! Do the hash itself.
            n = x + y

            if ( mod(n,2) == 0) then
                t = (n/2) * (n+1)
            else
                t = n * ( (n+1) / 2 )
            end if

            hash_b = t + x

        end function hash_b

        ! Find function: Searches for a occurence in a vector.
        !
        integer(kind=4) function find(val, vector)

            implicit none

            integer(kind=4) :: vector(:)
            integer(kind=4) :: val

            integer(kind=4) :: idx, i

            do i = 1, size(vector)

                idx = vector(i) - val

                if (idx == 0) then
                    find = i
                    exit
                else
                    find = -1
                end if

            end do

        end function find

end module shared
