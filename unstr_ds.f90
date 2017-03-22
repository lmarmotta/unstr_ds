! This program ains to teach some basic unstrctured code datastructures.
!
! Author: Leonardo Motta Maia

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

    ! Connectivit matrix inpoel(1:nnode, 1:nelem)
    real(kind=8), allocatable, dimension(:,:) :: inpoel

    ! Coordinate points coord(1:ndimn, 1:npoin)
    real(kind=8), allocatable, dimension(:,:) :: coord


    !
    ! HASH TABLE CREATION
    !

    ! Threshold of colisions.
    integer(kind=4) :: num_colision = 1000

    ! face(face_index,1): vertex number one of the face.
    ! face(face_incex,2): vertex number two of the face.
    ! face(face_incex,3): element to the left.
    ! face(face_incex,4): element to the right.
    integer(kind=4), allocatable, dimension(:,:) :: face

    ! Colisions.
    integer(kind=4), allocatable, dimension(:) :: colision

    integer(kind=4), allocatable, dimension(:) :: hash_vec

    contains

        !
        ! PUBLIC FUNCTIONS
        !

        ! Hash function #1: Summation of the two values.
        !
        integer(kind=4) function hash(val_1, val_2)

            implicit none

            integer(kind=4) val_1, val_2

            hash = val_1 + val_2

        end function hash

        ! Find dunction: Searches for a occurence in a vector.
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

program unstr_dr

    use shared
    implicit none

    call basic_ds

    call hc_faces

end program unstr_dr

subroutine basic_ds

    use shared
    implicit none

    integer(kind=4) :: i, elemn, kindof, n1, n2, n3, n4
    integer(kind=4) :: point, x_coord, y_coord


    ! At first let's get our mesh size.

    open(1,file='elemnConn.dat')
    open(2,file='pointCord.dat')
    open(3,file='elemnBonc.dat')

    read(1,*) nelem
    read(2,*) npoin
    read(3,*) nghos

    write(*,*)
    write(*,'(A,I8)') " + The number of elements in the mesh is   : ", nelem
    write(*,'(A,I8)') " + The number of points in the mesh is     : ", npoin
    write(*,'(A,I8)') " + The number of boundary faces in the mesh: ", nghos


    ! Now, let's build the connective matrix. (Debug this guy !)

    allocate(inpoel(nnode, nelem))

    do i = 1, nelem

        read(1,*) elemn, kindof, n1, n2, n3, n4

        inpoel(1,elemn) = n1
        inpoel(2,elemn) = n2
        inpoel(3,elemn) = n3
        inpoel(4,elemn) = n4
    
    end do


    ! Now, Let's build the coordinates. (Debug this guy !)

    allocate(coord(2,npoin))

    do i = 1, npoin

        read(2,*) point, x_coord, y_coord

        coord(1,point) = x_coord
        coord(2,point) = y_coord

    end do


    close(1)
    close(2)
    close(3)

end subroutine basic_ds

subroutine hc_faces

    use shared
    implicit none

    integer(kind=4) :: ivol, nfaces, idx, nf, p1, p2
    integer(kind=4), allocatable, dimension(:) :: ihash


    ! Let's allocate the number of the faces in the mesh (just for QUAD).

    nfaces = ((nelem * 4) + nghos) / 2

    write(*,'(A,I8)') " + The number of faces in the mesh: ", nfaces

    allocate(ihash(2*nfaces))
    allocate(face(nfaces,4))

    ihash = 0
    face  = 0
    nf    = 0

    do ivol = 1, nelem


        ! Do the horizontal faces.

        p1 = inpoel(1,ivol)
        p2 = inpoel(2,ivol)

        idx = hash(p1,p2)

        if (ihash(idx) == 0) then

            nf = nf + 1

            ihash(idx) = idx

            face(nf,1) = p1
            face(nf,2) = p2
            face(nf,3) = ivol
            face(nf,4) = -1

        else

            face(nf,4) = ivol

        end if


        ! Do the vertical faces.

        p1 = inpoel(2,ivol)
        p2 = inpoel(3,ivol)

        idx = hash(p1,p2)

        if (ihash(idx) == 0) then

            nf = nf + 1

            ihash(idx) = idx

            face(nf,1) = p1
            face(nf,2) = p2
            face(nf,3) = ivol
            face(nf,4) = -1

        else

            face(nf,4) = ivol

        end if


        ! Do the vertical faces.

        p1 = inpoel(3,ivol)
        p2 = inpoel(4,ivol)

        idx = hash(p1,p2)

        if (ihash(idx) == 0) then

            nf = nf + 1

            ihash(idx) = idx

            face(nf,1) = p1
            face(nf,2) = p2
            face(nf,3) = ivol
            face(nf,4) = -1

        else

            face(nf,4) = ivol

        end if

        ! Do the vertical faces.

        p1 = inpoel(4,ivol)
        p2 = inpoel(1,ivol)

        idx = hash(p1,p2)

        if (ihash(idx) == 0) then

            nf = nf + 1

            ihash(idx) = idx

            face(nf,1) = p1
            face(nf,2) = p2
            face(nf,3) = ivol
            face(nf,4) = -1

        else

            face(nf,4) = ivol

        end if

   end do


    do idx = 1, nfaces
        write(*,*) face(idx,1),face(idx,2),face(idx,3),face(idx,4)
    end do


end subroutine hc_faces
