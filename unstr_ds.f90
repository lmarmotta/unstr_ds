! This program ains to teach some basic unstrctured code datastructures.
!
! Author: Leonardo Motta Maia
! Co-Author: André Aguiar
! Co-Author: Edson Basso

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

program unstr_dr

    use shared
    implicit none

    call basic_ds

    call hc_faces

end program unstr_dr

subroutine basic_ds

    use shared
    implicit none

    integer(kind=4) :: i, elemn, kindof, n1, n2, n3, n4, bc_t
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


    ! Now, let's build the connective matrix.

    allocate(inpoel(nnode, nelem))

    do i = 1, nelem

        read(1,*) elemn, kindof, n1, n2, n3, n4

        inpoel(1,elemn) = n1
        inpoel(2,elemn) = n2
        inpoel(3,elemn) = n3
        inpoel(4,elemn) = n4
    
    end do


    ! Now, Let's build the coordinates.

    allocate(coord(2,npoin))

    do i = 1, npoin

        read(2,*) point, x_coord, y_coord

        coord(1,point) = x_coord
        coord(2,point) = y_coord

    end do


    ! Now, Let's read the ghosts.

    allocate(ghost(nghos,3))

    do i = 1, nghos
        
        read(3,*) bc_t, n1, n2

        ghost(i,1) = bc_t
        ghost(i,2) = n1
        ghost(i,3) = n2

    end do


    close(1)
    close(2)
    close(3)

end subroutine basic_ds

subroutine hc_faces

    ! Here I use hash table to build the faces of the mesh. The goal here is to 
    ! build efficiently the mesh datastructure. Let's roll...

    use shared
    implicit none

    integer(kind=4) :: ivol, nfaces, idx, nf, p1, p2, bc, n_colision
    integer(kind=4) :: ig, is_bc, pf1, pf2, pg1, pg2, max_hash_size
    integer(kind=4), allocatable, dimension(:) :: ihash
    integer(kind=4), allocatable, dimension(:,:) :: c_vol


    ! Let's allocate the number of the faces in the mesh (just for QUAD).

    nfaces = ((nelem * 4) + nghos) / 2

    write(*,'(A,I8)') " + The number of faces in the mesh         : ", nfaces

    allocate(face(nfaces,4))
    allocate(ighost(-nghos:1,4))

    face          =  0
    nf            =  0
    bc            = -1
    n_colision    =  0
    max_hash_size =  0


    ! Get the size of the hash main vector.

    do ivol = 1, nelem


        ! First face combination.

        p1 = inpoel(1,ivol)
        p2 = inpoel(2,ivol)

        idx = hash_b(p1,p2)

        if (idx > max_hash_size) then
            max_hash_size = idx
        end if


        ! Second face combination.

        p1 = inpoel(2,ivol)
        p2 = inpoel(3,ivol)

        idx = hash_b(p1,p2)

        if (idx > max_hash_size) then
            max_hash_size = idx
        end if


        ! Third face combination.

        p1 = inpoel(3,ivol)
        p2 = inpoel(4,ivol)

        idx = hash_b(p1,p2)

        if (idx > max_hash_size) then
            max_hash_size = idx
        end if


        ! Fourth face combination.

        p1 = inpoel(4,ivol)
        p2 = inpoel(1,ivol)

        idx = hash_b(p1,p2)

        if (idx > max_hash_size) then
            max_hash_size = idx
        end if

    end do


    write(*,'(A,I8)') " + The maximun hash size is                : ", max_hash_size

    allocate(c_vol(1:max_hash_size,2))
    allocate(ihash(1:max_hash_size))

    c_vol = 0
    ihash = 0


    ! Now, proceed with the mesh itself.

    do ivol = 1, nelem


        ! Do the first face of an element.

        p1 = inpoel(1,ivol)
        p2 = inpoel(2,ivol)


        ! Get the hash index for these two points.

        idx = hash_b(p1,p2)


        ! If the hash has an empty position it means that no face using these
        ! two points was created. Being that the case, we can create it without
        ! any fear.

        if (ihash(idx) == 0) then


            ! Increase the number of faces counter.

            nf = nf + 1


            ! We are now creating a face what means that this hash position is
            ! no longer availiable.

            ihash(idx) = idx


            ! When we have a colision, these information will be useful for us
            ! in the colision analysis.

            c_vol(idx,1) = nf
            c_vol(idx,2) = ivol


            ! Hey there ! I'm your face based datastructure that you need to
            ! graduate.... ;)

            face(nf,1) = p1
            face(nf,2) = p2
            face(nf,3) = ivol
            face(nf,4) = -1

        else if (ihash(idx) /= 0 .and. face(nf,4) == -1) then 

            ! If the hash position is not empty, this means that this volume
            ! shares a face with a volume that we already creat all faces. This
            ! means that this cell is the right cell... which is pretty cool !

            face(c_vol(idx,1),4) = ivol

        else if (ihash(idx) /= 0 .and. face(nf,4) /= -1) then 

            n_colision = n_colision + 1

        end if


        ! Every time now on, the procedure will be repeated to other face inside
        ! the volume. If you have more then one type of mesh element, do it for
        ! the number of faces of the element.

        ! Do the second face of an element.

        p1 = inpoel(2,ivol)
        p2 = inpoel(3,ivol)

        idx = hash_b(p1,p2)


        if (ihash(idx) == 0) then

            nf = nf + 1

            ihash(idx) = idx

            c_vol(idx,1) = nf
            c_vol(idx,2) = ivol

            face(nf,1) = p1
            face(nf,2) = p2
            face(nf,3) = ivol
            face(nf,4) = -1

        else if (ihash(idx) /= 0 .and. face(nf,4) == -1) then 

            face(c_vol(idx,1),4) = ivol

        else if (ihash(idx) /= 0 .and. face(nf,4) /= -1) then 

            n_colision = n_colision + 1

        end if


        ! Do the third face of an element.

        p1 = inpoel(3,ivol)
        p2 = inpoel(4,ivol)

        idx = hash_b(p1,p2)


        if (ihash(idx) == 0) then

            nf = nf + 1

            ihash(idx) = idx

            c_vol(idx,1) = nf
            c_vol(idx,2) = ivol

            face(nf,1) = p1
            face(nf,2) = p2
            face(nf,3) = ivol
            face(nf,4) = -1

        else if (ihash(idx) /= 0 .and. face(nf,4) == -1) then 

            face(c_vol(idx,1),4) = ivol

        else if (ihash(idx) /= 0 .and. face(nf,4) /= -1) then 

            n_colision = n_colision + 1

        end if

        ! Do the fourth face of an element.

        p1 = inpoel(4,ivol)
        p2 = inpoel(1,ivol)

        idx = hash_b(p1,p2)


        if (ihash(idx) == 0) then

            nf = nf + 1

            ihash(idx) = idx

            c_vol(idx,1) = nf
            c_vol(idx,2) = ivol

            face(nf,1) = p1
            face(nf,2) = p2
            face(nf,3) = ivol
            face(nf,4) = -1

        else if (ihash(idx) /= 0 .and. face(nf,4) == -1) then 

            face(c_vol(idx,1),4) = ivol

        else if (ihash(idx) /= 0 .and. face(nf,4) /= -1) then 

            n_colision = n_colision + 1

        end if

    end do


    ! Now we have to number the ghosts.

    do nf = 1, nfaces
        if (face(nf,4) < 0) then
            face(nf,4) = bc
            bc = bc - 1
        end if
    end do

    
    ! Now, to deal with the mesh ghosts, Let's loop through the faces of the
    ! mesh. Check the boundary faces, and check the nodes that are contained
    ! inside that element.

    do nf = 1, nfaces
        do ig = 1, nghos

            is_bc = face(nf,4)


            ! If the face I'm in is a boundary face, I have to link the ighost
            ! vector with the face index.

            if (is_bc < 0) then

                pf1 = face(nf,1)
                pf2 = face(nf,2)

                pg1 = ghost(ig,2)
                pg2 = ghost(ig,3)

                if (pf1 == pg1 .and. pf2 == pg2 .or. pf1 == pg2 .and. pf2 == pg1) then

                    ighost(-ig,1) = ghost(ig,1)  ! Boundary type.
                    ighost(-ig,2) = nf           ! Face index.
                    ighost(-ig,3) = face(nf,1)   ! Face point 1.
                    ighost(-ig,4) = face(nf,2)   ! Face point 2.

                end if

            end if
        end do
    end do


    write(*,'(A,I8)') " + The number of collisions                : ", n_colision


    ! Print the results to debug file.

    open(4,file="debug_ds.dat")

    write(4,'(A)') "Writing face vector connectivity."
    write(4,'(A)') "Face index :: face point #1 :: face point #2 :: CR :: CL "

    do idx = 1, nfaces
        write(4,'(5I8)') idx,face(idx,1),face(idx,2),face(idx,3),face(idx,4)
    end do

    write(4,*) ""
    write(4,*) "-------------------------------------------"
    write(4,*) "-------------------------------------------"

    write(4,'(A)') "Writing ghost vector connectivity."
    write(4,'(A)') "Ghost index :: Boundary Type :: face index :: face point #1 :: face point #2 :: CL"

    do ig = 1, nghos
        write(4,'(5I8)') ig,ighost(-ig,1),ighost(-ig,2),ighost(-ig,3),ighost(-ig,4)
    end do

    close(4)

end subroutine hc_faces
