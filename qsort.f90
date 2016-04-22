! Note: these implementations are not mine, but derive from ACM QUICKSORT - ALGORITHM #402 (W. H. Verity's implementation)

SUBROUTINE qsort_int (a,ord,n)
  IMPLICIT NONE

  INTEGER, intent(in)  :: a(n)
  INTEGER, intent(in)  :: n
  integer, INTENT(out) :: ord(n)

  !==============SORTS THE ARRAY A(I),I=1,2,...,N BY PUTTING THE
  !   ASCENDING ORDER VECTOR IN ORD.  THAT IS ASCENDING ORDERED A
  !   IS A(ORD(I)),I=1,2,...,N; DESCENDING ORDER A IS A(ORD(N-I+1)),
  !   I=1,2,...,N .  THIS SORT RUNS IN TIME PROPORTIONAL TO N LOG N .


  !     ACM QUICKSORT - ALGORITHM #402 - IMPLEMENTED IN FORTRAN 66 BY
  !                                 WILLIAM H. VERITY, WHV@PSUVM.PSU.EDU
  !                                 CENTER FOR ACADEMIC COMPUTING
  !                                 THE PENNSYLVANIA STATE UNIVERSITY
  !                                 UNIVERSITY PARK, PA.  16802



  INTEGER :: poplst(2,20)
  integer :: x,xx,z,zz,y
  INTEGER :: yp, ndeep, u1, l1, i, u, l, p, q, ix, iz, ip, iq

  !     TO SORT DIFFERENT INPUT TYPES, CHANGE THE FOLLOWING
  !     SPECIFICATION STATEMENTS; FOR EXAMPLE, FOR FORTRAN CHARACTER
  !     USE THE FOLLOWING:  CHARACTER *(*) A(N)

  ndeep=0
  u1=n
  l1=1
  DO   i=1,n
     ord(i)=i
  END DO
2 IF (u1 <= l1) then
     RETURN
  end if

3 l=l1
  u=u1

  ! PART

4 p=l
  q=u
  !     FOR CHARACTER SORTS, THE FOLLOWING 3 STATEMENTS WOULD BECOME
  !     X = ORD(P)
  !     Z = ORD(Q)
  !     IF (A(X) .LE. A(Z)) GO TO 2

  !     WHERE "CLE" IS A LOGICAL FUNCTION WHICH RETURNS "TRUE" IF THE
  !     FIRST ARGUMENT IS LESS THAN OR EQUAL TO THE SECOND, BASED ON "LEN"
  !     CHARACTERS.

  x=a(ord(p))
  z=a(ord(q))
  IF (x <= z) GO TO 5
  y=x
  x=z
  z=y
  yp=ord(p)
  ord(p)=ord(q)
  ord(q)=yp
5 IF (u-l <= 1) GO TO 15
  xx=x
  ix=p
  zz=z
  iz=q

  ! LEFT

6 p=p+1
  IF (p >= q) GO TO 7
  x=a(ord(p))
  IF (x >= xx) GO TO 8
  GO TO 6
7 p=q-1
  GO TO 13

  ! RIGHT

8 q=q-1
  IF (q <= p) GO TO 9
  z=a(ord(q))
  IF (z <= zz) GO TO 10
  GO TO 8
9 q=p
  p=p-1
  z=x
  x=a(ord(p))

  ! DIST

10 IF (x <= z) GO TO 11
  y=x
  x=z
  z=y
  ip=ord(p)
  ord(p)=ord(q)
  ord(q)=ip
11 IF (x <= xx) GO TO 12
  xx=x
  ix=p
12 IF (z >= zz) GO TO 6
  zz=z
  iz=q
  GO TO 6

  ! OUT

13 CONTINUE
  IF (.NOT.(p /= ix.AND.x /= xx)) GO TO 14
  ip=ord(p)
  ord(p)=ord(ix)
  ord(ix)=ip
14 CONTINUE
  IF (.NOT.(q /= iz.AND.z /= zz)) GO TO 15
  iq=ord(q)
  ord(q)=ord(iz)
  ord(iz)=iq
15 CONTINUE
  IF (u-q <= p-l) GO TO 16
  l1=l
  u1=p-1
  l=q+1
  GO TO 17
16 u1=u
  l1=q+1
  u=p-1
17 CONTINUE
  IF (u1 <= l1) GO TO 18

  ! START RECURSIVE CALL

  ndeep=ndeep+1
  poplst(1,ndeep)=u
  poplst(2,ndeep)=l
  GO TO 3
18 IF (u > l) GO TO 4

  ! POP BACK UP IN THE RECURSION LIST

  IF (ndeep == 0) GO TO 2
  u=poplst(1,ndeep)
  l=poplst(2,ndeep)
  ndeep=ndeep-1
  GO TO 18

END SUBROUTINE qsort_int

SUBROUTINE qsort_real (a, ord,n)
  IMPLICIT NONE

  real,    intent(in)  :: a(n)
  INTEGER, intent(in)  :: n
  integer, INTENT(out) :: ord(n)

  !==============SORTS THE ARRAY A(I),I=1,2,...,N BY PUTTING THE
  !   ASCENDING ORDER VECTOR IN ORD.  THAT IS ASCENDING ORDERED A
  !   IS A(ORD(I)),I=1,2,...,N; DESCENDING ORDER A IS A(ORD(N-I+1)),
  !   I=1,2,...,N .  THIS SORT RUNS IN TIME PROPORTIONAL TO N LOG N .


  !     ACM QUICKSORT - ALGORITHM #402 - IMPLEMENTED IN FORTRAN 66 BY
  !                                 WILLIAM H. VERITY, WHV@PSUVM.PSU.EDU
  !                                 CENTER FOR ACADEMIC COMPUTING
  !                                 THE PENNSYLVANIA STATE UNIVERSITY
  !                                 UNIVERSITY PARK, PA.  16802



  INTEGER :: poplst(2,20)
  REAL :: x,xx,z,zz,y
  INTEGER :: yp, ndeep, u1, l1, i, u, l, p, q, ix, iz, ip, iq

  !     TO SORT DIFFERENT INPUT TYPES, CHANGE THE FOLLOWING
  !     SPECIFICATION STATEMENTS; FOR EXAMPLE, FOR FORTRAN CHARACTER
  !     USE THE FOLLOWING:  CHARACTER *(*) A(N)

  ndeep=0
  u1=n
  l1=1
  DO   i=1,n
     ord(i)=i
  END DO
2 IF (u1 <= l1) then
     RETURN
  end if

3 l=l1
  u=u1

  ! PART

4 p=l
  q=u
  !     FOR CHARACTER SORTS, THE FOLLOWING 3 STATEMENTS WOULD BECOME
  !     X = ORD(P)
  !     Z = ORD(Q)
  !     IF (A(X) .LE. A(Z)) GO TO 2

  !     WHERE "CLE" IS A LOGICAL FUNCTION WHICH RETURNS "TRUE" IF THE
  !     FIRST ARGUMENT IS LESS THAN OR EQUAL TO THE SECOND, BASED ON "LEN"
  !     CHARACTERS.

  x=a(ord(p))
  z=a(ord(q))
  IF (x <= z) GO TO 5
  y=x
  x=z
  z=y
  yp=ord(p)
  ord(p)=ord(q)
  ord(q)=yp
5 IF (u-l <= 1) GO TO 15
  xx=x
  ix=p
  zz=z
  iz=q

  ! LEFT

6 p=p+1
  IF (p >= q) GO TO 7
  x=a(ord(p))
  IF (x >= xx) GO TO 8
  GO TO 6
7 p=q-1
  GO TO 13

  ! RIGHT

8 q=q-1
  IF (q <= p) GO TO 9
  z=a(ord(q))
  IF (z <= zz) GO TO 10
  GO TO 8
9 q=p
  p=p-1
  z=x
  x=a(ord(p))

  ! DIST

10 IF (x <= z) GO TO 11
  y=x
  x=z
  z=y
  ip=ord(p)
  ord(p)=ord(q)
  ord(q)=ip
11 IF (x <= xx) GO TO 12
  xx=x
  ix=p
12 IF (z >= zz) GO TO 6
  zz=z
  iz=q
  GO TO 6

  ! OUT

13 CONTINUE
  IF (.NOT.(p /= ix.AND.x /= xx)) GO TO 14
  ip=ord(p)
  ord(p)=ord(ix)
  ord(ix)=ip
14 CONTINUE
  IF (.NOT.(q /= iz.AND.z /= zz)) GO TO 15
  iq=ord(q)
  ord(q)=ord(iz)
  ord(iz)=iq
15 CONTINUE
  IF (u-q <= p-l) GO TO 16
  l1=l
  u1=p-1
  l=q+1
  GO TO 17
16 u1=u
  l1=q+1
  u=p-1
17 CONTINUE
  IF (u1 <= l1) GO TO 18

  ! START RECURSIVE CALL

  ndeep=ndeep+1
  poplst(1,ndeep)=u
  poplst(2,ndeep)=l
  GO TO 3
18 IF (u > l) GO TO 4

  ! POP BACK UP IN THE RECURSION LIST

  IF (ndeep == 0) GO TO 2
  u=poplst(1,ndeep)
  l=poplst(2,ndeep)
  ndeep=ndeep-1
  GO TO 18

END SUBROUTINE qsort_real
