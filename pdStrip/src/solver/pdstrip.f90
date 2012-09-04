! Copyright (c) 1980-2006 Heinrich S�ding
!               2005-2006 Volker Bertram
!
! This is Free Software. For license terms, see the LICENSE file.

module pdstripsubprograms
!implicit none                   !BNV/2006-05-08 there are implicit variables at lines
                                 ! 713, 720, 730 and 743

integer, parameter:: nmumax=100  !max. number of wave angles, regular waves
integer, parameter:: nofmax=240  !max. number of offset points on a section and the free surface
integer, parameter:: nfremax=52  !max. number of frequencies for hydrodynamic section calculations
integer, parameter:: nsemax=100  !max. number of offset sections
integer, parameter:: nprmax=100  !max. number of pressure points per section
integer, parameter:: nfs=55      !panel number on free suface on each side of the body
integer, parameter:: nomma=200   !max. no. of frequencies in transfer function calculation
integer, parameter:: nvmax=50    !max. no. of ship speeds
integer, parameter:: nbmax=200   !max. no. of points where motions should be determined
logical:: sym                    !true if all offset sections are symmetric
logical:: subm                   !true if a section is fully submerged
logical:: lsect                  !true if hydrodynamic section calculations required
logical:: lsign                  !t to compute significant amplitudes in natural seaways; f otherw.
logical:: ls                     !true if intersection forces and moments to be determined
integer:: ise,nse                !index and number of sections
integer:: npres,npres1           !number of pressure points per section; npres`=max(npres,1)
integer:: nof(nsemax)            !number of offset points on a section
integer:: ngap                   !number of `gaps' of a section
integer:: gap(10)                !offset point index where a gap begins
integer:: nmu                    !number of wave angles in hydrodynamic section calculations

real, parameter:: pi=3.14159
real:: wangl(nmumax)             !wave angles in hydrodynamic section calculations
real:: g                         !gravity acceleration
real:: rho                       !water density
real:: zwl                       !z coordinate of the still waterline
real:: zbot                      !z coordinate of the water bottom
real:: tg                        !reference draft (same for all sections)
real:: waven                     !wave number
real:: x(0:nsemax+1)             !x coordinates of sections
real:: yof(nsemax,nofmax)        !y coordinates of offset points (section index, point intex)
real:: zof(nsemax,nofmax)        !z coordinates of offset points (section index, point intex)
real:: bcross(nsemax)            !maximum breadth of sections used for cross-flow resistance
real:: tcross(nsemax)            !maximum draft of sections used for cross-flow resistance
real:: area(0:nsemax)            !submerged area of sections
real:: ys(0:nsemax+1)            !y coordinate of center of gravity of section area
real:: zs(0:nsemax+1)            !z coordinate of center of gravity of section area
real:: zdrift                    !z coordinate for water particle drift velocity

character(80):: text             !text describing the ship and its loading case
character(80):: offsetfile       !name of the file containing the offset point data
! GT/2006-04-28
character(LEN = 128) :: pathName = ' ', fileName = 'pdstrip' ! used to make GUI in Windows work

real, dimension(:), allocatable :: yofTemp, zofTemp   ! GT/2006-05-03: additional code to facilitate Salford FTN95 compiler

interface operator(.mprod.); module procedure prodmatr; end interface !matrix product
interface operator(.vprod.); module procedure vectprod; end interface !vector product

contains

subroutine sectiondata           !input section data; call of sectionhydrodynamics
! for deep and shallow water; section motion and wave excitation; with and without symmetry
integer:: i,ii,j                 !indices
integer:: im1                    !i-1 except for i=1
integer:: nfre=52                !number of frequencies
real:: fp(nfremax)               !wave frequency parameter omega^2*t/g
real:: om(nfremax)               !circular wave frequency
real:: da                        !section area increment
real:: yof1(nofmax),zof1(nofmax) !y, z coordinates of offset points of one section only
complex:: addedm(3,3,nfremax)    !complex added mass matrix of a section = a+d/(i*omega_e)
complex:: diff(3,nmumax,nfremax) !diffraction force amplitude vector
complex:: frkr(3,nmumax,nfremax) !Froude-Krilow force amplitude vector
complex:: pr(nprmax,3+nmumax,nfremax) !pressure amplitude at pressure points on section contour

fp=(/0.01,0.015, 0.020,.025,.031,.04,.05,.063,.08,.10,.12,.15,.19,.24,.31,.39, &
   .47,.55,.62,.70,.80,.90,1.0,1.1,1.25,1.4,1.55,1.7,1.9,2.2,2.4,2.7,3.0,3.3,3.6,4.0, &
   4.5,5.2,6.,7.,8.,9.,10.5,12.0,13.5,15.,17.,20.,25.,30.,40.,50./)     !standard frequency parameters

!nfre=3; fp(1:3)=(/0.1,1.,10./)  !only for rapid testing

read(5,'(a)') text
write(6,'(/1x,1a80)') text
write(6,'(a)') ' In the following input data x,y,z are directed forward, to port side, up'
read(5,*) g,rho,zwl,zbot,zdrift,nmu,wangl(1:nmu),offsetfile
write(6,'(/'' Gravity acceleration          '',f 8.3)') g
write(6,'( '' Water density                 '',f 8.3)') rho
write(6,'( '' z of still waterline          '',f 8.3)') zwl
write(6,'( '' z of water bottom             '',g12.3)') zbot
write(6,'( '' z for water drift velocity    '',f 8.3)') zdrift
write(6,'( '' Wave encounter angles         '',(10f8.3))') wangl(1:nmu)
write(6,'(a,a)') ' Offset file name: ',trim(offsetfile)
if (nmu>nmumax) stop'>> Too many wave angles'
if (zbot>0) write(6,*)'*** positive bottom z coordinate appears unreasonable'
zwl=-zwl; zbot=-zbot; zdrift=-zdrift                      !change from input to output coord. system
if (lsect) then
   write(20,'(a)') text
   write(20,*)nfre                    !write initial data on file sectionsresults
endif
open(9,file=TRIM(pathName)//offsetfile,status='old')
read(9,*) nse,sym,tg                                  !number of sections, symmetry?, reference draft
write(6,'(a,i8  )') ' Number of sections            ',nse
write(6,'(a,a   )') ' Symmetry?                     ',merge('     yes','      no',sym)
write(6,'(a,f8.3)') ' Reference draft               ',tg
if (nse>nsemax) stop'>> Too many sections'
if (tg<=0) stop'>> Reference draft should be positive'
if (sym.and.npres/=0) stop'>> Pressure calculation requires sym=.false'
if (npres==1 .or. npres==2) stop'>> Min. 3 pressure points per section required. 0 or more than 15 recommended'
om(1:nfre)=sqrt(fp(1:nfre)*g/tg)                   !circular fr. resulting from frequency parameters
Sections: do ise=1,nse
   read(9,*) x(ise),nof(ise),ngap,gap(1:ngap)               !section x coord., no. of offsets, gap data
   read(9,*) yof(ise,1:nof(ise)),zof(ise,1:nof(ise))                          !offsets read from file 9
   write(6,'(" Section no.",i3," at x=",f10.3)') ise,x(ise)
   if (ngap>0) write(6,'(a,10i5)') ' Gaps following point no.',gap(1:ngap)
   write(6,'(" y:",8f10.3)') yof(ise,1:nof(ise))
   write(6,'(" z:",8f10.3)') zof(ise,1:nof(ise))
   if (sym.and.yof(ise,1)/=0)stop'First y must be 0 for symmetrical body'
   yof(ise,1:nof(ise))=-yof(ise,1:nof(ise))
   zof(ise,1:nof(ise))=-zof(ise,1:nof(ise))                 !change from input to output coord. system
   area(ise)=0; ys(ise)=0; zs(ise)=0            !compute area etc. gaps covered 2 times if sym=.false.
   Offsets: do i=1,nof(ise)
      im1=merge(nof(ise),i-1,i==1)
      if (ngap>0.and.any(gap(1:ngap)==im1)) cycle
      da=(yof(ise,im1)*zof(ise,i)-zof(ise,im1)*yof(ise,i))/2
      area(ise)=area(ise)+da
      ys(ise)=ys(ise)+da*(yof(ise,im1)+yof(ise,i))/3
      zs(ise)=zs(ise)+da*(zof(ise,im1)+zof(ise,i))/3
   enddo Offsets
   bcross(ise)=maxval(yof(ise,1:nof(ise)))-minval(yof(ise,1:nof(ise)))
   tcross(ise)=maxval(zof(ise,1:nof(ise)))-minval(zof(ise,1:nof(ise)))
   if (ngap>0) tcross(ise)=tcross(ise)-merge(1.,0.5,sym)*sum(abs(zof(ise,gap(1:ngap))-zof(ise,gap(1:ngap)+1)))
   if (sym) then
      da=yof(ise,nof(ise))*(zof(ise,nof(ise))-zof(ise,1))
      area(ise)=2*area(ise)+da
      ys(ise)=0
      zs(ise)=2*zs(ise)+da*(2*zof(ise,nof(ise))+zof(ise,1))/3
      bcross(ise)=2*bcross(ise)
   endif
   ys(ise)=ys(ise)/area(ise)
   zs(ise)=zs(ise)/area(ise)
   if ((.not.sym.and.(yof(ise,1)-yof(ise,nof(ise)))**2+(zof(ise,1)-zof(ise,nof(ise)))**2<1e-4) &
        .or.(sym.and.abs(yof(ise,nof(ise)))<1e-3)) then
      subm=.true.; write(6,*) 'fully submerged'
   else
      subm=.false.
      if (zof(ise,nof(ise))/=zwl) stop'>> Last z must be equal to zwl'
      if (.not.sym.and.zof(ise,1)/=zwl) stop'>> First z must be equal to zwl'
   endif
   yof1(1:nof(ise))=yof(ise,1:nof(ise)); zof1(1:nof(ise))=zof(ise,1:nof(ise))
   if (lsect) then
      call sectionhydrodynamics(nof(ise),yof1,zof1,nfre,om,addedm,diff,frkr,pr)
      Frequencies: do i=1,nfre
         write(20,*) om(i),nmu,wangl(1:nmu)*pi/180                 !store on file sectionresults: wave data
         write(20,*) (om(i)**2*addedm(j,1:3,i),j=1,3)                                      !radiation force
         write(20,*) (diff(j,1:nmu,i),j=1,3)                                             !diffraction force
         write(20,*) (frkr(j,1:nmu,i),j=1,3)                                           !Froude-Krilow force
         if (npres>0) write(20,*)(ii,pr(ii,1:3+nmu,i),ii=1,npres)                               !pressures
      enddo Frequencies
   endif
enddo Sections
if (lsect) then
   ! GT/2006-05-03: original code replaced by code to facilitate Salford FTN95 compiler
   allocate(yofTemp(1:nse)); allocate(zofTemp(1:nse))                                              ! GT/2006-05-03
   do ise = 1, nse                                                                                 ! GT/2006-05-03
      yofTemp(ise) = sum((/(yof(ise,1:nof(ise)))/))                                                ! GT/2006-05-03
      zofTemp(ise) = sum((/(zof(ise,1:nof(ise)))/))                                                ! GT/2006-05-03
   end do                                                                                          ! GT/2006-05-03
   write(20,*) npres+sum(wangl(1:nmu))+sum(x(1:nse))+sum(yofTemp)+sum(zofTemp)+g+rho+zwl+1/zbot    !test number for unchanged input data
   deallocate(yofTemp); deallocate(zofTemp)                                                        ! GT/2006-05-03
end if
end subroutine sectiondata

subroutine sectionhydrodynamics(nof1,yof1,zof1,nfre,om,addedm,diff,frkr,pr)
integer, intent(in):: nof1             !number of offset points on one section
integer:: nfre                         !number of wave frequencies
integer:: i,ii                         !indices
integer:: iom                          !index of frequency
integer:: ileft(nprmax)                !index of offset point following a pressure point
real:: yof1(nofmax),zof1(nofmax)       !y and z coordinates of offset points of one section
real:: yq(0:nofmax),zq(0:nofmax)       !y and z coordinates of sources
real:: om(nfremax)                     !circular frequency
real:: girth(nofmax)                   !girth length of section up to offset points
real:: u                               !girth length up to current pressure point
real:: aint(nprmax)                    !interpolation factor for pressure point between offsets
complex:: addedm(3,3,nfremax)          !complex added mass matrices of one section, all frequencies
complex:: diff(3,nmumax,nfremax)       !diffraction force amplitudes of one section, all frequencies
complex:: frkr(3,nmumax,nfremax)       !Froude-Krilow force amplitudes, one section, all frequ.
complex:: addedmprel(2,2)              !preliminary added mass matrix (1 or 2 degrees of freedom)
complex:: diffprel(2,nmumax)           !preliminary diffraction force
complex:: frkrprel(2,nmumax)           !preliminary Froude-Krilow force
complex:: pr(nprmax,3+nmumax,nfremax)  !pressure amplitudes
if (nfre.gt.nfremax) stop'>> Too many frequencies'
call testsuitability(nof1,yof1,zof1)                            !discretization of section suitable?
girth(1)=0.
do i=2,nof1
   if (ngap>0.and.any(gap(1:ngap)==i-1)) then
      girth(i)=girth(i-1); cycle
   endif
   girth(i)=girth(i-1)+sqrt((yof1(i)-yof1(i-1))**2+(zof1(i)-zof1(i-1))**2)
enddo
PressurePoints: do ii=1,npres                                           !ii index of pressure points
   u=(ii-1.0)/(npres-1)*girth(nof1)                              !u= girth length to pressure point ii
   i=which(nof1-1,u<=girth(2:nof1))+1                       !pressure point between ileft-1 and ileft
   ileft(ii)=i
   aint(ii)=(u-girth(i-1))/(girth(i)-girth(i-1))     !intp=val(ileft-1)+aint*(val(ileft)-val(ileft-1))
   write(24,*)yof1(i-1)+aint(ii)*(yof1(i)-yof1(i-1)),zof1(i-1)+aint(ii)*(zof1(i)-zof1(i-1))
enddo PressurePoints
Frequencies: do iom=1,nfre
   Symmetry: if (sym) then
      addedm(1:3,1:3,iom)=0
      call addedmassexcitations(nof1,yof1,zof1,om(iom),1,addedm(2,2,iom),diff(2,1:nmu,iom), &
         frkr(2,1:nmu,iom))
      call addedmassexcitations(nof1,yof1,zof1,om(iom),2,addedmprel,diffprel,frkrprel)
      addedm(1:3:2,1:3:2,iom)=addedmprel
      diff(1:3:2,1:nmu,iom)=diffprel(1:2,1:nmu)
      frkr(1:3:2,1:nmu,iom)=frkrprel(1:2,1:nmu)
   else symmetry
      call addedmassexcitationa(nof1,yof1,zof1,om(iom),addedm(1,1,iom),diff(1,1,iom), &
         frkr(1,1,iom),pr(1,1,iom),ileft,aint)
   endif Symmetry
enddo Frequencies
end subroutine sectionhydrodynamics

function pq(dy,dz)  !pq=source (flux 2 pi) potential. dy,dz vector from source point to actual point
real:: pq,dy,dz
pq=0.5*log(dy**2+dz**2)
end function pq

function pqn(dy1,dz1,dy2,dz2)   !pqn=flux (area/time) between p1, p2 due to source of strength 2 pi.
real:: pqn,dy1,dz1,dy2,dz2             !pqn positive for flow to the left when looking from p1 to p2
pqn=-atan2(dy2*dz1-dz2*dy1,dy1*dy2+dz1*dz2)
end function pqn

function pqb(yof,zof,yq,zq)                                   !pq possibly with bottom mirror source
real:: pqb,yof,zof,yq,zq
pqb=pq(yof-yq,zof-zq)
if (zbot<1e6) pqb=pqb+pq(yof-yq,zof-(2*zbot-zq))
end function pqb

function pqnb(yof1,zof1,yof2,zof2,yq,zq)                     !pqn possibly with bottom mirror source
real:: pqnb,yof1,zof1,yof2,zof2,yq,zq
pqnb=pqn(yof1-yq,zof1-zq,yof2-yq,zof2-zq)
if (zbot<1e6) pqnb=pqnb+pqn(yof1-yq,zof1-(2*zbot-zq),yof2-yq,zof2-(2*zbot-zq))
end function pqnb

function pqs(yof,zof,yq,zq,mp)                                           !=pq with y=0 mirror source
integer:: mp                                                 !and possibly with bottom mirror source
real:: pqs,yof,zof,yq,zq
pqs=pq(yof-yq,zof-zq)+mp*pq(yof+yq,zof-zq)
if (zbot<1000.) pqs=pqs+pq(yof-yq,zof-(2*zbot-zq))+mp*pq(yof+yq,zof-(2*zbot-zq))
end function pqs

function pqsn(yof1,zof1,yof2,zof2,yq,zq,mp)                             !=pqn with y=0 mirror source
integer:: mp
real:: pqsn,yof1,zof1,yof2,zof2,yq,zq                       !and possibly with bottom mirror sources
pqsn=pqn(yof1-yq,zof1-zq,yof2-yq,zof2-zq)+mp*pqn(yof1+yq,zof1-zq,yof2+yq,zof2-zq)
if (zbot<1000.) pqsn=pqsn+pqn(yof1-yq,zof1-(2*zbot-zq),yof2-yq,zof2-(2*zbot-zq)) &
   +mp*pqn(yof1+yq,zof1-(2*zbot-zq),yof2+yq,zof2-(2*zbot-zq))
end function pqsn

subroutine addedmassexcitationa(nof1,yof1,zof1,om,addedm1,diff1,frkr1,pr,ileft,aint)
! added mass and wave excitation, deep and shallow water, asymmetrical section allowed
! bottom at zbot, surface at zwl. determines also pressure at interpolated points.
! for deep water use zbot=1.e6. water density 1 assumed.
! computes complex added mass = mass-i/omegae*damping for sway,heave,roll (3*3 matrix)
! and diffraction and Froude-Krilow forces diff,frkr for nmu wave angles wangl (degree)
! section offset points from starboard waterline point over basis to port wl point
! +y to starboard; +z downwards. first and last point should be on waterline.
integer, intent(in):: nof1             !no. of offset points on a section
integer:: ileft(nprmax)                !index of offset point following a pressure point
integer:: k,k1                         !offset point indices
integer:: nf                           !number of free-surface panels (undamped) on either side
integer:: iw                           !index of wave angle
integer:: is                           !points to row if equation solver detects singularity
integer:: i,ii,j,l,m                   !indices
real:: aint(nprmax)                    !interpolation factor for pressure point between offsets
real:: yof1(nofmax),zof1(nofmax)       !y and z coordinate of offset points on one section
real:: t                               !water depth
real:: fact1,fact2,factor,fact         !factors
real:: dy,dz                           !range of contour segment in y and z direction
real:: sinw                            !sin(wave angle)
real:: y1                              !y coordinate of a section offset point
real:: om                              !circular frequency
real:: h                               !point distance on free surface near to body
real:: hend                            !point distance on free surface far from body
real:: yq(0:nofmax),zq(0:nofmax)       !y and z coordinates of sources
complex:: a(0:nofmax,0:nofmax+3+nmumax)!coefficient matrix to determine source strengths
complex:: ciom                         !i*omega
complex:: detl                         !log(determinant) of the coefficient matrix a
complex:: f                            !force amplitude
complex:: addedm1(3,3)                 !complex added mass matrix for one frequency, one section
complex:: pr(nprmax,3+nmumax)          !pressure amplitudes at pressure points
complex:: phiatoffsets(nofmax)         !pressure amplitudes at offset points
complex:: diff1(3,nmu)                 !diffraction force amplitude, one frequency, one section
complex:: frkr1(3,nmu)                 !Froude-Krilow force amplitude, one frequency, one section
complex:: zw1,zw2,zw1a,zw2a            !complex intermediate numbers

if (nof1.gt.100) stop'***  Too many (>100) points on section.'
if (nof1+nfs>nofmax) stop'*** Too many body + waterline panels'
if (nmu.gt.nmumax) stop'*** Too many angles'
if (.not.subm.and.(abs(zof1(1)-zwl)>0.001.or.abs(zof1(nof1)-zwl) >0.001)) stop'*** First or last section point not on waterline'
addedm1=0.; diff1=0; pr=0
TwoFreeSurfaceDiscretisations: do nf=25,28,3
   ciom=(0.,1.)*om
   t=zbot-zwl                                                                           !t=water depth
   waven=max(om**2/g,om/sqrt(g*t))                                                        !wave number
   if (waven*t<6) then
      do i=1,10                               !for shallow water: iterative determination of wave number
         waven=waven-(waven*tanh(waven*t)-om**2/g)/(tanh(waven*t)+waven/cosh(waven*t)**2)/2
      enddo
      fact1=g*exp( waven*t)/2/cosh(waven*t); fact2=g*exp(-waven*t)/2/cosh(waven*t)
   else
      fact1=g; fact2=0.
   endif
   h=sqrt((yof1(nof1)-yof1(nof1-1))**2+(zof1(nof1)-zof1(nof1-1))**2)    !near distance on free surface
   hend=2*pi/(12*waven)                      !far-off distance of points on free surface=wavelength/12

   i=0                        ! compute free surface grid points yof1,zof1 and all source points yq,zq
   BodySegments: do k=1,nof1-1
      if (ngap>0.and.any(gap(1:ngap)==k)) cycle
      i=i+1
      yq(i)=(yof1(k)+yof1(k+1))/2-(zof1(k+1)-zof1(k))/20                   !source points within section
      zq(i)=(zof1(k)+zof1(k+1))/2+(yof1(k+1)-yof1(k))/20
   enddo BodySegments
   FreeSurfaceSegments: do k=nof1,nof1+nfs-1
      h=min(h*1.5,hend)
      yof1(k+1)=yof1(k)-h
      yof1(nfs+k+1)=merge(yof1(1),yof1(nfs+k),k==nof1)+h
      zof1(k+1)=zwl
      zof1(nfs+k+1)=zwl
      yq(k-ngap)=yof1(k)-0.5*h
      zq(k-ngap)=zwl-1.0*h
      yq(nfs+k-ngap)=merge(yof1(1),yof1(nfs+k),k==nof1)+0.5*h
      zq(nfs+k-ngap)=zwl-1.0*h
   enddo FreeSurfaceSegments
   yq(0)=0                                                                                !zero source
   zq(0)=zwl-abs(yof1(nof1+nfs))/2
   a(0:nof1-ngap+2*nfs-1,nof1-ngap+2*nfs:nof1-ngap+2*nfs+2+nmu)=0.            !inhomogeneous terms = 0
   frkr1=0

 !Body boundary condition (panel integrals) and Fr.-Krilow forces
   j=0                                                        !j=index of body segments excluding gaps
   BodySegm: do k=1,nof1-1
      if (ngap>0.and.any(gap(1:ngap)==k)) cycle
      j=j+1
      dy=yof1(k+1)-yof1(k)
      dz=zof1(k+1)-zof1(k)
      a(j,nof1-ngap+2*nfs  )=-ciom*dz
      a(j,nof1-ngap+2*nfs+1)=+ciom*dy
      a(j,nof1-ngap+2*nfs+2)=+ciom*(yof1(k+1)**2-yof1(k)**2+zof1(k+1)**2-zof1(k)**2)/2
      WaveAngles: do iw=1,nmu
         sinw=sin(wangl(iw)*pi/180)
         zw1=cmplx(-waven*dz,waven*sinw*dy)
         if (abs(zw1)>50.) stop'>> Frequency too high'
         if (abs(zw1).lt.0.001) zw1=0.001
         zw1a=-conjg(zw1)
         zw2=exp(waven*cmplx(-zof1(k)+zwl,yof1(k)*sinw))* merge((exp(zw1)-1.)/zw1,1.+0.5*zw1,abs(zw1 )>0.01)
         zw2a=exp(waven*cmplx( zof1(k)-zwl,yof1(k)*sinw))*merge((exp(zw1a)-1.)/zw1a,1.+0.5*zw1a,abs(zw1a)>0.01)
         a(j,nof1-ngap+2*nfs+2+iw)=-fact1*waven/om*cmplx(dy,sinw*dz)*zw2*(0.,1.)
         frkr1(1,iw)=frkr1(1,iw)+fact1*zw2*dz*rho
         frkr1(2,iw)=frkr1(2,iw)-fact1*zw2*dy*rho
         frkr1(3,iw)=frkr1(3,iw)-fact1*zw2*(yof1(k+1)**2-yof1(k)**2+zof1(k+1)**2-zof1(k)**2)/2*rho
         Shallow: if (fact2/=0) then
            a(j,nof1-ngap+2*nfs+2+iw)=a(j,nof1-ngap+2*nfs+2+iw)+fact2*waven/om*cmplx(dy,-sinw*dz)*zw2a*(0.,1.)
            frkr1(1,iw)=frkr1(1,iw)+fact2*zw2a*dz*rho
            frkr1(2,iw)=frkr1(2,iw)-fact2*zw2a*dy*rho
            frkr1(3,iw)=frkr1(3,iw)-fact2*zw2a*(yof1(k+1)**2-yof1(k)**2+zof1(k+1)**2-zof1(k)**2)/2*rho
         endif Shallow
      enddo WaveAngles
      Columns: do i=0,nof1-ngap+2*nfs-1
         a(j,i)=pqnb(yof1(k),zof1(k),yof1(k+1),zof1(k+1),yq(i),zq(i))
      enddo Columns
   enddo BodySegm

   !Free surface condition near to body
   FreeSSegmentsNear: do k=nof1,nof1+2*nf+1
      k1=merge(k-nf+nfs-1,k,k>nof1+nf)                                   !k1=k on y>0-side if k<=nof1+nf
      y1=merge(yof1(1),yof1(k1),k==nof1+nf+1)
      dy=yof1(k1+1)-y1
      AllSources: do i=0,nof1-ngap+2*nfs-1      !2-point integration (.316,.684) correct in case of k1=i
         a(k1-ngap,i)=pqnb(y1,zwl,yof1(k1+1),zwl,yq(i),zq(i))*merge(1,-1,k==k1) &
            +om**2/g*abs(dy)*(pqb(y1+0.316*dy,zwl,yq(i),zq(i))+pqb(y1+0.684*dy,zwl,yq(i),zq(i)))/2
      enddo AllSources
   enddo FreeSSegmentsNear

   !Free surface condition far from body (radiation condition)
   FarSegments: do k=nof1+nf+1,nof1+2*nfs-nf-2
      k1=merge(k+nf+1,k,k>=nof1+nfs)
      dy=yof1(k1+1)-yof1(k1)
      factor=(real(k1-(nof1+nf))/(nfs-nf))**2
      if (k/=k1) factor=(real(k1-(nof1+nfs+nf))/(nfs-nf))**2
      AllColumns: do i=0,nof1-ngap+2*nfs-1
         a(k1-ngap,i)=waven*abs(dy)* &
            (pqb(yof1(k1)+0.316*dy,zwl,yq(i),zq(i))+pqb(yof1(k1)+0.684*dy,zwl,yq(i),zq(i)))/2 &
            -(0.,1.)*(pqb(yof1(k1+1)-factor*dy,zwl,yq(i),zq(i))-pqb(yof1(k1)-factor*dy,zwl,yq(i),zq(i)))
      enddo AllColumns
   enddo FarSegments

   a(0,0:nof1-ngap+2*nfs-1)=1.                                           ! sum of source strengths = 0
   call simqcd(a(0,0),nof1-ngap+2*nfs,3+nmu,nofmax+1,is,1.e-5,detl)             !solve equation system
   if (is.ne.0) stop'*** Singular equation system. Coinciding offset points?'

   !Added mass and diffraction calc. by pressure integration over section contour
   Forces: do m=1,3                                 !for transverse force, vertical force, roll moment
      MotionsAndWaves: do l=1,3+nmu                                               !for sway, heave, roll
         f=0                                                                                        !force
         AllBodySegments: do k=1,nof1-1
            if (ngap>0.and.any(gap(1:ngap)==k)) cycle
            dy=yof1(k+1)-yof1(k)
            dz=zof1(k+1)-zof1(k)
            if (m==1) then
               fact=-dz                                                       !horizontal force
            elseif (m==2) then
               fact=+dy                                                     !vertical force
            else
               fact=+0.5*(yof1(k+1)**2-yof1(k)**2+zof1(k+1)**2-zof1(k)**2)                  !m==3; moment
            endif
            ForAllSources: do i=0,nof1-ngap+2*nfs-1
               f=f+a(i,nof1-ngap+2*nfs-1+l)*fact*(pqb(yof1(k)+0.316*dy,zof1(k)+0.316*dz,yq(i),zq(i)) &
                  +pqb(yof1(k)+0.684*dy,zof1(k)+0.684*dz,yq(i),zq(i)))/2               !/2 for two-point integr.
            enddo ForAllSources
         enddo AllBodySegments
         RadiationDiffraction: if (l<=3) then        !added mass = -force /acceleration; pr.=-rho i om phi
            addedm1(m,l)=addedm1(m,l)+0.5*ciom*f/(-om**2)*rho                     !*0.5 for average nf=25,28
         else radiationdiffraction
            diff1(m,l-3)=diff1(m,l-3)-0.5*ciom*f*rho
         endif RadiationDiffraction
      enddo MotionsAndWaves
   enddo Forces

   !Calculation of pressures
   WithPressure: if (npres>0) then
      Motions: do l=1,3+nmu
         Offsets: do k=1,nof1
            phiatoffsets(k)=0.
            Sources: do i=0,nof1-ngap+2*nfs-1
               phiatoffsets(k)=phiatoffsets(k)+a(i,nof1-ngap+2*nfs-1+l)*pqb(yof1(k),zof1(k),yq(i),zq(i))
            enddo Sources
         enddo Offsets
         PressurePoints: do ii=1,npres
            pr(ii,l)=pr(ii,l)-ciom*rho*(phiatoffsets(ileft(ii)-1)+  &            !pressure at pressure point
               aint(ii)*(phiatoffsets(ileft(ii))-phiatoffsets(ileft(ii)-1)))/2            !sum for 2 nf values
         enddo PressurePoints
      enddo Motions
   endif WithPressure
enddo TwoFreeSurfaceDiscretisations
end subroutine addedmassexcitationa

subroutine addedmassexcitations(nof1,yof1,zof1,om,ndof,addedm1,diff1,frkr1)
! added mass and wave excitation for deep and shallow water, symmetrical section
! bottom at zbot, surface at zwl. for deep water use zbot=1.e6. water density 1 assumed.
! complex added mass = mass-i/omegae*damping for vertical (ndof=1; 1x1 matrix) or
! horizontal/roll motion (ndof=2; 2x2 matrix) as well as vertical (ndof=1) diffraction
! and Froude-Krilow forces or horizontal force and roll moment (ndof=2)
! section offset points from basis (y=0)to port wl point
! +y to starboard; +z downwards. last point should be on waterline
integer, intent(in):: nof1             !number of offsets on one half section
integer:: ndof                         !number of degrees of freedom: 1 heave, 2 sway/roll
integer:: mp                           !+1 if ndof=1; -1 if ndof=2
integer:: i,j                          !indices
integer:: iw                           !index of wave angle
integer:: is                           !points to row if equation solver detects singularity
integer:: l                            !motion index
integer:: m                            !force or degree-of-freedom index
integer:: k                            !index of offset points on section
integer:: nf                           !number of free-surface panels (undamped)
real:: om                              !circular frequency
real:: yof1(nofmax),zof1(nofmax)       !y and z coordinates of offset points of one section
real:: yq(0:nofmax),zq(0:nofmax)       !y and z coordinates of sources
real:: t                               !water depth
real:: fact1,fact2,factor,fact         !factors
real:: h                               !point distance on free surface near to body
real:: hend                            !point distance on free surface far from body
real:: dy,dz                           !range of contour segment in y and z direction
real:: sinw                            !sin(wave angle)
complex:: a(0:nofmax,0:nofmax+3+nmumax)!coefficient matrix for source strengths determination
complex:: ciom                         !i*om
complex:: detl                         !log(determinant) of a
complex:: f(2,2+nmumax)                !force amplitude
complex:: addedm1(ndof,ndof)           !complex added mass matrix
complex:: diff1(ndof,nmu)              !diffraction force amplitude, one section
complex:: frkr1(ndof,nmu)              !Froude-Krilow force amplitude, one section
complex:: zw1,zw2,zw1a,zw2a            !intermediate values

if (nof1.gt.50) stop'*** Too many points on section. should be <=50'
if (ndof.ne.1.and.ndof.ne.2) stop'*** Incorrect ndof. should be 1 or 2'
if (nmu.gt.nmumax) stop'*** Too many angles'
if (nof1+nfs>nofmax) stop'*** Too many body + waterline panels'
addedm1=0.
diff1=0.
twofreesurfacediscretisations: do nf=25,28,3                   !uses two values nf; results averaged
   mp=3-2*ndof
   ciom=(0.,1.)*om
   t=zbot-zwl                                                                           !t=water depth
   waven=max(om**2/g,om/sqrt(g*t))
   if (waven*t<6) then
      do i=1,10                               !for shallow water: iterative determination of wave number
         waven=waven-(waven*tanh(waven*t)-om**2/g)/(tanh(waven*t)+waven/cosh(waven*t)**2)/2
      enddo
      fact1=g*exp( waven*t)/2/cosh(waven*t); fact2=g*exp(-waven*t)/2/cosh(waven*t)
   else
      fact1=g; fact2=0.
   endif
   h=sqrt((yof1(nof1)-yof1(nof1-1))**2+(zof1(nof1)-zof1(nof1-1))**2)      !near distance on free surf.
   hend=2*pi/(12*waven)                      !far-off distance of points on free surface=wavelength/12

   !Compute free surface grid points yof1,zof1 and all source points yq,zq
   i=0
   BodySegments: do k=1,nof1-1
      if (ngap>0.and.any(gap(1:ngap)==k)) cycle
      i=i+1
      yq(i)=(yof1(k)+yof1(k+1))/2-(zof1(k+1)-zof1(k))/20                   !source points within section
      zq(i)=(zof1(k)+zof1(k+1))/2+(yof1(k+1)-yof1(k))/20
   enddo BodySegments

   FreeSurfaceSegments: do k=nof1,nof1+nfs-1
      h=min(h*1.5,hend)
      yof1(k+1)=yof1(k)-h
      zof1(k+1)=zwl
      yq(k-ngap)=yof1(k)-0.5*h
      zq(k-ngap)=zwl-1.0*h
   enddo FreeSurfaceSegments

   yq(0)=0
   zq(0)=zwl-abs(yof1(nof1+nfs))/2
   a(0:nof1-ngap+nfs-1,nof1-ngap+nfs:nof1-ngap+nfs-1+ndof+nmu)=0.             !inhomogeneous terms = 0
   frkr1(1:ndof,1:nmu)=0

   !Body boundary condition (panel integrals) and Fr.-Krilow forces
   j=0
   BodySegm:do k=1,nof1-1
      if (ngap>0.and.any(gap(1:ngap)==k)) cycle
      j=j+1
      dy=yof1(k+1)-yof1(k)
      dz=zof1(k+1)-zof1(k)
      if (mp.eq.1) then
         a(j,nof1-ngap+nfs)=+ciom*dy
      else
         a(j,nof1-ngap+nfs  )=-ciom*dz                                 !inhomogeneous terms for body motion
         a(j,nof1-ngap+nfs+1)=+ciom*(yof1(k+1)**2-yof1(k)**2+zof1(k+1)**2-zof1(k)**2)/2
      endif
      WaveAngles: do iw=1,nmu
         sinw=sin(wangl(iw)*pi/180)
         zw1=cmplx(-waven*dz,waven*sinw*dy)
         if (abs(zw1)>50) stop'>> Frequency too high'
         if (abs(zw1).lt.0.001) zw1=0.001                       !stop'frequency too low' seems unnecessary
         zw1a=-conjg(zw1)
         zw2 =exp(waven*cmplx(-zof1(k)+zwl,yof1(k)*sinw))* merge((exp(zw1 )-1.)/zw1,1.+0.5*zw1,abs(zw1)>0.01)
         zw2a=exp(waven*cmplx( zof1(k)-zwl,yof1(k)*sinw))* merge((exp(zw1a)-1.)/zw1a,1.+0.5*zw1a,abs(zw1a)>0.01)
         if (mp.eq.1) then                                      !heave motion
            a(j,nof1-ngap+nfs-1+ndof+iw)=-fact1*waven/om*real(cmplx(dy,sinw*dz)*zw2)*(0.,1.)
            frkr1(1,iw)=frkr1(1,iw)-fact1*2*real(zw2 )*dy*rho
            Shallow: if (fact2/=0) then
               a(j,nof1-ngap+nfs-1+ndof+iw)=a(j,nof1-ngap+nfs-1+ndof+iw)+fact2*waven/om*real(cmplx(dy,-sinw*dz)*zw2a)*(0.,1.)
               frkr1(1,iw)=frkr1(1,iw)-fact2*2*real(zw2a)*dy*rho
            endif Shallow
         else                                                                            !sway/roll motion
            a(j,nof1-ngap+nfs-1+ndof+iw)=-fact1*waven/om*real(cmplx(-sinw*dz,dy)*zw2)
            frkr1(1,iw)=frkr1(1,iw)+fact1*(0.,2.)*aimag(zw2)*dz*rho
            frkr1(2,iw)=frkr1(2,iw)-fact1*(0.,1.)*aimag(zw2)*(yof1(k+1)**2-yof1(k)**2+zof1(k+1)**2-zof1(k)**2)*rho
            ShallowWater: if (fact2/=0) then
               a(j,nof1-ngap+nfs-1+ndof+iw)=a(j,nof1-ngap+nfs-1+ndof+iw)+fact2*waven/om*real(cmplx( sinw*dz,dy)*zw2a)
               frkr1(1,iw)=frkr1(1,iw)+fact2*(0.,2.)*aimag(zw2a)*dz*rho
               frkr1(2,iw)=frkr1(2,iw)-fact2*(0.,1.)*aimag(zw2a)*(yof1(k+1)**2-yof1(k)**2+zof1(k+1)**2-zof1(k)**2)*rho
            endif ShallowWater
         endif                                                                         !heave or sway/roll
      enddo WaveAngles
      Columns: do i=ndof-1,nof1-ngap+nfs-1
         a(j,i)=pqsn(yof1(k),zof1(k),yof1(k+1),zof1(k+1),yq(i),zq(i),mp)
      enddo Columns
   enddo BodySegm

   !Free-surface condition near to body
   FreeSSegmentsNear: do k=nof1,nof1+nf
      dy=yof1(k+1)-yof1(k)
      AllSources:do i=ndof-1,nof1-ngap+nfs-1            !2-point integration for correct integral at k=i
         a(k-ngap,i)=pqsn(yof1(k),zwl,yof1(k+1),zwl,yq(i),zq(i),mp) &
            +om**2/g*abs(dy)*(pqs(yof1(k)+0.316*dy,zwl,yq(i),zq(i),mp) &
            +pqs(yof1(k)+0.684*dy,zwl,yq(i),zq(i),mp))/2
      enddo AllSources
   enddo FreeSSegmentsNear

   !Free-surface condition far from body (radiation condition)
   FarSegments: do k=nof1+nf+1,nof1+nfs-1
      dy=yof1(k+1)-yof1(k)
      factor=(real(k-nof1-nf)/(nfs-nf-1))**2
      AllColumns: do i=ndof-1,nof1-ngap+nfs-1
         a(k-ngap,i)=waven*abs(dy)*(pqs(yof1(k)+0.316*dy,zwl,yq(i),zq(i),mp) &
                                +pqs(yof1(k)+0.684*dy,zwl,yq(i),zq(i),mp))/2 &
                       -(0.,1.)*(pqs(yof1(k+1)-factor*dy,zwl,yq(i),zq(i),mp) &
                                -pqs(yof1(k  )-factor*dy,zwl,yq(i),zq(i),mp))
      enddo AllColumns
   enddo FarSegments

   if (ndof==1) a(0,0:nof1-ngap+nfs-1)=1.                      !sum of sources = 0 only in case ndof=1
   call simqcd(a(ndof-1,ndof-1),nof1-ngap+nfs+1-ndof,ndof+nmu,nofmax+1,is,1.e-5,detl) !solve equations
   if (is.ne.0) stop'***  Singular equation system. Coinciding offset points?'

   Forces: do m=1,ndof                                             !ndof=1 heave, ndof=2 sway and roll
      MotionsAndWaves: do l=1,ndof+nmu                                    !m force index, l motion index
         f(m,l)=0
         AllBodySegments: do k=1,nof1-1                                             !k section point index
            if (ngap>0.and.any(gap(1:ngap)==k)) cycle
            dy=yof1(k+1)-yof1(k)
            dz=zof1(k+1)-zof1(k)
            if (mp.eq.1) then
               fact=+dy                                                        !heave motion
            else
               if (m.eq.1) fact=-dz
               if (m.eq.2) fact=+0.5*(yof1(k+1)**2-yof1(k)**2+zof1(k+1)**2-zof1(k)**2)
            endif
            ForAllSources: do i=ndof-1,nof1-ngap+nfs-1
               f(m,l)=f(m,l)+a(i,nof1-ngap+nfs-1+l)*fact* &
                      (pqs(yof1(k)+0.316*dy,zof1(k)+0.316*dz,yq(i),zq(i),mp) &
                      +pqs(yof1(k)+0.684*dy,zof1(k)+0.684*dz,yq(i),zq(i),mp))/2
            enddo ForAllSources
         enddo AllBodySegments
         if (l<=ndof) then
            addedm1(m,l)=addedm1(m,l)+ciom*f(m,l)/(-om**2)*rho
         else
            diff1(m,l-ndof)=diff1(m,l-ndof)-ciom*f(m,l)*rho
         endif          !/2 because of average for nf=25,28; *2 because of symmetry
      enddo MotionsAndWaves
   enddo Forces
enddo TwoFreeSurfaceDiscretisations
end subroutine addedmassexcitations

subroutine testsuitability(nof1,yof2,zof2)
! Tests whether body sources 1/20 panel length inside section contour are outside
! of a line 1/15 panel length inside section contour and not on other side of y=0
integer, intent(in):: nof1             !no. of offset points on one section
integer:: i,k                          !indices
integer:: nof1neu                      !no. of offset points including mirror image at y<0
integer:: ngapneu,gap1(10)             !no. and point indices of gaps including mirror image
real:: yof1(nofmax),zof1(nofmax)       !y, z of offset points, including mirror image
real:: yofi(nofmax),zofi(nofmax)       !y, z of points inside section contour
real:: dy,dz                           !coordinate differences of offset points
real:: yq,zq                           !y, z of sources
real:: yof2(nofmax),zof2(nofmax)       !y, z of original offset points (possibly of half section)
real:: angle                           !angle under which section contour is seen from a source
real:: ay,az,by,bz                     !y, z coordinate difference between source and offset point

if (sym) then                           !complement section by mirror image in fields yof1,zof1,gap1
   yof1(nof1:2*nof1-1)=yof2(1:nof1)
   zof1(nof1:2*nof1-1)=zof2(1:nof1)
   yof1(nof1-1:1:-1)=-yof1(nof1+1:2*nof1-1)
   zof1(nof1-1:1:-1)= zof1(nof1+1:2*nof1-1)
   nof1neu=2*nof1-1
   ngapneu=2*ngap
   if (ngap>0) then
      gap1(1:ngap)=gap(1:ngap)+nof1-1
      gap1(ngap+1:ngapneu)=nof1-gap(1:ngap)
   endif
else
   yof1(1:nof1)=yof2(1:nof1)                                                    !fill fields yof1,zof1
   zof1(1:nof1)=zof2(1:nof1)
   nof1neu=nof1
   ngapneu=ngap
   gap1(1:ngapneu)=gap(1:ngap)
endif
do i=1,nof1neu                                                     !generate interior contour points
   if (i==1.or.(ngapneu>0.and.any(gap1(1:ngapneu)==i-1))) then
      dy=yof1(i+1)-yof1(i)
      dz=zof1(i+1)-zof1(i)
   else if (i==nof1neu.or.(ngapneu>0.and.any(gap1(1:ngapneu)==i))) then
      dy=yof1(i)-yof1(i-1)
      dz=zof1(i)-zof1(i-1)
   else
      dy=(yof1(i+1)-yof1(i-1))/2
      dz=(zof1(i+1)-zof1(i-1))/2
   endif
   yofi(i)=yof1(i)-dz/15
   zofi(i)=zof1(i)+dy/15
enddo
Sources: do k=1,nof1neu-1 !for all source points
   if (ngapneu>0.and.any(gap1(1:ngapneu)==k)) cycle                                       !not at gaps
   yq=(yof1(k)+yof1(k+1))/2-(zof1(k+1)-zof1(k))/20
   zq=(zof1(k)+zof1(k+1))/2+(yof1(k+1)-yof1(k))/20
   if (yq*(yof1(k)+yof1(k+1))<abs((yof1(k)+yof1(k+1))* &
      (zof1(k+1)-zof1(k)))/30) stop '>> Unsuitable section discretisation 1'        !source too near to y=0
   angle=0
   Offsets: do i=1,nof1neu   !compute angle under which the closed section contour is seen from source
      if (i==1) then
         ay=yq-yof1(nof1neu)
         az=zq-zof1(nof1neu)
      else
         ay=by
         az=bz
      endif
      by=yq-yof1(i)
      bz=zq-zof1(i)
      angle=angle+atan2(ay*bz-az*by,ay*by+az*bz)
   enddo Offsets
   if (abs(angle-6.283)>1.) stop '>> Unsuitable section discretisation 2'
enddo Sources                                      !because source is too near or outside of contour
end subroutine testsuitability

function which(n,l)                      !first i for which l(i)=.true., i=1..n. 0 if l(1:n)=.false
integer:: n,which; logical, dimension(n):: l
do which=1,n
   if (l(which)) return
enddo
which=0
end function which


function betr2(c)
real:: betr2
complex:: c
betr2=c*conjg(c)
end function betr2

subroutine simqcd(a,n,nr,im,i,s,detl)
! Gauss algorithm for a complex linear equation system. Full matrix, several inhomogeneous vectors.
! a(row,column)=complex coefficient matrix + NEGATIVE right-hand sides as additional columns. When
! finished right-hand sides are replaced by the solutions. Coefficient matrix is destroyed.
! n=no. of equations (rows), nr no. of right-hand sides, im range of first index of a.
! i=0 after normal solutions, /=0 if a pivot is <s (singular or near-singular matrix a).
! detl ist ln(determinant of coefficient matrix). With column pivoting.
implicit complex (a-h,o-z)
real:: s
integer:: n,nr,im,i,j,k,nnr,mmr,k1,k2,imax
dimension a(*)
nnr=(n+nr)*im
mmr=n*im
k1=-im
detl=0.
do i=1,n
   k1=k1+im+1
   biga=a(k1)
   imax=k1
   do j=k1+1,k1+n-i
      if (abs(biga).lt.abs(a(j))) then
         biga=a(j)
         imax=j
      endif
   enddo
   if (abs(biga).lt.s) return
   detl=detl+log(biga)
   a(imax)=a(k1)
   do k=k1+im,nnr,im
      imax=imax+im
      save=-a(imax)/biga
      a(imax)=a(k)
      a(k)=save
      k2=k1
      do j=k+1,k+n-i
         k2=k2+1
         a(j)=a(j)+a(k2)*save
      enddo
   enddo
enddo
do i=mmr,nnr-1,im
   k1=mmr+1
   do j=n,2,-1
      k1=k1-im
      k2=i
      save=a(i+j)
      do k=k1,k1+j-2
         k2=k2+1
         a(k2)=a(k2)+a(k)*save
      enddo
   enddo
enddo
i=0
end subroutine simqcd

function fillcomplmatr(nr,nc,elements) !generates complex matrix from complex elements
integer:: nr,nc,ir,ic,i
complex:: fillcomplmatr(nr,nc),elements(nr*nc) !elements: 1st row, 2nd row ...
i=0
do ir=1,nr; do ic=1,nc
   i=i+1
   fillcomplmatr(ir,ic)=elements(i)
enddo; enddo
end function fillcomplmatr

function fillrealmatr(nr,nc,elements) !generates complex matrix from real elements
integer:: nr,nc,ir,ic,i
real:: elements(nr*nc) !elements: 1st row, 2nd row ...
complex:: fillrealmatr(nr,nc)
i=0
do ir=1,nr; do ic=1,nc
   i=i+1
   fillrealmatr(ir,ic)=elements(i)
enddo; enddo
end function fillrealmatr

function prodmatr(ma,mb)
integer:: nra,nca,nrb,ncb,i,j
complex, intent(in),dimension(:,:):: ma,mb
complex, dimension(size(ma,1),size(mb,2)):: prodmatr
nra=size(ma,1); nca=size(ma,2)
nrb=size(mb,1); ncb=size(mb,2)
if (nca/=nrb) stop'>> Incompatible matrices in prodmatr'
do i=1,nra; do j=1,ncb
   prodmatr(i,j)=sum(ma(i,1:nca)*mb(1:nrb,j))
enddo; enddo
end function prodmatr

function vectprod(a,b)
real,intent(in):: a(3),b(3)
real:: vectprod(3)
vectprod(1)=a(2)*b(3)-a(3)*b(2)
vectprod(2)=a(3)*b(1)-a(1)*b(3)
vectprod(3)=a(1)*b(2)-a(2)*b(1)
end function vectprod

subroutine writematr(name,matr) !writes matr below text 'name'
integer:: ir,nr
complex, dimension(:,:):: matr
character(*):: name
nr=size(matr,1)
write(*,*)name
do ir=1,nr
   write(*,'(6(2g10.3,1x))')matr(ir,:)
enddo
end subroutine writematr

function logmatr(ma,mb)           !log of each element of ma so that |ma(i,j)-mb(i,j)| becomes small
integer:: nra,nca,nrb,ncb,i,j,k                              !principal value of log if mb(1,1)=999.
real:: ar
complex, intent(in), dimension(:,:):: ma,mb
complex:: logmatr(size(ma,1),size(ma,2))
nra=size(ma,1); nca=size(ma,2)
nrb=size(mb,1); ncb=size(mb,2)
if (mb(1,1)/=(999.,0.).and.(nra/=nrb.or.nca/=ncb)) stop'Incompatible matrices in logmatr'
AllElements: do i=1,nra; do j=1,nca
   logmatr(i,j)=merge(log(ma(i,j)),(-60.,0.),ma(i,j)/=(0.,0.))
   if (mb(1,1)/=(999.,0.)) then                          !adapt phase angle for small difference to mb
      ar=aimag(logmatr(i,j)-mb(i,j))
      k=0
      do
         if (abs(ar)<=3.1416) exit                                    !intentionally a little more than pi
         k=k-sign(1.1,ar)                                 !k = no. of required 2pi phase angle corrections
         ar=ar-sign(2*pi,ar)
      enddo
      if (k/=0) logmatr(i,j)=logmatr(i,j)+cmplx(0.,k*2.*pi)
   endif
enddo; enddo AllElements
end function logmatr

subroutine signampl                              !Computes significant amplitudes in natural seaways
logical:: lfirst=.true.              !true for first seaway, then false
integer:: i                          !index
integer:: ifr, nfr                   !index and number of offsets of frequency valuation curve
integer:: nb                         !number of points where motions are determined
integer:: iom, nom                   !index and no. of frequencies in transfer function calc.
integer:: iv,nv                      !index and number of speeds in transfer function calc.
integer:: imu                        !index of wave angles in transfer function calc.
integer:: iart,nart                  !index and no. of transfer functions excl. pressures & drift
integer:: i1                         !index of acceleration quantities
integer:: is2                        !index of intersections (for sectional forces and moments)
integer:: ib                         !index of motion points
integer:: ipart,npart                !index and no. of sub-intervals within one wave angle interval
integer:: iommax(6*(nsemax+(nbmax*3)/2),nvmax) !frequency index of maximum of transfer function
real:: rla(nomma)                    !wave length
real:: vf(nvmax)                     !ship speeds
real:: ybetr2(6*(nsemax+(nbmax*3)/2),nvmax,2*nmumax) !square of response amplitude operators
real:: om(0:nomma+1)                 !wave circular frequencies
real:: ampken(6*(nsemax+(nbmax*3)/2),nvmax) !significant amplitudes
real:: bewf(30)                      !acceleration valuation factors
real:: muf(-nmumax:2*nmumax)         !wave angles
real:: muin(-nmumax:2*nmumax)        !integral of wave energy spreading function over wave angle
real:: fr(10)                        !frequencies for defining acceleration valuation function
real:: maxdampken(6*(nsemax+(nbmax*3)/2),nvmax) !max. contrib. of one frequency interval to ampken
real:: fxi(nvmax,nmumax)             !long. drift force in regular wave per wave amplitude^2
real:: feta(nvmax,nmumax)            !transverse drift force in regular wave per wave amplitude^2
real:: barfxi(nvmax)                 !long. drift force in a seaway
real:: barfeta(nvmax)                !transverse drift force in a seaway
real:: dotxdr(nvmax,nmumax)          !reduced long. drift velocity in a regular wave/wave ampl.^2
real:: dotydr(nvmax,nmumax)          !reduced transv. drift velocity in a regular wave/wave ampl.^2
real:: bardotxdr                     !reduced longitudinal water drift velocity in a seaway
real:: bardotydr                     !reduced transverse water drift velocity in a seaway
real:: ypr(nprmax,nsemax),zpr(nprmax,nsemax) !y and z of pressure points
real:: pres2(nprmax,nsemax,nvmax,nmumax) !pressure response amplitude squared
real:: h                             !significant wave height
real:: t                             !significant period corr. to c.o.gravity of wave spectrum
real:: mainmu                        !main direction of the seaway
real:: expon                         !e in angular spreading of wave energy acc. to cos^e(mu-mainmu)
real:: spuh                          !peak enhancement factor in Jonswap spectrum
real:: ompeak                        !circular frequency at peak of wave spectrum
real:: domsp                         !interval of circular frequency for spectrum integration
real:: sumrm                         !sum of mu integrals; used to normalize the mu integrals
real:: rmgl,rmbe                     !left end and range of wave angle interval
real:: omgl,ombe                     !left end and range of wave circular frequency interval
real:: mu                            !wave angle
real:: omzw                          !intermediate circular frequency within one frequency interval
real:: bvh                           !non-dim. breadth of peak enhancement in Jonswap spectrum
real:: omv                           !non-dimensional frequency in Jonswap spectrum
real:: sjonsw                        !Jonswap spectral ordinate
real:: rml                           !mass of suspended load
real:: skip                          !used to skip values when reading a file
real:: ome                           !encounter circular frequency
real:: fq                            !encounter frequency, ome/(2pi)
real:: fint                          !quantity for interpolation of acceleration valuation function
real:: bw                            !valuation factor for accelerations
real:: dampken                       !contribution to significant amplitude
real:: spi                           !integral of wave spectrum over frequency
real:: signpres(nprmax,nsemax,nvmax) !significant pressure amplitude
complex:: cpres(nprmax,nsemax)       !complex pressure amplitude
complex:: cskip                      !complex quantity to skip values when reading a file
character(80):: text1                !text describing the case

if (.not.lsign) stop
rewind 20; rewind 21; rewind 24
write(6,'(1x,131(1h-)/)')
write(6,'(/1x,1a80)') text
write(6,*) 'Significant amplitudes in natural seaways'
open(22,status='scratch',form='unformatted')
write(6,*)'Frequency    acceleration valuation factor'
read(5,*) nfr,(fr(ifr),bewf(ifr),ifr=1,nfr)
fr(1)=fr(1)+1.e-8
if (nfr.gt.30) stop'>> Too many frequencies'
write(6,'(f9.3,f20.3)') (fr(ifr),bewf(ifr),ifr=1,nfr)
if (npres>0) read(24,*) ((ypr(i,ise),zpr(i,ise),i=1,npres),ise=1,nse)

Seaways: do
   read(21,'(a)')text1                                                 !general data read from file 21
   if (text1.ne.text) then
      write(6,*)text1
      write(6,*)text
      stop'>> File 21 does not fit to the input data'
   endif
   read(21,*) nb,ls,g,rho,rml         !reads data for nmu,muf complemented to cover [-90 : 270] degrees
   read(21,*) nom,(rla(iom),iom=1,nom),nv,(vf(iv),iv=1,nv), &
              nmu,(muf(imu),imu=1,nmu),nse,(x(ise),ise=1,nse)      !rotations not divided by k as in pdstrip.out
   nart=6*(1+nb+merge(1,0,ls)*(nse-1))+2               !+1 for suspended weight rel., + 1 abs. motions
   read(5,*) h,t,mainmu,expon,spuh                                                  !reads seaway datda
   if (h.eq.0) stop
   write(6,*) '--------------------------------------------------------------------------'
   write(6,*)
   write(6,'('' Significant wave height                      '',F10.3)') h
   write(6,'('' Period T1 corr. to c.o.g. of spectrum        '',F10.3)') t
   write(6,'('' Central wave dir.(0 from stern, 90 from stb.)'',F10.3)') mainmu
   write(6,'('' Exponent n in cos^n angular spreading funct. '',F10.3)') expon
   write(6,'('' Peak enhancement factor (1 for P.-M.spectrum)'',F10.3)') spuh

   ! Preparations for integration
   ompeak=(4.65+0.182*spuh)/t
   write(6,'(/'' Peak period              '',f10.3)')6.283/ompeak
   domsp=0.025*ompeak
   do iom=1,nom
      waven=2.*pi/rla(iom)
      om(iom)=sqrt(waven*g*tanh(waven*(zbot-zwl)))
   enddo
   ampken(1:nart+3*nb,1:nv)=0.
   maxdampken(1:nart+3*nb,1:nv)=0
   if (npres>0) then                                                                !if pressure calc.
      signpres(1:npres,1:nse,1:nv)=0.
      barfxi(1:nv)=0; barfeta(1:nv)=0
      bardotxdr=0; bardotydr=0
   endif

   ! Determine integral over mu
   sumrm=0
   WaveAngles: do imu=1,nmu
      rmgl=(merge(muf(imu-1),muf(nmu)-2*pi,imu>1)+muf(imu))/2.               !left end of interval
      rmbe=(merge(muf(imu+1),muf(1)+2*pi,imu<nmu)+muf(imu))/2.-rmgl          !range of interval
      if (rmbe>0.78.and.abs(rmgl-mainmu*pi/180)<0.5*pi) then
         stop'>> The specified wave angles are not sufficient for this seaway'
      end if
      npart=rmbe/0.02+1
      muin(imu)=0
      Parts: do ipart=1,npart
         mu=rmgl+(ipart-0.5)*rmbe/npart
         if (    modulo(mainmu*pi/180-mu,2*pi).lt.pi/2. &
             .or.modulo(mainmu*pi/180-mu,2*pi).gt.pi*3/2.) then
            muin(imu)=muin(imu)+(cos(mainmu*pi/180-mu))**expon*rmbe/npart
         end if
      enddo Parts
      sumrm=sumrm+muin(imu)
   enddo WaveAngles
   muin(1:nmu)=muin(1:nmu)/sumrm


   ! Integration over omega for all responses
   om(0)=max(5.*ompeak-om(1),om(1))
   om(nom+1)=min(1.2*ompeak-om(nom),om(nom))
   Frequencies: do iom=1,nom
      omgl=(om(iom+1)+om(iom))/2             !left end of interval
      ombe=(om(iom-1)+om(iom))/2.-omgl       !range of interval
      npart=ombe/domsp+1                                                           !no. of sub-intervals
      spi=0                                                                      !spectrum part integral
      SubIntervals: do ipart=1,npart                !integration of wave spectrum from omgl to omgl+ombe
         omzw=omgl+(ipart-0.5)*ombe/npart
         bvh=merge(0.09,0.07,omzw>ompeak)
         omv=omzw/ompeak
         sjonsw=h**2*t*(177.5-6.52*spuh)/(t*omzw)**5*exp(-1.25/omv**4)
         if (abs(omv-1.).lt.0.36) sjonsw=sjonsw*spuh**exp(-(omv-1)**2/(2.*bvh**2))
         spi=spi+sjonsw*ombe/npart      !=wave spectrum, integral over current omega interval
      enddo SubIntervals
      Speeds: do iv=1,nv
         Angles: do imu=1,nmu
            if (lfirst) then      !for first seaway: read transfer functions for all angles
               read(21,*) (ybetr2(iart,iv,imu),iart=1,nart)
               write(22) (ybetr2(iart,iv,imu),iart=1,nart)                 !write into unformatted scratch file
               if (npres>0) then
                  read(24,*) ((skip,i=1,4),(cpres(i,ise),cskip,cskip,i=1,npres),ise=1,nse)
                  pres2(1:npres,1:nse,iv,imu)=abs(cpres(1:npres,1:nse))**2
                  read(24,*) fxi(iv,imu),feta(iv,imu),dotxdr(1,imu),dotydr(iv,imu)
                  write(22) pres2(1:npres,1:nse,iv,imu),fxi(iv,imu),feta(iv,imu),dotxdr(1,imu),dotydr(iv,imu)
               endif
            else  !for further seaways: read transfer functions from unformatted scratch file (faster)
               read(22) (ybetr2(iart,iv,imu),iart=1,nart)
               read(22) pres2(1:npres,1:nse,iv,imu),fxi(iv,imu),feta(iv,imu),dotxdr(1,imu),dotydr(iv,imu)
            endif
         enddo Angles
         WaveDirection: do imu=1,nmu                                         !Integration over wave angles
            if (muin(imu).eq.0.) cycle
            ome=om(iom)-2.*pi/rla(iom)*vf(iv)*cos(muf(imu))
            fq=abs(ome)/2/pi                                                 !for valuation of accelerations
            ifr=which(nfr,fq<fr(1:nfr))
            if (ifr==0) stop'Frequency range of acceleration valuation function unsufficient'
            ifr=max(ifr,2)
            fint=(log(fq)-log(fr(ifr-1)))/(log(fr(ifr))-log(fr(ifr-1)))
            bw=exp(log(bewf(ifr-1))+fint*(log(bewf(ifr))-log(bewf(ifr-1))))
            if (abs(ybetr2(1,iv,imu)-999.**2)<10) cycle                !skip case (iom,iv,imu) if surfriding
            VariousResponses: do iart=1,nart
               dampken=spi*ybetr2(iart,iv,imu)*muin(imu)
               ampken(iart,iv)=ampken(iart,iv)+dampken
               maxdampken(iart,iv)=max(maxdampken(iart,iv),dampken)
               if (maxdampken(iart,iv)==dampken) iommax(iart,iv)=iom
               i1=nart+iart-8                !for accelerations: there are no acceleration transfer functions!
               if (iart.gt.8.and.iart.le.3*(nb+2)+2) then
                  ampken(i1,iv)=ampken(i1,iv)+dampken*ome**4*bw**2
                  maxdampken(i1,iv)=max(maxdampken(i1,iv),dampken*ome**4*bw**2)
                  if (maxdampken(i1,iv)==dampken*ome**4*bw**2) iommax(i1,iv)=iom
               endif
            enddo VariousResponses
            WithPressure: if (npres>0) then
               signpres(1:npres,1:nse,iv)=signpres(1:npres,1:nse,iv)+ &
                  spi*pres2(1:npres,1:nse,iv,imu)*muin(imu)                            !signif. ampl. of pressure
               barfxi (iv)=barfxi (iv)+spi*fxi (iv,imu)*muin(imu)                    !longitudinal drift force
               barfeta(iv)=barfeta(iv)+spi*feta(iv,imu)*muin(imu)                      !transverse drift force
               if (iv==1) then
                  bardotxdr=bardotxdr+spi*dotxdr(1,imu)*muin(imu)                          !long. drift velocity
                  bardotydr=bardotydr+spi*dotydr(iv,imu)*muin(imu)                        !transv.drift velocity
               endif
            endif WithPressure
         enddo WaveDirection
      enddo Speeds
   enddo Frequencies

   !Print results
   Speed: do iv=1,nv
      write(6,'(/'' Ship speed    '',f10.3)') vf(iv)
      write(6,'(74(1h-))')
      SuspendedWeight: if (rml.ne.0.) then
         write(6,'('' Suspended weight, rel. transv. motion   '',f20.3)') 2.*sqrt(ampken(7,iv))
         call warning(ampken(7,iv),maxdampken(7,iv),iommax(7,iv),rla,nom,1)
         write(6,'('' Suspended weight, abs. transv. motion   '',f20.3)') 2.*sqrt(ampken(8,iv))
         call warning(ampken(8,iv),maxdampken(8,iv),iommax(8,iv),rla,nom,1)
      endif SuspendedWeight
      write(6,'(/'' Direction  '',3i20)') 1,2,3
      write(6,'(/'' Translation  '',3f20.3)') (2.*sqrt(ampken(iart,iv)),iart=1,3)
      do iart=1,3
         call warning(ampken(iart,iv),maxdampken(iart,iv),iommax(iart,iv),rla,nom,iart)
      enddo
      write(6,'('' Rotation in degr'',f17.3,2f20.3)') (2.*180./pi*sqrt(ampken(iart,iv)),iart=4,6)
      do iart=4,6
         call warning(ampken(iart,iv),maxdampken(iart,iv),iommax(iart,iv),rla,nom,iart-3)
      enddo
      MotionPoints: do ib=1,nb
         if (ib.eq.1) write(6,'(/'' Absolute motion  '')')
         write(6,'('' at point'',i3,''  '',3f20.3)') ib,(2.*sqrt(ampken(iart,iv)),iart=6+3*ib,8+3*ib)
         do iart=6+3*ib,8+3*ib
            call warning(ampken(iart,iv),maxdampken(iart,iv),iommax(iart,iv),rla,nom,iart-5-3*ib)
         enddo
      enddo MotionPoints
      Accelerations: do ib=1,nb
         if (ib.eq.1) write(6,'(/'' Accelerations   '')')
         write(6,'('' at point'',i3,''  '',3f20.3)')ib,(2.*sqrt(ampken(nart+iart-8,iv)),iart=6+3*ib,8+3*ib)
         do iart=6+3*ib,8+3*ib
            call warning(ampken(nart+iart-8,iv),maxdampken(nart+iart-8,iv),iommax(nart+iart-8,iv),rla,nom,iart-5-3*ib)
         enddo
      enddo Accelerations
      RelativeMotions: do ib=1,nb
         if (ib.eq.1) write(6,'(/'' Relative motions '')')
         write(6,'('' at point'',i3,''  '',3f20.3)') ib,(2.*sqrt(ampken(iart,iv)),iart=6+3*ib+3*nb,8+3*ib+3*nb)
         do iart=6+3*ib+3*nb,8+3*ib+3*nb
            call warning(ampken(iart,iv),maxdampken(iart,iv),iommax(iart,iv),rla,nom,iart-5-3*ib-3*nb)
         enddo
      enddo RelativeMotions
      ForceMoment: do is2=1,merge(1,0,ls)*(nse-1)
         if (is2.eq.1) write(6,'(/'' Force and moment at transverse intersections''/11x,''x'')')
         write(6,'('' Force '',f7.2,3f20.3)')(x(is2)+x(is2+1))/2.,(2.*sqrt(ampken(iart,iv)),iart=3+6*(nb+is2),5+6*(nb+is2))
         do iart=3+6*(nb+is2),5+6*(nb+is2)
            call warning(ampken(iart,iv),maxdampken(iart,iv),iommax(iart,iv),rla,nom,iart-2-6*(nb+is2))
         enddo
         write(6,'('' Moment'',7x,3f20.3)') (2.*sqrt(ampken(iart,iv)),iart=6+6*(nb+is2),8+6*(nb+is2))
         do iart=6+6*(nb+is2),8+6*(nb+is2)
            call warning(ampken(iart,iv),maxdampken(iart,iv),iommax(iart,iv),rla,nom,iart-5-6*(nb+is2))
         enddo
      enddo ForceMoment
      IfPressures: if (npres>0) then
         Pressures: do ise=1,nse
            write(6,'(a,i5,a,f10.3)') ' Pressure on section no.',ise,' at x=',x(ise)
            write(6,'(13f10.3)') 2*sqrt(signpres(1:npres,ise,iv))
         enddo Pressures
         write(6,'(a,f10.3)') ' Longitudinal drift force         ',2*barfxi(iv)
         write(6,'(a,f10.3)') ' Transverse drift force           ',2*barfeta(iv)
         write(6,'(a,f10.3)') ' Longitudinal water drift velocity',2*bardotxdr
         write(6,'(a,f10.3)') ' Transverse water drift velocity  ',2*bardotydr
      endif IfPressures
   enddo Speed
   rewind 21; rewind 22                                                               !for next seaway
   lfirst=.false.
enddo Seaways
end subroutine signampl

subroutine warning(ampken,maxdampken,iommax,rla,nom,i)
implicit none                                   !warns if accuracy of omega integration questionable
integer n,i,iommax,nom
real ampken,maxdampken,rla(*)
if (maxdampken/ampken>0.1) then
   write(6,'(a,i2,a,f10.3)') '*** result no.',i,' Inaccurate due to peak at wave length',rla(iommax)
end if
if (iommax<=2) then
   write(6,'(a,i2,a)')'*** result no.',i,' Inaccurate: shorter waves required.'
end if
if (iommax>=nom-1) then
   write(6,'(a,i2,a)')'*** result no.',i,' Inaccurate: longer waves required.'
end if
end subroutine warning

end module pdstripsubprograms

program pdstrip         !public domain strip method for motions, loads and added resistance in waves
use pdstripsubprograms
implicit none
logical:: iniExists             !used to make GUI in Windows work
logical:: ltrans                !t to compute transfer functions; f otherwise
logical:: ltrwet(nvmax)         !true if transom is wetted at the respective speed

integer,parameter:: nfinmax=20  !max. no. of fins
integer,parameter:: nsailmax=10 !max. no. of sails
integer,parameter:: nforcemax=10!max. no. of motion-dependant forces
integer:: i,j,j1,j2,k           !indices
integer:: ifre,nfre             !index and no. of frequencies of section hydrodynamic calculations
integer:: is,ns                 !index and no. of intersections
integer:: ifin,nfin             !index and number of fins
integer:: isail,nsail           !index and number of sails
integer:: nforce                !number of motion-dependent forces
integer:: ib,nb                 !index and number of points where motions should be determined
integer:: im,imu                !index of wave angle
integer:: iskip                 !used to skip values when reading a file
integer:: i1,i2                 !intermediate integer values
integer:: iom, nom              !index and no. of frequencies in transfer function calc.
integer:: iv,nv                 !index and no. of speeds in transfer function calc.
integer:: iomesec               !index of encounter frequency (section hydrodynamic calculations)
integer:: ioma,iomb,iomc        !indices of wave frequency (section hydrodynamic calculations)
integer:: itz                   !iteration counter
integer:: ks                    !indicator for singular coefficient matrix in simqcd
integer:: iab(nsemax)           !1 if flow separation between respective section and nex forward
integer:: imcorr(nmumax)        !wave angle index for quartering waves corr. to actual waves

real:: omsec(nfremax)           !circular frequency for section hydrodynamics
real:: mu(nmumax)               !wave angle
real:: wavelength(nomma)        !wave length
real:: speed(nvmax)             !list of ship speeds
real:: lfin(nfinmax)            !fin lenght
real:: addedmassfin(nfinmax)    !added mass of fin
real:: bfin(nfinmax),skorfin(nfinmax) !intermediate quantities for fin force determination
real:: xfin(3,nfinmax)          !3 coordinates of fin force center
real:: afin(3,nfinmax)          !fin axis direction
real:: cfin(nfinmax)            !chord length of fin
real:: cmfin(nfinmax)           !added mass coefficient of fin
real:: clgrfin(nfinmax)         !d(lift coefficient)/d(attack angle) of fin
real:: rfin(nfinmax)            !hull influence factor for fin lift
real:: sfin(nfinmax)            !hull influence factor for fin angle of attack
real:: cdfin(nfinmax)           !transverse drag coefficient of fin
real:: vll(3,3)                 !lower left part of V matrix for intersection forces
real:: mass(nsemax)             !mass of ship or ship part in front of an intersection
real:: xg(nsemax),yg(nsemax),zg(nsemax) !coordinates of mass center of gravity
real:: xb(nbmax),yb(nbmax),zb(nbmax) !coordinates of motion points
real:: cdy(nsemax),cdz(nsemax)  !transverse and vertical, resp., ship section resistance coeff.
real:: rdyzdx(3*nprmax)         !intermediate values to compute the potential at pressure points
real:: aforce(3,nforcemax)      !direction vector of motion-dependent force
real:: xforce(3,nforcemax)      !location of motion-dependent force
real:: kd                       !wave number times water depth
real:: yint(nprmax,nsemax),zint(nprmax,nsemax) !y and z coordinates of pressure points
real:: maxheight                !maximum wave height for quadratic responses
real:: uwind(3)                 !apparent wind vector
real:: xsail(3,nsailmax)        !coordinates of sail force point of attack
real:: csail(3,nsailmax)        !chord vector of sail
real:: msail(3,nsailmax)        !mast vector of sail
real:: dcndasail(nsailmax)      !sail lift coefficient per angle of attack
real:: xsail1(3,nsailmax)       !coordinates of point where the sail angle of attack is determined
real:: ssail(3,nsailmax)        != csail x msail
real:: ssailabs(nsailmax)       !absolute value of ssail
real:: cmsail(nsailmax)         !added mass coefficient of sail
real:: thxx,thyy,thzz,thxy,thyz,thxz !mass-weighed average values of x^2, y^2,z^2,xy,yz,xz resp.
real:: steepness                !wave steepness (used to determine cross-flow forces)
real:: dx                       !distance between successive sections
real:: yleft,yright             !y coordinates of section at waterline on port and starboard side
real:: b1,b2,b3                 !integral of 1, y, y^2 resp. over waterline breadth at a section
real:: az                       !z moment of section area
real:: zw                       !various intermediate real values
real:: rnf2,rnf3                !y and z, respectively, components of normal vector on fin
real:: uw                       !true wind speed
real:: muw                      !direction of true wind relative to wave propagation
real:: rml                      !mass of suspended weight
real:: xl,yl,zl                 !coordinates of suspension point
real:: cablelength              !cable length of suspended weight
real:: testnumber               !used to test whether stored section data fit to input data
real:: waveheight               !wave height for nonlinearities in transfer functions
real:: vs                       !ship speed
real:: cosm,sinm                !cos(mu) and sin(mu)
real:: om                       !wave circular frequency
real:: ome                      !encounter circular frequency
real:: omlow                    !lower limit of om
real:: fint1b,fint2b            !interpol. factors for encounter frequency (radiation quantities)
real:: fint1a,fint2a            !interp. factors for wave frequency (diffraction quantities)
real:: fakta                    !interpolation factor for diffraction
real:: fint1c,fint2c,faktc      !interpolation factors for potential (pressure)
real:: ser,serold               !error quantities to balance old, new results in nonlinear iteration
real:: vcross                   !cross-flow velocity
real:: longaddmass              !longitudinal added mass
real:: vfin                     !velocity at fin
real:: uwindabs                 !absolute value of wind velocity
real:: baralpha                 !average angle of attack of sail
real:: cn                       !normal force coefficient of sail
real:: err                      !error in motions during iteration
real:: vcompar                  !max. speed (stationary + periodic) in wave propagation direction
real:: xs                       !x of intersection (midway between offset sections)
real:: fxi,feta                 !longitudinal and transverse drift force
real:: dystb,dyprt              !change of y at waterline starbd, port between successive sections
real:: xdotdr                   !water particle reduced drift velocity in wave direction
real, dimension(3):: lu,lo,ru,ro !lower left, upper left, lower right, upper right point of patch
real, dimension(3):: d1,d2      !patch diagonals
real, dimension(3,1):: fl       !patch area vector (normal on patch)
real, dimension(3,1):: xaverage !patch center

complex,parameter:: ci=(0.,1.)  !imaginary unit
complex:: BNVtemp1(6,1)         !BNV/2006-05-08: additional code to facilitate Salford FTN95 compiler
complex:: BNVtemp2(3,1)         !BNV/2006-05-08: additional code to facilitate Salford FTN95 compiler
complex:: massmatr(6,6,nsemax)  !mass matrix, total ship and in front of each intersection
complex:: restorematr(6,6,nsemax+1) !restoration matrix
complex:: vsmatr(6,6,nsemax)    !V matrix for intersection forces and moments
complex:: ciome                 !i omega_e
complex:: cik                   !i times wave number
complex:: calda                 !intermediate result in longitudinal force determination
complex:: cfsece(3,3+nmumax)    !complex amplitude of radiation and diffraction force
complex:: cfksece(3,nmumax)     !complex amplitude of Froude-Krilow force
complex:: czeta3(nsemax+1)      !complex amplitude of wave at point (x,0,0)
complex:: czw,cvfw              !intermediate complex results
complex:: cpr(3*nprmax)         !complex amplitude of radiation pressure
complex:: cpd(nprmax*nmumax)    !complex amplitude of diffraction pressure
complex:: calfin(6,nfinmax)     !fin steering parameters: turning angle per motion
complex:: cdetl                 !logarith of determinant of equation system matrix
complex:: wdy,wdz               !wave orbital velocity in y and z direction, resp.
complex:: cpfk(nprmax)          !Froude-Krilow wave pressure amplitude
complex:: cpaveragers(6)        !average radiation pressure amplitude on a patch for 6 motions
complex:: cpaveragew            !patch average wave pressure amplitude
complex:: cpfkbow(nprmax)       !Froude-Krilow pressure amplitude before bow
complex:: cvfs                  !velocity amplitude of a fin due to ship motion
complex:: calforce(7,nforcemax) !motion-dependent force per motion amplitude & load transv. ampl.
complex:: cym,cymabs            !transverse motion ampl. of suspended load rel. to ship and earth
complex:: sailfactor(3)         !intermediate result for sail force
complex:: cxis,cetas,czetas     !motion of mass center of gravity in 3 directions
complex:: cskip                 !used to skip values in read statements
complex:: zwmatr61(6,1),zwmatr62(6,2),zwmatr63(6,3),zwmatr33(3,3) !intermediate matrix results
complex:: restorekorrmatr(6,6)  !correction of restoring matrix for non-wetted transom
complex:: wfinmatr(1,6,nfinmax) !W matrix for fin (changes ship motion to fin motion)
complex:: cfinmatr(1,6,nfinmax) !matrix for fin forces due to ship motion
complex:: wsailmatr(3,6,nsailmax) !W matrix for sail (changes ship motion to sail motion)
complex:: rhovsail(6,1,nsailmax)!intermediate matrix to determine sail force due to ship motion
complex:: vforcematr(6,1,nforcemax) !V matrix for motion-dependent force
complex:: alforcematr(1,6)      !matrix of force steering constants
complex:: bforcematr(6,6,nforcemax) !contribution of motion-dependent force to the added-mass matrix
complex:: wbmatr(3,6,nbmax)     !W matrix for motion points (point motion per ship motion)
complex:: fsec(3,3,nsemax,0:nfremax) !radiation force matrix of sections
complex,allocatable:: prsec(:,:,:,:) !matrix of radiation pressures in section calculations
complex,allocatable:: pdsecv(:,:,:)  !matrix of diffraction pressures, 1 section, 1 frequency
complex,allocatable:: pdsec(:,:,:,:,:) !matrix of diffraction pressures
complex:: fsecev(3,1,nmumax)    !matrix of diffraction force of a section
complex:: fksecev(3,1,nmumax)   !matrix of Froude-Krilow force of a section
complex:: fsece(3,1,nmumax,nsemax,0:nfremax) !matrix of diffr. force of sections, all frequencies
complex:: fksece(3,1,nmumax,nsemax,0:nfremax) !matrix of Fr.-Kr. force of sections, all frequencies
complex:: ex(6,1,nsemax)        !matrix of excitation
complex:: awprev(3,6)           !matrix of radiation force of a section times W, previous section
complex:: geprev(3,1)           !diffraction force of previous section
complex,allocatable:: prwprev(:,:) !matrix of rad. pressure*w of previous section
complex:: motion(6,1)           !ship motion amplitudes per unit wave amplitude
complex:: motion1(6,1)          !motions ampl. of previous iteration step for the given wave height
complex:: addedmass(6,6,nsemax) !added mass matrix of ship and parts in front of intersection*ome^2
complex:: addedmasschge(6,6)    !change of total added mass matrix due to one section
complex:: exchge(6,1)           !change of total excitation due to one section
complex:: fprou(6,6)            !motion forces in modified motion computation
complex:: fprouf(6,6)           !fin force due to ship motions in modified motion computation
complex:: fwithoutu(6,1)        !forces without motion contribution in modified motion computation
complex:: ncrossold(3,3,nsemax) !cross-flow damping, previous iteration step
complex:: ncross(3,3,nsemax)    !cross-flow damping
complex:: ecrossold(3,1,nsemax) !cross-flow excitation, previous iteration step
complex:: ecross(3,1,nsemax)    !cross-flow excitation
complex,allocatable:: pwg(:,:,:)!wave pressure
complex,allocatable:: prg(:,:,:)!radiation pressure
complex,allocatable:: pst(:,:,:)!statical pressure
complex,allocatable:: prs(:,:,:)!sum of pressure contributions
complex:: wmatr(3,6,nsemax)     !W matrix for a section (change of ship motion to section motion)
complex:: fdi(3,1)              !diffraction matrix of a section
complex,allocatable:: pdi(:,:)  !diffraction pressure of a section
complex,allocatable:: pw(:,:,:) !wave pressure of a section
complex:: fki(3,1)              !Froude-Krilow force of a section
complex:: vmatr(6,3,nsemax)     !matrix V (change of section force to ship force)
complex:: uvmatr(6,1)           !matrix for longitudinal forces, see theory: U_1
complex,allocatable:: pwprev(:,:) !wave pressure, previous section
complex,allocatable:: phi0(:,:) !derivatives of phi_0
complex,allocatable:: pri(:,:    ) !radiation pressure/ome^2
complex:: amatr(3,3)            !added mass matrix
complex:: awmatr(3,6)           !A.W
complex:: dawmatr(3,6)          !Difference of A.W between successive sections
complex,allocatable:: prw(:,:,:)!wave pressures
complex,allocatable:: dprw(:,:) !Difference of prw between successive sections
complex:: ez23(3,1)             !diffractions force
complex:: dge(3,1)              !difference of ez23 between successive sections
complex,allocatable:: dpw(:,:)  !difference of wave pressure between successive sections
complex,allocatable:: phi0w(:,:)!phi0 times W
complex:: zwmatr66(6,6),zwmatr22(2,2),zw1matr61(6,1) !intermediate matrices
complex:: uu(2,3)               !matrix used for cross-flow force determination
complex:: alfin(1,6)            !axis direction of fin
complex:: udyz(2,1)             !cross-flow velocity horizontal and vertical
complex:: bfinold(6,6,nfinmax)  !complex added mass matrix of fins, previous iteration
complex:: efinold(6,1,nfinmax)  !complex excitation matrix of fins, previous iteration
complex:: bfinnew(6,6)          !complex added mass matrix of fins, current iteration
complex:: addedmfin(6,6)        !added mass matrix due to fin
complex:: efinnew(6,1)          !excitation force due to fin, current iteration
complex:: exfin(6,1)            !excitation force due to fin
complex:: szw(6,6)              !restoring matrix incl. transom correction
complex:: lineqmatr(6,7)        !coefficient matrix of linear equation system for motions, 6x6
complex:: loadcol(6,1)          !additional column for suspended load in coefficient matrix
complex:: loadrow(1,7)          !additional row for suspended load in coefficient matrix
complex:: lineq7(7,8)           !coefficient matrix for motions including suspended load, 7x8
complex:: xib(3,1,nbmax)        !motion of motion points in the inertial system
complex:: zww(3,1)              !orbital motion of water particle
complex:: xiw4(3,1)             !orbital motion amplitude at a motion point
complex:: relmotion(3,1)        !relative motion of a motion point
complex:: sectionforce(6,1)     !intersecction force and moment
complex:: weightmatr(6,6)       !force, moment on total ship due to weight in modified motion calc.
complex:: flmo(6,1)             !force and moment of a pressure panel
complex:: motionmod(6,1)        !modified motions for pressure correction
complex,allocatable:: pres(:,:,:) !complex pressure amplitudes
complex,allocatable:: pot(:,:)  !complex amplitudes of potential at pressure points

character(80):: text1,skiptext

INQUIRE(file='pdstrip.ini',exist=iniExists)   !used to make GUI in Windows work
IF (iniExists) THEN
   OPEN(10,FILE='pdstrip.ini',STATUS='OLD')
   READ(10,'(a, /, a)') pathName, fileName    ! GT/2006-04-28
   CLOSE(10)
END IF

open(5,file=TRIM(pathName)//TRIM(fileName)//'.inp',status='old')    ! GT/2006-04-28
open(6,file=TRIM(pathName)//TRIM(fileName)//'.out',status='replace')    ! GT/2006-04-28
read(5,*) npres,lsect,ltrans,lsign
write(6,'(a,i3)') ' No. of pressure points per section ',npres
write(6,'(a,a )') ' Section hydrodynamic data?         ',merge('yes',' no',lsect)
write(6,'(a,a )') ' Transfer functions?                ',merge('yes',' no',ltrans)
write(6,'(a,a )') ' Significant amplitudes?            ',merge('yes',' no',lsign)
if (npres.gt.50) stop'>> Too many pressure points per section'
if (lsect.and..not.ltrans.and.lsign) stop'>> Illegal combination of lsect,ltrans,lsign'
open(20,file=TRIM(pathName)//'sectionresults',status=merge('old    ','replace',.not.lsect))
open(21,file=TRIM(pathName)//'responsefunctions',status=merge('old    ','replace',.not.ltrans.and.lsign))
open(23,file=TRIM(pathName)//'relativemotions',status=merge('old    ','replace',.not.ltrans.and.lsign))
open(24,file=TRIM(pathName)//'pressuretransfer',status=merge('old    ','replace',.not.lsect))
npres1=max(npres,1)
allocate(prsec(npres1,3,nsemax,0:nfremax),phi0(npres1,3),pwprev(npres1,1),pw(npres1,1,nsemax))
allocate(pdi(npres1,1),prg(npres1,6,nsemax),pwg(npres1,1,nsemax),prwprev(npres1,6))
allocate(prs(npres1,6,nsemax),pres(npres1,1,nsemax),pot(npres1,1))
allocate(pdsec(npres1,1,nmumax,nsemax,0:nfremax),pdsecv(npres1,1,nmumax),pri(npres1,3))
allocate(prw(npres1,6,nsemax),dprw(npres1,6),dpw(npres1,1),phi0w(npres1,6),pst(npres1,6,nsemax))
call sectiondata
if(.not.ltrans.and..not.lsign)stop                                                                     !Soe110406
write(6,'(131(1h-))')                !calculation of transfer functions (responses in regular waves)
rewind 20; rewind 24
area(nse+1)=0.
write(6,'(a)') ' In the following input data x,y,z are directed forward, port, up'
read(20,'(a)') text1
if (trim(adjustl(text1)).ne.trim(adjustl(text))) then
    write(6,*) text1; write(6,*)text; stop'File 20 does not fit to the input data'
endif
write(6,'(/1x,1a80)')text
read(20,*)nfre
read(5,*)ls
write(6,*)'Intersection forces? ',merge('yes','no ',ls)
ns=merge(nse,1,ls)
write(6,*) 'Mass data of total ship:'
write(6,'(a)') "  Inter-    mass    center of gravity at     inertial averages relative to cog"
write(6,'(a)') " Section               x       y       z     yy+zz     xx+zz     xx+yy        xy        yz        xz"
Intersections: do is=1,ns
   read(5,*)mass(is),xg(is),yg(is),zg(is),thxx,thyy,thzz,thxy,thyz,thxz
   if (is==2) write(6,*)'Mass data of ship part in front of intersection'
   write(6,'(i5,f11.3,3f8.3,6f10.3)')is-1,mass(is),xg(is),yg(is),zg(is),thxx,thyy,thzz,thxy,thyz,thxz
   yg(is)=-yg(is); zg(is)=-zg(is)                                      !change to interior coordinates
   thxy=-thxy; thxz=-thxz                                              !change to interior coordinates
   thxx=(thxx+yg(is)**2+zg(is)**2)
   thyy=(thyy+xg(is)**2+zg(is)**2)                       !moments of inertia relating to coord. origin
   thzz=(thzz+xg(is)**2+yg(is)**2)
   thxy=(thxy+xg(is)*yg(is))
   thyz=(thyz+yg(is)*zg(is))                      !mixed moments of intertia relating to coord. origin
   thxz=(thxz+xg(is)*zg(is))
   massmatr(:,:,is)=fillrealmatr(6,6,(/             &                            !generate mass matrix
      1.,   0.,     0.,    0.,     zg(is), -yg(is), &
      0.,   1.,     0.,  -zg(is),   0.,     xg(is), &
      0.,   0.,     1.,   yg(is), -xg(is),   0.   , &
      0., -zg(is), yg(is), thxx,  -thxy,   -thxz  , &
     zg(is),0.,  -xg(is), -thxy,   thyy,   -thyz  , &
    -yg(is), xg(is), 0.,  -thxz,  -thyz,    thzz/))*mass(is)
enddo Intersections

read(5,*) (iab(ise),ise=1,nse)
write(6,'(/a)')' Section        x          yb          zb        area     breadth  flow sep. in front?' !Soe110406
write(6,'(i5,5f12.3,5x,i2)') (ise,x(ise),-ys(ise),-zs(ise),area(ise),bcross(ise),iab(ise),ise=1,nse)
x(0)=x(1); ys(0)=ys(1); zs(0)=zs(1)
area(0)=-area(1)
x(nse+1)=x(nse); ys(nse+1)=ys(nse); zs(nse+1)=zs(nse)
read(5,*) steepness,maxheight                  !read data for quadratical damping forces on sections
write(6,'(/'' Wave steepness, quadr. effects'',f10.3)')steepness
write(6,'( '' but wave height not exceeding '',f10.3)')maxheight
if (steepness.gt.0) then
   read(5,*) (cdy(ise),cdz(ise),ise=1,nse)
   write(6,*) 'Section  breadth     draft     CD horizontal   CD vertical'
   write(6,'(i7,2f10.3, 2f15.3)') (ise,bcross(ise),tcross(ise),cdy(ise),cdz(ise), ise=1,nse)
endif

OffsetSections: do ise=nse,1,-1                                  !integral terms of restoring matrix
   restorematr(:,:,ise)=0.
   dx=(x(ise+1)-x(ise-1))/2.                                                      !central differences
   yright=merge(yof(ise,1),-yof(ise,nof(ise)),.not.sym)
   yleft=yof(ise,nof(ise))
   b1=(yright-yleft)*dx
   b2=(yright**2-yleft**2)/2.*dx
   b3=(yright**3-yleft**3)/3.*dx
   az=area(ise)*zs(ise)
   zwmatr63=fillrealmatr(6,3,(/                        &
         0.,       0.,    -area(ise)*dx,               &
        0.,   area(ise)*dx,     0.,                    &
        b1,       b2,      -b1*x(ise),                 &
        b2,   -az*dx+b3,   -b2*x(ise),                 &
   -b1*x(ise), -b2*x(ise),  -az*dx+b1*x(ise)**2,       &
        0.,  x(ise)*area(ise)*dx, ys(ise)*area(ise)*dx  /))*rho*g
   restorematr(1:6,3:5,ise)=restorematr(1:6,3:5,ise+1)+zwmatr63
enddo OffsetSections

OffsetS: do ise=1,nse                                       !contributions not involving x integrals
            do i=ise,ise-1,-1
               zwmatr63=fillrealmatr(6,3,(/                     &
                area(i),      ys(i)*area(i),    -x(i)*area(i),  &
                   0.,                0.,              0.,      &
                   0.,                0.,              0.,      &
                   0.,                0.,              0.,      &
                zs(i)*area(i),  ys(i)*zs(i)*area(i),   0.,      &
               -ys(i)*area(i), -ys(i)**2*area(i),      0.  /))*rho*g/2.
               restorematr(1:6,3:5,ise)=restorematr(1:6,3:5,ise)+zwmatr63
            enddo
enddo OffsetS

BodySections: do is=1,ns                                               !contributions of body weight
   zwmatr63=fillrealmatr(6,3,(/                &
      0.,       0.,              mass(is),     &
      0.,   -mass(is),            0.,          &
      0.,       0.,               0.,          &
      0.,  zg(is)*mass(is),       0.,          &
      0.,       0.,           mass(is)*zg(is), &
      0., -xg(is)*mass(is),  -mass(is)*yg(is)  /))*g
   restorematr(:,3:5,is)=restorematr(:,3:5,is)+zwmatr63
enddo BodySections
write(6,'(a,f8.3)') 'Transverse metacentric height:',real(restorematr(4,4,1))/g/mass(1)
zwmatr63=fillrealmatr(6,3,(/                 &
  area(1),   ys(1)*area(1),   -x(1)*area(1), &
    0.,            0.,                  0.,  &
    0.,            0.,                  0.,  &
    0.,            0.,                  0.,  &
  zs(1)*area(1), ys(1)*zs(1)*area(1),   0.,  &
 -ys(1)*area(1),  -ys(1)**2*area(1),    0.   /))*rho*g   !next: add two 0 columns left and one right
restorekorrmatr=0.
restorekorrmatr(:,3:5)=zwmatr63

IntersectionPreparation: do is=2,ns
   ise=merge(ise+nse,is-ns+nse, ise<=0)
   vsmatr(:,:,is)=0.
   do i=1,6; vsmatr(i,i,is)=1.; enddo
   vsmatr(5,3,is)=(x(ise)+x(ise-1))/2.
   vsmatr(6,2,is)=-vsmatr(5,3,is)
enddo IntersectionPreparation

read(5,*) nfin             !read fin data: no.,center,axis,control,length,chord,cm,dclda,hullf,anglef
if (nfin>0) then
   read(5,*)((xfin(k,ifin),k=1,3),(afin(k,ifin),k=1,3),(calfin(k,ifin),k=1,6), &
      lfin(ifin),cfin(ifin),cmfin(ifin),clgrfin(ifin),rfin(ifin),sfin(ifin),cdfin(ifin),ifin=1,nfin)
   write(6,'(/'' Data of fin number      '',i2,9i11)') (ifin,ifin=1,nfin)
   write(6,'(1x,131(1h-))')
   write(6,'('' Force center: x    '',10g11.3)') (xfin(1,i),i=1,nfin)
   write(6,'(''               y    '',10g11.3)') (xfin(2,i),i=1,nfin)
   write(6,'(''               z    '',10g11.3)') (xfin(3,i),i=1,nfin)
   write(6,'('' Axis direct.  x    '',10g11.3)') (afin(1,i),i=1,nfin)
   write(6,'(''               y    '',10g11.3)') (afin(2,i),i=1,nfin)
   write(6,'(''               z    '',10g11.3)') (afin(3,i),i=1,nfin)
   write(6,'('' Fin control angle amplitude / motion amplitude:'')')
   write(6,'(''  surge, real:      '',10g11.3)') (real(calfin(1,ifin)),ifin=1,nfin)
   write(6,'(''  surge, imaginary: '',10g11.3)') (aimag(calfin(1,ifin)),ifin=1,nfin)
   write(6,'(''  sway, real:       '',10g11.3)') (real(calfin(2,ifin)),ifin=1,nfin)
   write(6,'(''  sway, imaginary:  '',10g11.3)') (aimag(calfin(2,ifin)),ifin=1,nfin)
   write(6,'(''  heave, real:      '',10g11.3)') (real(calfin(3,ifin)),ifin=1,nfin)
   write(6,'(''  heave, imaginary: '',10g11.3)') (aimag(calfin(3,ifin)),ifin=1,nfin)
   write(6,'(''  roll, real:       '',10g11.3)') (real(calfin(4,ifin)),ifin=1,nfin)
   write(6,'(''  roll, imaginary:  '',10g11.3)') (aimag(calfin(4,ifin)),ifin=1,nfin)
   write(6,'(''  pitch, real:      '',10g11.3)') (real(calfin(5,ifin)),ifin=1,nfin)
   write(6,'(''  pitch, imaginary: '',10g11.3)') (aimag(calfin(5,ifin)),ifin=1,nfin)
   write(6,'(''  yaw, real:        '',10g11.3)') (real(calfin(6,ifin)),ifin=1,nfin)
   write(6,'(''  yaw, imaginary:   '',10g11.3)') (aimag(calfin(6,ifin)),ifin=1,nfin)
   write(6,'('' Fin length         '',10g11.3)') (lfin(i),i=1,nfin)
   write(6,'('' Chord              '',10g11.3)') (cfin(i),i=1,nfin)
   write(6,'('' Added mass coeff.  '',10g11.3)') (cmfin(i),i=1,nfin)
   write(6,'('' dcl/dalpha         '',10g11.3)') (clgrfin(i),i=1,nfin)
   write(6,'('' Hull factor r      '',10g11.3)') (rfin(i),i=1,nfin)
   write(6,'('' Angle factor s     '',10g11.3)') (sfin(i),i=1,nfin)
   write(6,'('' Transv. drag coeff.'',10g11.3)') (cdfin(i),i=1,nfin)
else
   write(6,*) 'No fins'
endif
write(6,'(1x,131(1h-)/)')
Fins: do ifin=1,nfin                                                               !prepare fin data
   zw=sqrt(sum(afin(1:3,ifin)**2))
   afin(1:3,ifin)=afin(1:3,ifin)/zw
   addedmassfin(ifin)=cmfin(ifin)*pi/4.*rho*cfin(ifin)**2*lfin(ifin)
   zw=sqrt(afin(2,ifin)**2+afin(3,ifin)**2)
   rnf2=-afin(3,ifin)/zw
   rnf3= afin(2,ifin)/zw
   wfinmatr(:,:,ifin)=fillrealmatr(1,6,(/ 0., rnf2, rnf3, rnf2*xfin(3,ifin)-rnf3*xfin(2,ifin), &
      -rnf3*(xfin(1,ifin)-0.5*cfin(ifin)), rnf2*(xfin(1,ifin)-0.5*cfin(ifin))/))
   bfin(ifin)=0.5*rho*lfin(ifin)*cfin(ifin)*clgrfin(ifin)
   cfinmatr(:,:,ifin)=fillrealmatr(1,6,(/0.,0.,0.,0.,-rnf3,rnf2/))
   skorfin(ifin)=sfin(ifin)*zw
enddo Fins

read(5,*)nsail                                                                            !sail data
if (nsail>0) then
   read(5,*)uw,muw
   read(5,*) (xsail(1:3,isail),csail(1:3,isail),msail(1:3,isail),dcndasail(isail),cmsail(isail),isail=1,nsail)
   write(6,'(a,f10.3)')  ' True wind speed            ',uw
   write(6,'(a,f10.3)')  ' Wind direction rel. to wave',muw
   write(6,'(a,10i10  )') ' Data of sail number        ',(i,i=1,nsail)
   write(6,'(1x,131(1h-))')
   write(6,'(a,10f10.3)') ' x of sail force point      ',xsail(1,1:nsail)
   write(6,'(a,10f10.3)') ' y of sail force point      ',xsail(2,1:nsail)
   write(6,'(a,10f10.3)') ' z of sail force point      ',xsail(3,1:nsail)
   write(6,'(a,10f10.3)') ' x component of chord vector',csail(1,1:nsail)
   write(6,'(a,10f10.3)') ' y component of chord vector',csail(2,1:nsail)
   write(6,'(a,10f10.3)') ' z component of chord vector',csail(3,1:nsail)
   write(6,'(a,10f10.3)') ' x component of mast vector ',msail(1,1:nsail)
   write(6,'(a,10f10.3)') ' y component of mast vector ',msail(2,1:nsail)
   write(6,'(a,10f10.3)') ' z component of mast vector ',msail(3,1:nsail)
   write(6,'(a,10f10.3)') ' dc_n/dalpha                ',dcndasail(1:nsail)
   write(6,'(a,10f10.3)') ' Mass coefficient c_m       ',cmsail(1:nsail)
   write(6,'(1x,131(1h-))')
   muw=muw*pi/180           !Preparations for sails. First change from input system to interior system
   Sails: do isail=1,nsail
      xsail(:,isail)=(/xsail(1,isail),-xsail(2,isail),-xsail(3,isail)/)
      csail(:,isail)=(/csail(1,isail),-csail(2,isail),-csail(3,isail)/)
      msail(:,isail)=(/msail(1,isail),-msail(2,isail),-msail(3,isail)/)
      xsail1(1:3,isail)=xsail(1:3,isail)+0.5*csail(1:3,isail)                     !speed reference point
      wsailmatr(:,:,isail)=fillrealmatr(3,6,(/                            &
       1., 0. ,0.,    0.,            xsail1(3,isail),  -xsail1(2,isail),  &
       0., 1. ,0., -xsail1(3,isail),      0.,           xsail1(1,isail),  &
       0., 0. ,1.,  xsail1(2,isail),-xsail1(1,isail),          0.         /))
      ssail(1:3,isail)=csail(:,isail).vprod.msail(:,isail)
      ssailabs(isail)=sqrt(sum(ssail(1:3,isail)**2))
      rhovsail(1:3,1,isail)=ssail(:,isail)*rho/800./ssailabs(isail)
      rhovsail(4:6,1,isail)=xsail(:,isail).vprod.ssail(:,isail)*rho/800./ssailabs(isail)
   enddo Sails
else
   write(6,*) 'No sails'
endif

read(5,*) nforce                                                             !motion-dependent forces
if (nforce>0) then
   read(5,*)((xforce(k,i),k=1,3),(aforce(k,i),k=1,3),(calforce(k,i),k=1,7),i=1,nforce)
   if (nforce>10) stop'>> Max. 10 motion-dependent forces allowed'
   write(6,'('' Motion-depend. forces'',i5,9i11)')(i,i=1,nforce)
   write(6,'(1x,131(1h-))')
   write(6,'('' Force center     x '',10g11.3)') (xforce(1,i),i=1,nforce)
   write(6,'(''                  y '',10g11.3)') (xforce(2,i),i=1,nforce)
   write(6,'(''                  z '',10g11.3)') (xforce(3,i),i=1,nforce)
   write(6,'('' Force direction  x '',10g11.3)') (aforce(1,i),i=1,nforce)
   write(6,'(''                  y '',10g11.3)') (aforce(2,i),i=1,nforce)
   write(6,'(''                  z '',10g11.3)') (aforce(3,i),i=1,nforce)
   write(6,'('' Force amplitude devided by amplitude of:'')')
   write(6,'('' Surge, real:       '',10g11.3)') (real(calforce(1,i)),i=1,nforce)
   write(6,'('' Surge, imaginary:  '',10g11.3)') (aimag(calforce(1,i)),i=1,nforce)
   write(6,'('' Sway, real:        '',10g11.3)') (real(calforce(2,i)),i=1,nforce)
   write(6,'('' Sway, imaginary:   '',10g11.3)') (aimag(calforce(2,i)),i=1,nforce)
   write(6,'('' Heave, real:       '',10g11.3)') (real(calforce(3,i)),i=1,nforce)
   write(6,'('' Heave, imaginary:  '',10g11.3)') (aimag(calforce(3,i)),i=1,nforce)
   write(6,'('' Roll, real:        '',10g11.3)') (real(calforce(4,i)),i=1,nforce)
   write(6,'('' Roll, imaginary:   '',10g11.3)') (aimag(calforce(4,i)),i=1,nforce)
   write(6,'('' Pitch, real:       '',10g11.3)') (real(calforce(5,i)),i=1,nforce)
   write(6,'('' Pitch, imaginary:  '',10g11.3)') (aimag(calforce(5,i)),i=1,nforce)
   write(6,'('' Yaw, real:         '',10g11.3)') (real(calforce(6,i)),i=1,nforce)
   write(6,'('' Yaw, imaginary:    '',10g11.3)') (aimag(calforce(6,i)),i=1,nforce)
   write(6,'('' Weight motion, real'',10g11.3)') (real(calforce(7,i)),i=1,nforce)
   write(6,'('' Weight motion, imag'',10g11.3)') (aimag(calforce(7,i)),i=1,nforce)
else
   write(6,*)'No forces depending on ship motions'
endif
write(6,'(1x,131(1h-)/)')
Forces: do i=1,nforce                                                     !preparation of force data
   aforce(1:3,i)=aforce(1:3,i)/sqrt(sum(aforce(1:3,i)**2))                           !normalize aforce
   vforcematr(:,:,i)=fillrealmatr(6,1,(/              &                                      !V matrix
    aforce(1,i), -aforce(2,i), -aforce(3,i),          &          !change input to internal coordinates
    xforce(2,i)*aforce(3,i)-xforce(3,i)*aforce(2,i),  &
   -xforce(3,i)*aforce(1,i)+xforce(1,i)*aforce(3,i),  &
   -xforce(1,i)*aforce(2,i)+xforce(2,i)*aforce(1,i)  /))
   alforcematr=fillcomplmatr(1,6,calforce(:,i))
   bforcematr(:,:,i)=vforcematr(:,:,i).mprod.alforcematr
enddo Forces

read(5,*) rml,xl,yl,zl,cablelength                                   !read data of a suspended weight
if (rml.ne.0.) then
   write(6,*)'Data of suspended weight:'
   write(6,'('' Mass                     '',1g10.3)') rml
   write(6,'('' x,y,z of suspension point'',3g10.3)')xl,yl,zl
   write(6,'('' Length of cable          '',1g10.3)')cablelength
else
   write(6,*)'No suspended weight'
   cablelength=1.
endif
write(6,'(1x,131(1h-))')

read(5,*) nb                                                          !points for motion calculations
if (nb>0) then
   read(5,*)(xb(ib),yb(ib),zb(ib),ib=1,nb)
   write(6,'(/'' Points where motions and accelerations shall be determined'')')
   write(6,'('' no.'',14i9)')(ib,ib=1,nb)
   write(6,'(1x,131(1h-))')
   write(6,'(''   x'',14f9.3)')xb(1:nb)
   write(6,'(''   y'',14f9.3)')yb(1:nb)
   write(6,'(''   z'',14f9.3)')zb(1:nb)
   MotionPoints: do ib=1,nb                                          !preparation of motion point data
      wbmatr(:,:,ib)=fillrealmatr(3,6,(/         &
        1., 0., 0.,    0.,   -zb(ib), yb(ib),    &
        0., 1., 0.,  zb(ib),   0.,    xb(ib),    &
        0., 0., 1., -yb(ib), -xb(ib),    0.      /))
   enddo MotionPoints
endif

write(6,'(/a)')' In the following 1,2,3 designate directions forward, to starboard, downward'
Sections: do ise=1,nse                               !read and interpolate hydrodynamic section data
   if (npres>0) read(24,*)(yint(i,ise),zint(i,ise),i=1,npres)               !read idr, pressure points
   !omsec circular frequencies for which section data were determined
   !om  circular frequency for which transfer functions will be determined
   Frequencies: do ifre=1,nfre                            !read section hydrodynamic data from file 20
      read(20,*) omsec(ifre),nmu,(mu(imu),imu=1,nmu)
      read(20,*) (cfsece(i,1:3),i=1,3)                                                  !radiation forces
      read(20,*) (cfsece(i,4:nmu+3),i=1,3)                                            !diffraction forces
      read(20,*) (cfksece(i,1:nmu),i=1,3)                                           !Froude-Krilow forces
      if (npres>0) then                                             !radiation and diffraction pressures
         read(20,*)(iskip,(cpr(i+npres*j),j=0,2),(cpd(i+npres*j),j=0,nmu-1),i=1,npres)
      endif
      FirstFrequency: if (ifre.eq.1) then
         fsec(1,1,ise,0)=999.                         !special matrices as initial values because of mloga
         prsec(1,1,ise,0)=999.
         pdsecv(1,1,1:nmu)=999.
         fsecev(1,1,1:nmu)=999.
         fksecev(1,1,1:nmu)=999
      endif FirstFrequency
      cik=ci*waven          !change to internal global coordinates; logarithm of data for interpolation
      fsec(:,:,ise,ifre)=transpose(fillcomplmatr(3,3,cfsece)/omsec(ifre)**2)
      fsec(:,:,ise,ifre)=logmatr(fsec(:,:,ise,ifre),fsec(:,:,ise,ifre-1))                     !radiation
      prsec(:,:,ise,ifre)=transpose(fillcomplmatr(3,npres1,cpr)/omsec(ifre)**2)
      prsec(:,:,ise,ifre)=logmatr(prsec(:,:,ise,ifre),prsec(:,:,ise,ifre-1))
      Angles: do imu=1,nmu
         pdsecv(:,:,imu)=logmatr(transpose(fillcomplmatr(1,npres1,cpd(npres1*(imu-1)+1))),pdsecv(:,:,imu))
         pdsec(:,:,imu,ise,ifre)=pdsecv(:,:,imu)
         fsecev(:,:,imu)=logmatr(transpose(fillcomplmatr(1,3,cfsece(1,imu+3))),fsecev(:,:,imu))
         fsece(:,:,imu,ise,ifre)=fsecev(:,:,imu)
         fksecev(:,:,imu)=logmatr(transpose(fillcomplmatr(1,3,cfksece(1,imu))),fksecev(:,:,imu))
         fksece(:,:,imu,ise,ifre)=fksecev(:,:,imu)
      enddo Angles
   enddo Frequencies
enddo Sections
read(20,*) testnumber
! GT/2006-05-03: original code replaced by code to facilitate Salford FTN95 compiler
allocate(yofTemp(1:nse)); allocate(zofTemp(1:nse))                                                              ! GT/2006-05-03
do ise = 1, nse                                                                                                 ! GT/2006-05-03
   yofTemp(ise) = sum((/(yof(ise,1:nof(ise)))/)); zofTemp(ise) = sum((/(zof(ise,1:nof(ise)))/))                 ! GT/2006-05-03
end do                                                                                                          ! GT/2006-05-03
if (abs(npres+sum(wangl(1:nmu))+sum(x(1:nse))+sum(yofTemp)+sum(zofTemp)+g+rho+zwl+1/zbot-testnumber)>3e-2) then ! Soe110406
   stop'>> File sectionresults does not fit to input data.'                                                     ! Soe110406
end if
deallocate(yofTemp); deallocate(zofTemp)                                                                        ! GT/2006-05-03
imcorr(1:nmu)=(/(imu,imu=1,nmu)/)            !add encounter angles >90 degrees; maximum <270 degrees
i1=merge(1,0,abs(mu(nmu)-pi/2).lt.0.001)
i2=merge(1,0,abs(mu(1  )+pi/2).lt.0.001)
WaveAngles: do imu=nmu-i1,1+i2,-1
   nmu=nmu+1
   imcorr(nmu)=imu                                       !corresponding quartering mu for section data
   mu(nmu)=pi-mu(imu)
enddo WaveAngles
read(5,*) nom,(wavelength(iom),iom=1,nom)                                          !read wave lengths
read(5,*) nv,(speed(iv),ltrwet(iv),iv=1,nv)                         !read velocities, transom wetted?
if (.not.ltrans) call signampl      !skips transfer functions, continues with significant amplitudes
write(6,'(/'' Motions and intersection forces in regular waves'')')
write(21,'(a)')text                                                  !output initial data on file 21
write(21,*) nb,ls,g,rho,rml
write(21,*) nom,(wavelength(iom),iom=1,nom),nv,(speed(iv),iv=1,nv),nmu,(mu(imu),imu=1,nmu), nse,(x(ise),ise=1,nse)
Wavelengths: do iom=1,nom                                                       !for all wavelengths
   waven=2.*pi/wavelength(iom)
   cik=ci*waven
   om=sqrt(waven*g*tanh(waven*(zbot-zwl)))
   waveheight=min(maxheight,steepness*wavelength(iom))
   Speeds: do iv=1,nv                                                                  !for all speeds
      vs=speed(iv)
      WaveDir: do imu=1,nmu                                               !for all wave encounter angles
         cosm=cos(mu(imu))
         sinm=sin(mu(imu))
         ome=om-waven*vs*cosm
         ciome=ci*ome
         if (abs(ome)>omsec(nfre).or.om>omsec(nfre)) then
            write(6,*)' *** Frequency range of section data insufficient'
            write(6,*)'     for wave length',wavelength(iom),' speed',speed(iv), '     wave encounter angle',mu(imu)*180./pi
            exit WaveDir
         endif
         omlow=2*vs/(x(nse)-x(1))                                !factors for interpolation over frequency
         iomb=which(nfre-1,omsec(2:nfre)>=om)
         fint1b=(omsec(iomb+1)**2-om**2)/(omsec(iomb+1)**2-omsec(iomb)**2)
         fint2b=1-fint1b                                        !fint1b,fint2b,iomb for mfki interpolation
         ioma=which(nfre-1,omsec(2:nfre)>=om.and.omsec(1:nfre-1)>=omlow)
         if (ioma==0) ioma=nfre                                                                !necessary?
         iomesec=ioma+1
         if (om.ge.omsec(ioma)) then
            fint1a=(omsec(iomesec)**2-om**2)/(omsec(iomesec)**2-omsec(ioma)**2)
            fint2a=1-fint1a                                       !fint1a,fint2a,ioma for mfdi interpolation
            fakta=1.
         else
            fint1a=1.
            fint2a=0
            fakta=(om/omsec(ioma))**2
         endif
         iomc=which(nfre-1,omsec(2:nfre)>=abs(ome).and.omsec(1:nfre-1)>=omlow)
         if (iomc==0) iomc=nfre                                                                !necessary?
         iomesec=iomc+1
         if (abs(ome).ge.omsec(iomc)) then
            fint1c=(omsec(iomesec)**2-ome**2)/(omsec(iomesec)**2-omsec(iomc)**2)
            fint2c=1-fint1c
            faktc=1.                                     ! fint1c,fint2c,iomc for ma,mfd90,mar interpolation
         else
            fint1c=1
            fint2c=0
            faktc=(ome/omsec(iomc))**2
         endif
         im=imcorr(imu)                              !im = corresponding wave angle index for section data
         ex(:,:,1)=0.
         awprev=0.
         geprev=0.
         prwprev=0.
         ser=0                                               !error quantity required to end the iteration
         motion1=0.
         addedmass(:,:,1)=0.

         Iteration: do itz=1,200                                                !for <=200 iteration steps
            addedmasschge=0.
            exchge=0.
            fprou=0.
            fprouf=0.
            fwithoutu=0.
            OffsetSects: do ise=nse,1,-1                          !for all offset sections, beginning at bow
               FirstIteration: if (itz==1) then
                  ncrossold(:,:,ise)=0.
                  ncross   (:,:,ise)=0.
                  ecrossold(:,:,ise)=0.
                  ecross   (:,:,ise)=0.
                  pwg(:,:,ise)=0.
                  prg(:,:,ise)=0.
                  czeta3(ise)=exp(-cik*x(ise)*cosm)                                     !determine zeta3 (wave)
                  wmatr(:,:,ise)=fillcomplmatr(3,6,(/                                       &
                   (0.,0.),  ciome,  (0.,0.),  (0.,0.),     (0.,0.),      ciome*x(ise)-vs,  &
                   (0.,0.), (0.,0.),  ciome,   (0.,0.), -ciome*x(ise)+vs,     (0.,0.),      &
                   (0.,0.), (0.,0.), (0.,0.),   ciome,      (0.,0.),         (0.,0.)       /))

                  ! interpolate section data over ome**2 (radiation) or om**2 (excitation)
                  ! and remove logarithm. ..i = interpolated, ..sec = determined for standard set of om
                  fdi=fakta*exp(fint1a*fsece(:,:,im,ise,ioma)+fint2a*fsece(:,:,im,ise,ioma+1))
                  pdi=fakta*exp(fint1a*pdsec(:,:,im,ise,ioma)+fint2a*pdsec(:,:,im,ise,ioma+1))
                  if (fint2a.eq.0.) then
                     fdi=cmplx(real(fdi),aimag(fdi)*fakta)
                     pdi=cmplx(real(pdi),aimag(pdi)*fakta)
                  endif
                  FroudeKrilow: do i=1,npres                                             !Froude-Krilow pressure
                     if (waven*(zbot-zwl)>3.0) then !deep water
                        cpfk(i)=-g*rho*exp(waven*cmplx(zwl-zint(i,ise),yint(i,ise)*sinm))
                        if (ise.eq.nse) cpfkbow(i)=-g*rho*exp(waven*(zwl-zint(i,ise)))                   !before bow
                     else
                        cpfk(i)=-g*rho*cosh(waven*(zint(i,ise)-zbot))/sinh(waven*(zbot-zwl))*exp(ci*waven*yint(i,ise)*sinm)
                        if (ise.eq.nse) then
                           cpfkbow(i)=-g*rho*cosh(waven*(zint(i,ise)-zbot))/sinh(waven*(zbot-zwl))
                        endif
                     endif
                  enddo FroudeKrilow
                  pw(:,1,ise)=czeta3(ise)*ci/om*(pdi(:,1)+cpfk(:))                  !wave pressure [ ] times rho
                  fki=exp(fint1b*fksece(:,:,im,ise,iomb)+fint2b*fksece(:,:,im,ise,iomb+1))
                  vmatr(:,:,ise)=fillrealmatr(6,3,(/    &
                        0.,             0.,         0., &
                        1.,             0.,         0., &
                        0.,             1.,         0., &
                        0.,             0.,         1., &
                      0.,   -(x(ise)+x(ise+1))/2.,  0., &
                  (x(ise)+x(ise+1))/2., 0.,         0.  /))
                  uvmatr=fillrealmatr(6,1,(/ 1., 0., 0., 0., (zs(ise)+zs(ise+1))/2., 0./))
                  dx=x(min(ise+1,nse))-x(min(ise,nse-1))
                  if (ise.eq.nse) then                                                  !at `section before bow'
                     czeta3(ise+1)=exp(-cik*(x(ise)+dx)*cosm)
                     pwprev(:,:)=czeta3(ise+1)*ci/om*fillcomplmatr(npres,1,cpfkbow)
                  endif
                  do i=1,npres                                            !phi_y^0, phi_z^0, y.phi_z^0-z.phi_y^0
                     if (ise.lt.nse) then
                        rdyzdx(i        )=(yint(i,ise)-yint(i,ise+1))/dx*vs
                        rdyzdx(i+  npres)=(zint(i,ise)-zint(i,ise+1))/dx*vs
                        rdyzdx(i+2*npres)=(yint(i,ise)+yint(i,ise+1))/2*rdyzdx(i+npres) -(zint(i,ise)+zint(i,ise+1))/2*rdyzdx(i)
                     else                                                                     !at foremost section
                        rdyzdx(i        )=(yint(i,ise)-0.        )/dx*vs
                        rdyzdx(i+  npres)=(zint(i,ise)-zint(1,ise))/dx*vs
                        rdyzdx(i+2*npres)=yint(i,ise)*rdyzdx(i+npres)-zint(i,ise)*rdyzdx(i)
                     endif
                  enddo
                  phi0(:,:)=transpose(fillrealmatr(3,npres,rdyzdx))                !matrix of phi^0 derivatives
                  pri=faktc*exp(fint1c*prsec(:,:,ise,iomc)+fint2c*prsec(:,:,ise,iomc+1))
                  amatr=faktc*exp(fint1c*fsec(:,:,ise,iomc)+fint2c*fsec(:,:,ise,iomc+1))
                  if (fint2c.eq.0.) then
                     amatr=cmplx(real(amatr)/faktc,aimag(amatr))
                     pri=cmplx(real(pri)/faktc,aimag(pri))                       !radiation pressure, interpolated
                  endif
                  if (ome.lt.0.) then
                     amatr=conjg(amatr)
                     pri=conjg(pri)
                  endif
                  awmatr=amatr.mprod.wmatr(:,:,ise)
                  dawmatr=vs*(awprev-awmatr)
                  awprev=awmatr
                  prw(:,:,ise)=pri.mprod.wmatr(:,:,ise)
                  dprw=vs/2/dx*(prwprev-prw(:,:,ise))
                  prwprev=prw(:,:,ise)
                  ez23=czeta3(ise)*fdi
                  dge=geprev-ez23
                  geprev=ez23
                  dpw=vs/2/dx*(pwprev-pw(:,:,ise))
                  pwprev=pw(:,:,ise)
                  phi0w=0.25*phi0.mprod.(wmatr(:,:,ise)+wmatr(:,:,min(ise+1,nse)))
                  prg(:,:,ise)=prg(:,:,ise)+phi0w
                  prg(:,:,min(ise+1,nse))=prg(:,:,min(ise+1,nse))+phi0w         !at foremost section add 2 times
                  if (iab(ise).eq.0) then
                     zwmatr66=vmatr(:,:,ise).mprod.dawmatr
                     addedmass(:,:,1)=addedmass(:,:,1)+zwmatr66
                     if (ls.and.ise.ne.nse) addedmass(:,:,ise+1)=addedmass(:,:,ise+1)+0.5*zwmatr66
                     zw1matr61=vs*ci/om*vmatr(:,:,ise).mprod.dge
                     prg(:,:,ise)=prg(:,:,ise)+dprw
                     prg(:,:,min(ise+1,nse))=prg(:,:,min(ise+1,nse))+dprw
                     ex(:,:,1)=ex(:,:,1)+zw1matr61
                     pwg(:,:,ise)=pwg(:,:,ise)+dpw
                     pwg(:,:,min(ise+1,nse))= pwg(:,:,min(ise+1,nse))+dpw
                     if (ls.and.ise.ne.nse) then
                        ex(:,:,ise+1)=ex(:,:,ise+1)+0.5*zw1matr61
                     endif
                  endif
                  ! Froude-Krilow longitudinal force
                  if (waven*(zbot-zwl)>3.0) then                                                     !deep water
                     calda=-rho*g*exp(cmplx(-waven*((zs(ise+1)+zs(ise))/2.-zwl),0.))*(area(ise+1)-area(ise))
                  else                                                                            !shallow water
                     calda=-rho*g*cosh(waven*((zs(ise+1)+zs(ise))/2.-zbot))/sinh(waven*(zbot-zwl))*(area(ise+1)-area(ise))
                  endif
                  ex(:,:,1)=ex(:,:,1)+calda*(czeta3(ise)+czeta3(min(nse,ise+1)))/2.*uvmatr
                  vmatr(5,2,ise)=-x(ise)
                  vmatr(6,1,ise)= x(ise)
                  uvmatr(5,1)=zs(ise)
                  awmatr=-ciome*awmatr
                  prg(:,:,ise)=prg(:,:,ise)-ciome*prw(:,:,ise)
                  addedmass(:,:,1)=addedmass(:,:,1)+(x(ise+1)-x(ise-1))/2.*(vmatr(:,:,ise).mprod.awmatr)
                  !BNV/2006-05-08: original code replaced by code to facilitate Salford FTN95 compiler
                  BNVtemp1=vmatr(:,:,ise).mprod.(fki+ome/om*fdi)                    !BNV/2006-05-08
                  ex(:,:,1)=ex(:,:,1)+(x(ise+1)-x(ise-1))/2.*czeta3(ise)*BNVtemp1   !BNV/2006-05-08
                  pwg(:,:,ise)=pwg(:,:,ise)-ciome*pw(:,:,ise)
                  pst(:,:,ise)=0.
                  pst(1:npres,3,ise)=1.
                  pst(1:npres,4,ise)=yint(1:npres,ise)
                  pst(1:npres,5,ise)=-x(ise)
                  pst(:,:,ise)=rho*g*pst(:,:,ise)
               else FirstIteration           !itz>1: correction for quadratical forces (radiation, excitation)
                  uu=fillrealmatr(2,3,(/ &
                             1., 0., 0., &
                             0., 1., 0.  /))
                  udyz=uu.mprod.wmatr(:,:,ise).mprod.motion1
                  czw=om*waveheight/2.*exp(-cik*x(ise)*cosm)
                  if (waven*(zbot-zwl)>3.0) then                       !deep water
                     wdy=czw*sinm*exp(-waven*tcross(ise))
                     wdz=czw*ci*exp(-waven*tcross(ise))
                  else                                                                            !shallow water
                     wdy=czw*sinm*cosh(waven*(tcross(ise)+zwl-zbot))/sinh(waven*(zbot-zwl))
                     wdz=-czw*ci *sinh(waven*(tcross(ise)+zwl-zbot))/sinh(waven*(zbot-zwl))
                  endif
                  vcross=8/3./pi*sqrt((wdy-udyz(1,1))*conjg(wdy-udyz(1,1))+ (wdz-udyz(2,1))*conjg(wdz-udyz(2,1)))
                  ncrossold(:,:,ise)=ncross(:,:,ise)
                  zwmatr22=fillrealmatr(2,2,(/ &
                     tcross(ise)*cdy(ise)*vcross*0.5*rho,   0.,             &
                     0.,           bcross(ise)*cdz(ise)*vcross*0.5*rho  /))
                  ncross(:,:,ise)=transpose(uu).mprod.zwmatr22.mprod.uu
                  addedmasschge=addedmasschge-(x(ise+1)-x(ise-1))/2* &
                     (vmatr(:,:,ise).mprod.(ncross(:,:,ise)-ncrossold(:,:,ise)).mprod.wmatr(:,:,ise))
                  ecrossold(:,:,ise)=ecross(:,:,ise)
                  ecross(:,:,ise)=fillcomplmatr(3,1,        (/ &
                   0.5*rho*vcross*wdy*tcross(ise)*cdy(ise),    &
                   0.5*rho*vcross*wdz*bcross(ise)*cdz(ise),    &
                                    (0.,0.)                  /))
                  !BNV/2006-05-08: original code replaced by code to facilitate Salford FTN95 compiler
                  BNVtemp2=ecross(:,:,ise)-ecrossold(:,:,ise)         !BNV/2006-05-08
                  BNVtemp1=vmatr(:,:,ise).mprod.BNVtemp2              !BNV/2006-05-08
                  exchge=exchge+(x(ise+1)-x(ise-1))/2.*BNVtemp1       !BNV/2006-05-08
               endif FirstIteration
               if (itz.eq.1) then
                  if (ls.and.ise.ne.1) then
                     addedmass(:,:,ise)=addedmass(:,:,1)
                     ex(:,:,ise)=ex(:,:,1)
                  endif
               elseif (ls.or.ise.eq.1) then
                  addedmass(:,:,ise)=addedmass(:,:,ise)+addedmasschge
                  ex(:,:,ise)=ex(:,:,ise)+exchge
               endif
            enddo OffsetSects

            FirstIt: if (itz==1) then             !longitudinal force due to transom
               if (ltrwet(iv)) then
                  if (waven*(zbot-zwl)>3.0) then
                     calda=-rho*g*exp(cmplx(-waven*(zs(1)-zwl),0.))*area(1)               !deep water
                  else
                     calda=-rho*g*cosh(waven*(zs(1)-zbot))/sinh(waven*(zbot-zwl))*area(1) !shallow water
                  endif
                  ex(:,:,1)=ex(:,:,1)+calda*czeta3(1)*uvmatr
               endif
              !approximation of longitudinal added mass. uvmatr for last section
              longaddmass= real(massmatr(1,1,1))/(3.14*sqrt((x(nse)-x(1))**3*rho/real(massmatr(1,1,1)))-14.)*ome**2
              addedmass(:,:,1)=addedmass(:,:,1)+longaddmass*(uvmatr.mprod.transpose(uvmatr))
           endif FirstIt

           ForFins: do ifin=1,nfin               !influence of fins; including quadratical force components
              if (itz.eq.1) then
                 bfinold(:,:,ifin)=0.
                 efinold(:,:,ifin)=0.
              endif
              if (waven*(zbot-zwl)>3.0) then    !deep water. xfin input system, zwl interior system
                 cvfw=om*exp(cmplx(-waven*(-xfin(3,ifin)-zwl),waven*(-(xfin(1,ifin)-0.5*cfin(ifin))*cosm-  &
                    xfin(2,ifin)*sinm)))*(wfinmatr(1,2,ifin)*sinm+wfinmatr(1,3,ifin)*ci)
              else
                 cvfw=om/sinh(waven*(zbot-zwl)) &                                        !xfin in input system!
                      *exp(ci*waven*(-(xfin(1,ifin)-0.5*cfin(ifin))*cosm-xfin(2,ifin)*sinm))  &
                      *(wfinmatr(1,2,ifin)*sinm*cosh(waven*(-xfin(3,ifin)-zbot)) &
                      -wfinmatr(1,3,ifin)*ci*sinh(waven*(-xfin(3,ifin)-zbot)))
              endif
              vfin=bfin(ifin)*vs
              if (itz.gt.1) then
                 cvfs=ciome*sum(wfinmatr(1,:,ifin)*motion1(:,1))
                 vfin=vfin+rho*lfin(ifin)*cfin(ifin)*0.5*8/(3*pi)*rfin(ifin)*cdfin(ifin)*abs(cvfw*waveheight/2-cvfs)
              endif
              alfin=fillcomplmatr(1,6,calfin(:,ifin))
              if (ome<0) alfin=conjg(alfin)
              bfinnew=transpose(wfinmatr(:,:,ifin)).mprod.( &
                 (ome**2*rfin(ifin)*addedmassfin(ifin)-ciome*vfin*rfin(ifin))*wfinmatr(:,:,ifin) &
                 +vfin*vs*rfin(ifin)*cfinmatr(:,:,ifin)+vfin*vs*skorfin(ifin)*alfin)
              addedmfin=bfinnew-bfinold(:,:,ifin)
              bfinold(:,:,ifin)=bfinnew
              addedmass(:,:,1)=addedmass(:,:,1)+addedmfin
              fprouf=fprouf+addedmfin
              efinnew=cvfw*rfin(ifin)*(ci*om*addedmassfin(ifin)+vfin)*transpose(wfinmatr(:,:,ifin))
              exfin=efinnew-efinold(:,:,ifin)
              efinold(:,:,ifin)=efinnew(:,:)
              ex(:,:,1)=ex(:,:,1)+exfin
              fwithoutu=fwithoutu+exfin
              if (ls) then
                 do ise=nse,2,-1
                    if (x(ise).lt.xfin(1,ifin)) then
                       addedmass(:,:,ise)=addedmass(:,:,ise)+addedmfin
                       ex(:,:,ise)=ex(:,:,ise)+exfin
                     endif
                  enddo
               endif
            enddo ForFins

            IfSails: if (nsail>0.and.itz==1) then     !change of addedmass due to sails only once
               uwind=uw*(/cos(mu(imu)+muw),-sin(mu(imu)+muw),0./)
               uwind(1)=uwind(1)-vs                                                     !uwind = apparent wind
               if (abs(uwind(1))<0.001) uwind(1)=1e-3
               uwindabs=sqrt(sum(uwind**2))
               AllSails: do isail=1,nsail
                  baralpha=asin(sum(uwind*ssail(1:3,isail))/uwindabs/ssailabs(isail))
                  cn=max(-1.5,min(dcndasail(isail)*baralpha,1.5))
                  sailfactor(1:3)=-uwind(1:3)*cn*ssailabs(isail) &
                     -pi/4.*cmsail(isail)*ciome*sqrt(sum(csail(1:3,isail)**2))*ssail(1:3,isail)
                  if (abs(cn)<1.5) sailfactor=sailfactor-0.5*uwindabs*ssail(1:3,isail)*dcndasail(isail)
                  sailfactor(1:3)=sailfactor(1:3)*ciome
                  addedmass(:,:,1)=addedmass(:,:,1)+(rhovsail(:,:,isail).mprod. &
                     reshape(sailfactor,(/1,3/)).mprod.wsailmatr(:,:,isail))
               enddo AllSails
            endif IfSails

            FirstItn: if (itz.eq.1) then              !correction of restoring matrix for non-wetted transom
               if (.not.ltrwet(iv)) then
                  szw=restorematr(:,:,1)+restorekorrmatr
               else
                  szw=restorematr(:,:,1)
               endif
            endif FirstItn

            if (itz.eq.1.and.nforce>0) addedmass(:,:,1)=addedmass(:,:,1)+sum(bforcematr(:,:,1:nforce),3)
                          !correction for motion-dependent force; no influence on intersection forces
            lineqmatr(:,1:6)=-ome**2*massmatr(:,:,1)+szw-addedmass(:,:,1)  !prepare coeff. matr. for motions
                                            ! enlarge system for suspended weight. input coordinates!
            loadcol= fillrealmatr(6,1,(/0.,-ome**2*rml,0.,-(g-ome**2*(-zl+cablelength))*rml,0.,-ome**2*xl*rml/))
            do i=1,nforce                           !for external force depending on suspended weight motion
               loadcol=loadcol-calforce(7,i)*vforcematr(:,:,i)
            enddo
            loadrow=fillrealmatr(1,7,(/0.,1.,0.,zl-cablelength+g/ome**2,0.,xl,1.-g/cablelength/ome**2/))
            lineq7(1:6,1:6)=lineqmatr(:,1:6); lineq7(1:6,7:7)=loadcol; lineq7(1:6,8)=-ex(:,1,1)
            lineq7(7:7,1:7)=loadrow; lineq7(7,8)=0.
            call simqcd(lineq7,7,1,7,ks,1.e-6,cdetl)                                      !solve for motions
            if (ks.ne.0) stop'>>Singular motion equations system'
            motion=fillcomplmatr(6,1,lineq7(1:6,8))
            cym=lineq7(7,8)                                !transverse motion of suspended load rel. to ship
            cymabs=motion(6,1)*xl-motion(4,1)*(-zl+cablelength)+motion(2,1)+cym   !and in the inertial frame

            OldNewSuperposition: if (waveheight.gt.0) then
               err=sum(abs(motion(1:3,1)-motion1(1:3,1)*2/waveheight)/max(1.,abs(motion(1:3,1)))) &
                  +sum(abs(motion(4:6,1)-motion1(4:6,1)*2/waveheight)/max(waven,abs(motion(4:6,1))))
               if (err<0.005) exit Iteration
               serold=ser*(0.9-2.5/200.*itz)                            !factor to balance old and new results
               ser=serold+1./err
               motion1=serold/ser*motion1+waveheight/2/err/ser*motion
            else OldNewSuperposition
               exit Iteration
            endif OldNewSuperposition
         enddo Iteration

        write(6,'(1x,131(1h-))')                   !Iteration finished. Output of results
        if (itz.eq.200) write(6,*)'*** questionable accuracy in iteration'
        write(6,*)
        write(6,'('' wave circ. frequency'',f6.3)') om
        write(6,'('' encounter frequency '',f6.3)') ome
        write(6,'('' wave length         '',f7.2)') 2*pi/waven
        write(6,'('' wave number         '',f7.4)') waven
        write(6,'('' wave angle          '',f6.1)') mu(imu)*180/pi
        write(6,'('' speed               '',f6.2)') vs
        write(6,'(''  wetted transom?    '',l2)')   ltrwet(iv)
        write(6,'(''  log(determinant)   '',2f7.2)') cdetl
!       write(6,'(/'' Wave circ. frequency'',1f6.3,''  encounter frequ.'', &
!             &f6.3,''  wave length'',f7.2,''  wave number'',f7.4,''  wave angle'',f6.1/ &
!             &'' speed '',f6.2,''  wetted transom?        '',l2,''  log(determinant) '',2f7.2)') &
!             om,ome,6.28318/waven,waven,mu(imu)*180/pi,vs,ltrwet(iv),cdetl
        write(6,'(12x,3(''  Real part('',i1,'')  Imagin.part('',i1,'')    Abs(''i1'')''))')(i,i,i,i=1,3)
        write(6,'('' Translation'',3(1x,3f13.3))') (motion(i,1),abs(motion(i,1)),i=1,3)
        write(6,'('' Rotation/k '',3(1x,3f13.3))') (motion(i,1)/waven,abs(motion(i,1)/waven),i=4,6)
        vcompar=vs*cosm+waveheight/2*ome*abs(motion(1,1)*cosm+motion(2,1)*sinm)
        if ((ome>=0.and.vcompar>=om/waven).or.(ome<0.and.vcompar<=om/waven)) then
           write(6,*)'*** SURFRIDING. linearization inappropriate'
           motion(1,1)=(999.,0.)               !indicator for surfriding
        endif
        write(21,*)(betr2(motion(i,1)),i=1,6),betr2(cym),betr2(cymabs)
        write(23,*)(motion(i,1),i=1,6)
        if (rml.ne.0.) then
           write(6,*) 'Transverse motion of suspended weight'
           write(6,'('' rel.to ship:'',3f13.3)') cym,abs(cym)
           write(6,'('' absolute:   '',3f13.3)') cymabs,abs(cymabs)
        endif
        if (nb.gt.0) write(6,'(/'' Absolute motions in x, y, z direction'')')

        MotionPts:  do ib=1,nb
           xib(:,:,ib)=wbmatr(:,:,ib).mprod.motion
           write(6,'('' at point'',i3,3(1x,3f13.3))') ib,(xib(i,1,ib),abs(xib(i,1,ib)),i=1,3)
           write(21,*) (betr2(xib(i,1,ib)),i=1,3)
        enddo MotionPts
        if (nb.gt.0) write(6,'(/'' Relative motions'')')      !between ship and wave orbitals
        MotionP: do ib=1,nb
           if (waven*(zbot-zwl)>3.0) then         !wave particle amplitude in deep water
              zww=fillcomplmatr(3,1,(/ci*cosm,-ci*sinm,(1.,0.)/))
           else                                   !and in shallow water
              zww=fillcomplmatr(3,1,(/ci*cosm/tanh(waven*(zbot-zwl)),-ci*sinm/tanh(waven*(zbot-zwl)), (1.,0.)/))
           endif
           xiw4=exp(-cik*(xb(ib)*cosm+yb(ib)*sinm))*zww                                !yb in input system!
           relmotion=xib(:,:,ib)-xiw4
           write(6,'('' at point'',i3,3(1x,3f13.3))') ib,(relmotion(i,1),abs(relmotion(i,1)),i=1,3)
           write(21,*) (betr2(relmotion(i,1)),i=1,3)
        enddo MotionP

        IntersectionForces: if (ls) then
           write(6,*) '    at intersections midway between offset sections'
           write(6,*)'    x of intersection'
           AllIntersections: do ise=2,nse
              is=ise
              xs=(x(ise)+x(ise-1))/2.
              !BNV/2006-05-08: original code replaced by code to facilitate Salford FTN95 compiler
              BNVtemp1=((restorematr(:,:,is)-addedmass(:,:,is)-ome**2*massmatr(:,:,is)).mprod.motion)-ex(:,:,is)  !BNV/2006-05-08
              sectionforce=vsmatr(:,:,is).mprod.BNVtemp1                                                          !BNV/2006-05-08
              write(6,'('' Force '',f7.2,f12.3,2f13.3,2(1x,3f13.3))') xs,(sectionforce(i,1),abs(sectionforce(i,1)),i=1,3)
              write(6,'('' Moment  '',3x,3(1x,3f13.3))') (sectionforce(i,1),abs(sectionforce(i,1)),i=4,6)
              write(21,*) (betr2(sectionforce(i,1)),i=1,6)
           enddo AllIntersections
        endif IntersectionForces

        IfPressure: if (npres.gt.0) then                               !Pressures using corrected motions
           weightmatr=fillrealmatr(6,6,(/  &
            0.,  0.,  0.,  0.,  -1.,  0.,  &
            0.,  0.,  0.,  1.,   0.,  0.,  &
            0.,  0.,  0.,  0.,   0.,  0.,  &
            0.,  0.,  0.,-zg(1), 0.,  0.,  &
            0.,  0.,  0.,  0.,-zg(1), 0.,  &
            0.,  0.,  0., xg(1), 0.,  0.   /))
           weightmatr=g*mass(1)*weightmatr
           fwithoutu=fwithoutu+((weightmatr+fprouf).mprod.motion)
           Sect: do ise=1,nse
              prs(:,:,ise)=prg(:,:,ise)+pst(:,:,ise)
              PressurePoints: do i=1,npres
                 IfPanel: if (ise.gt.1.and.i.gt.1) then
                    lu=(/x(ise-1),yint(i-1,ise-1),zint(i-1,ise-1)/)
                    ru=(/x(ise  ),yint(i-1,ise  ),zint(i-1,ise  )/)
                    lo=(/x(ise-1),yint(i  ,ise-1),zint(i  ,ise-1)/)
                    ro=(/x(ise  ),yint(i  ,ise  ),zint(i  ,ise  )/)
                    d1=lo-ru
                    d2=ro-lu
                    fl(:,1)=-0.5*d1.vprod.d2
                    xaverage(:,1)=(lu+lo+ru+ro)/4
                    flmo(1:3,1)=fl(:,1)
                    flmo(4:6,1)=xaverage(:,1).vprod.fl(:,1)
                    cpaveragew =(pwg(i-1,1,ise-1)+pwg(i,1,ise-1)+pwg(i-1,1,ise)+pwg(i,1,ise))/4 !av. pres., wave
                    cpaveragers= (prs(i-1,:,ise-1)+prs(i,:,ise-1)+prs(i-1,:,ise)+prs(i,:,ise))/4    !and motions
                    fwithoutu=fwithoutu+cpaveragew*flmo
                    fprou=fprou+(flmo.mprod.reshape(cpaveragers,(/1,6/)))
                 endif IfPanel
              enddo PressurePoints
           enddo Sect
           lineqmatr(:,1:6)=fprou
           !BNV/2006-05-08: original code replaced by code to facilitate Salford FTN95 compiler
           BNVtemp1=massmatr(:,:,1).mprod.motion                          !BNV/2006-05-08
           lineqmatr(:,7:7)=fwithoutu+ome**2*BNVtemp1                     !BNV/2006-05-08
           lineqmatr(1,:)=(/(-1.,0.),(0.,0.),(0.,0.),(0.,0.),(0.,0.),(0.,0.),motion(1,1)/)
           call simqcd(lineqmatr,6,1,6,ks,1.e-6,cdetl)   !solve for modified motions
           if (ks.ne.0) stop'>> Singular system of equations for modified motion'
           motionmod=lineqmatr(:,7:7)
           Sectns: do ise=1,nse
              write(6,'(a,a)') ' Sect. point  coord(2)  coord(3)     Re(p)     Im(p)       |p|',&
                 '     ___wave component of p___      potential'
              pres(:,:,ise)=pwg(:,:,ise)+(prs(:,:,ise).mprod.motionmod)
              pot=((prw(:,:,ise).mprod.motion)+pw(:,:,ise))/rho
              write(24,*) iom,iv,imu,ise
              PressurePts: do i=1,npres
                 write(6,'(2i6,13f10.3)') ise,i,yint(i,ise),zint(i,ise), &
                    pres(i,1,ise),abs(pres(i,1,ise)),pwg(i,1,ise),abs(pwg(i,1,ise)),pot(i,1)
                 write(24,*) pres(i,1,ise),pwg(i,1,ise),pot(i,1)
              enddo PressurePts
           enddo Sectns

           !Determine drift forces only if npres>0
           cxis  =motion(1,1)-yg(1)*motion(6,1)+zg(1)*motion(5,1)           !long. motion of c.o.g. of mass
           cetas =motion(2,1)-zg(1)*motion(4,1)+xg(1)*motion(6,1)         !transv. motion of c.o.g. of mass
           czetas=motion(3,1)-xg(1)*motion(5,1)+yg(1)*motion(4,1)        !vertical motion of c.o.g. of mass
           fxi=0.5*mass(1)*ome**2*real(cetas*conjg(motion(6,1))-czetas*conjg(motion(5,1)))  !long.drift f.
           feta=-0.5*mass(1)*ome**2*real(cxis*conjg(motion(6,1)))                  !transverse drift force
           if (.not.ltrwet(iv)) fxi=fxi+0.25*rho*g*(abs(motion(4,1))**2+abs(motion(5,1))**2)*area(1)*(zs(1)-zwl)
           ForAllSections: do ise=1,nse                                               !relative-motion term
              if (ise==1) then
                 dystb=(yint(1,2)-yint(1,1))/2                                                   !for starboard
                 dyprt=(yint(npres,2)-yint(npres,1))/2                                                !for port
              elseif (ise==nse) then
                 dystb=((yint(1,nse)+yint(npres,nse))/2-yint(1,nse-1))/2               !wl breadth zero at stem
                 dyprt=((yint(1,nse)+yint(npres,nse))/2-yint(npres,nse-1))/2
              else                                                                          !not at ship ends
                 dystb=(yint(1,ise+1)-yint(1,ise-1))/2
                 dyprt=(yint(npres,ise+1)-yint(npres,ise-1))/2
              endif
              fxi=fxi+0.25*(abs(pres(1,1,ise))**2*dystb -abs(pres(npres,1,ise))**2*dyprt)/(rho*g)
              feta=feta+0.25*real((pres(1,1,ise)+pres(npres,1,ise))*exp(cik*x(ise)*cosm)* &
                 (exp(-cik*yint(1,ise)*sinm)-exp(-cik*yint(npres,ise)*sinm)))*(x(ise+1)-x(ise-1))/2
            enddo ForAllSections
            write(6,'(a,2f10.3)') ' Longitudinal and transverse drift force per wave amplitude squared ', fxi,feta
            kd=min(waven*(zbot-zwl),30.)                       !k times water depth in case of shallow water
            xdotdr=0.5*waven*om*(cosh(2*(waven*(zdrift-zwl)-kd))/sinh(kd)**2 -1./(waven*(zbot-zwl)*tanh(waven*(zbot-zwl))))
            write(6,'(a,2f10.3)') ' Long., transv. reduced water drift velocity per wave amplitude^2   ', &
               xdotdr*cosm,-xdotdr*sinm
            write(24,*) fxi,feta,xdotdr*cosm,-xdotdr*sinm
         endif IfPressure
      enddo WaveDir
   enddo Speeds
enddo Wavelengths
call signampl                       !calculate significant amplitudes in natural seaways if required
end program pdstrip
