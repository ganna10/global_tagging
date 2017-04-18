  program interpol
  use ncroutines
  use module_write_output
!  implicit none
  integer :: i,j,k,it,ncid,sp,m,l,domain,ii,jj
  real, allocatable,dimension(:) :: long
  real, allocatable,dimension(:) :: lat
  integer, parameter :: nlat=1800, nlong=3600
  integer, parameter :: nlat1=96, nlong1=144
  real :: lat1(96),long1(144),landfrac(96,144)
  real :: latd, longd
  real :: dist  
  real :: latcesm,longcesm
  real, allocatable, dimension(:,:) :: region_code,region_codes
                                       
  character(len=100) :: filename, path
  character(len = 1000) :: outfile
  character(len = 100), parameter :: outfile_method = 'create' 

  path = "/work/users/jco/tagging_global/land_mask"
  domain = 1

     print*, trim(path)
!

!     print*, 'read filename'
     write(filename, &
          '("HTAP_Phase2_tier2NC01x01.nc")')
    print*,filename
   write(outfile,'("Tier2_receptor_regions_1.9x2.5.nc")')
!    print*,outfile
     call nf90call(nf90_open(trim(path)//"/"//trim(filename), &
          NF90_NOWRITE,ncid), &
          "Could not open "//trim(path)//"/"//trim(filename))
!
! get dimensions of area grid and allocate necessary arrays
   allocate(lat(nlat))
   allocate(long(nlong))
   allocate(region_code(nlong,nlat))
   allocate(region_codes(nlong1,nlat1))
   call read2das1dnc(ncid,"lat",(/1,1/),(/nlat,1/),lat)
   call read2das1dnc(ncid,"long",(/1,1/),(/nlong,1/),long)
   call read3das2dnc(ncid,"region_code",(/1,1,1/),(/nlong,nlat,1/),region_code)
   call nf90call(nf90_close(ncid))

! calculate what????
 open(1,file='input.dat')
        k=0
        l=0
 do jj=1,96
  do ii=1,144
  read(1,*,end=101)lat1(jj),long1(ii),landfrac(jj,ii)
   j_loop: do j=1,nlat
     i_loop: do i=1,nlong
      if (long(i) .lt. 0.) then
        long(i)=long(i)+360.
       else
        long(i)=long(i)
      endif
       latd=lat1(jj)-lat(j)
       longd=long1(ii)-long(i) 
       dist=sqrt(latd**2+longd**2) 
       ! starting with oceans
        if ( dist .lt. 0.1 .and. landfrac(jj,ii) .ne. 0 .and. region_code(i,j) .ne. 21 ) then 
             region_codes(ii,jj)=region_code(i,j)
         else if ( dist .lt. 0.1 .and. landfrac(jj,ii) .ne. 0 .and. region_code(i,j) .ne. 20 ) then 
             region_codes(ii,jj)=region_code(i,j)
         else if ( dist .lt. 0.1 .and. landfrac(jj,ii) .ne. 0 .and. region_code(i,j) .ne. 22 ) then 
             region_codes(ii,jj)=region_code(i,j)
         else if ( dist .lt. 0.1 .and. landfrac(jj,ii) .ne. 0 .and. region_code(i,j) .ne. 23 ) then 
             region_codes(ii,jj)=region_code(i,j)
         else if ( dist .lt. 0.1 .and. landfrac(jj,ii) .ne. 0 .and. region_code(i,j) .ne. 24 ) then 
             region_codes(ii,jj)=region_code(i,j)
         else if ( dist .lt. 0.1 .and. landfrac(jj,ii) .ne. 0 .and. region_code(i,j) .ne. 25 ) then 
             region_codes(ii,jj)=region_code(i,j)
         else if ( dist .lt. 0.1 .and. landfrac(jj,ii) .ne. 0 .and. region_code(i,j) .ne. 26 ) then 
             region_codes(ii,jj)=region_code(i,j)
         else if ( dist .lt. 0.1 .and. landfrac(jj,ii) .ne. 0 .and. region_code(i,j) .ne. 27 ) then 
             region_codes(ii,jj)=region_code(i,j)
         else if ( dist .lt. 0.1 .and. landfrac(jj,ii) .ne. 0 .and. region_code(i,j) .ne. 28 ) then 
             region_codes(ii,jj)=region_code(i,j) 
         else
        if ( dist .lt. 0.1 .and. landfrac(jj,ii) .eq. 0 .and. region_code(i,j) .eq. 21 ) then 
            region_codes(ii,jj)=21     
        else if (dist .lt. 0.1 .and. landfrac(jj,ii) .eq. 0 .and. region_code(i,j) .eq. 20 ) then 
            region_codes(ii,jj)=20     
        else if (dist .lt. 0.1 .and. landfrac(jj,ii) .eq. 0 .and. region_code(i,j) .eq. 22 ) then 
            region_codes(ii,jj)=22     
        else if (dist .lt. 0.1 .and. landfrac(jj,ii) .eq. 0 .and. region_code(i,j) .eq. 23 ) then 
            region_codes(ii,jj)=23
        else if (dist .lt. 0.1 .and. landfrac(jj,ii) .eq. 0 .and. region_code(i,j) .eq. 24 ) then 
            region_codes(ii,jj)=24
        else if (dist .lt. 0.1 .and. landfrac(jj,ii) .eq. 0 .and. region_code(i,j) .eq. 25 ) then 
            region_codes(ii,jj)=25
        else if (dist .lt. 0.1 .and. landfrac(jj,ii) .eq. 0 .and. region_code(i,j) .eq. 26 ) then 
            region_codes(ii,jj)=26
        else if (dist .lt. 0.1 .and. landfrac(jj,ii) .eq. 0 .and. region_code(i,j) .eq. 27 ) then 
            region_codes(ii,jj)=27
        else if (dist .lt. 0.1 .and. landfrac(jj,ii) .eq. 0 .and. region_code(i,j) .eq. 28 ) then 
            region_codes(ii,jj)=28 
         endif
         ! polar regions
        if ( dist .lt. 0.1 .and. landfrac(jj,ii) .eq. 0 .and. region_code(i,j) .eq. 160 ) then 
            region_codes(ii,jj)=160
         endif
        if ( dist .lt. 0.1 .and. landfrac(jj,ii) .eq. 0 .and. region_code(i,j) .eq. 170 ) then 
            region_codes(ii,jj)=170
         endif
        if ( dist .lt. 0.1 .and. landfrac(jj,ii) .eq. 0 .and. region_code(i,j) .eq. 171 ) then 
            region_codes(ii,jj)=171
         endif
         ! check land areas around grid point
!        if ( dist .lt. 0.1 .and. landfrac(jj,ii).ne.0 .and. region_code(i,j) .eq. 21 .or. region_code(i,j) .eq. 20 ) then
!         region_codes(ii,jj)=max(           region_code(i-14 ,j-11), &
!           region_code(i-14 ,j-10), &
!           region_code(i-14 ,j-9), &
!           region_code(i-14 ,j-8), &
!           region_code(i-14 ,j-7), &
!           region_code(i-14 ,j-6), &
!           region_code(i-14 ,j-5), &
!           region_code(i-14 ,j-4), &
!           region_code(i-14 ,j-3), &
!           region_code(i-14 ,j-2), &
!           region_code(i-14 ,j-1), &
!           region_code(i-14 ,j), &
!           region_code(i-14 ,j+1), &
!           region_code(i-14 ,j+2), &
!           region_code(i-14 ,j+3), &
!           region_code(i-14 ,j+4), &
!           region_code(i-14 ,j+5), &
!           region_code(i-14 ,j+6), &
!           region_code(i-14 ,j+7), &
!           region_code(i-14 ,j+8), &
!           region_code(i-14 ,j+9), &
!           region_code(i-14 ,j+10), &
!           region_code(i-14 ,j+11), &
!           region_code(i-13 ,j-11), &
!           region_code(i-13 ,j-10), &
!           region_code(i-13 ,j-9), &
!           region_code(i-13 ,j-8), &
!           region_code(i-13 ,j-7), &
!           region_code(i-13 ,j-6), &
!           region_code(i-13 ,j-5), &
!           region_code(i-13 ,j-4), &
!           region_code(i-13 ,j-3), &
!           region_code(i-13 ,j-2), &
!           region_code(i-13 ,j-1), &
!           region_code(i-13 ,j), &
!           region_code(i-13 ,j+1), &
!           region_code(i-13 ,j+2), &
!           region_code(i-13 ,j+3), &
!           region_code(i-13 ,j+4), &
!           region_code(i-13 ,j+5), &
!           region_code(i-13 ,j+6), &
!           region_code(i-13 ,j+7), &
!           region_code(i-13 ,j+8), &
!           region_code(i-13 ,j+9), &
!           region_code(i-13 ,j+10), &
!           region_code(i-13 ,j+11), &
!           region_code(i-12 ,j-11), &
!           region_code(i-12 ,j-10), &
!           region_code(i-12 ,j-9), &
!           region_code(i-12 ,j-8), &
!           region_code(i-12 ,j-7), &
!           region_code(i-12 ,j-6), &
!           region_code(i-12 ,j-5), &
!           region_code(i-12 ,j-4), &
!           region_code(i-12 ,j-3), &
!           region_code(i-12 ,j-2), &
!           region_code(i-12 ,j-1), &
!           region_code(i-12 ,j), &
!           region_code(i-12 ,j+1), &
!           region_code(i-12 ,j+2), &
!           region_code(i-12 ,j+3), &
!           region_code(i-12 ,j+4), &
!           region_code(i-12 ,j+5), &
!           region_code(i-12 ,j+6), &
!           region_code(i-12 ,j+7), &
!           region_code(i-12 ,j+8), &
!           region_code(i-12 ,j+9), &
!           region_code(i-12 ,j+10), &
!           region_code(i-12 ,j+11), &
!           region_code(i-11 ,j-11), &
!           region_code(i-11 ,j-10), &
!           region_code(i-11 ,j-9), &
!           region_code(i-11 ,j-8), &
!           region_code(i-11 ,j-7), &
!           region_code(i-11 ,j-6), &
!           region_code(i-11 ,j-5), &
!           region_code(i-11 ,j-4), &
!           region_code(i-11 ,j-3), &
!           region_code(i-11 ,j-2), &
!           region_code(i-11 ,j-1), &
!           region_code(i-11 ,j), &
!           region_code(i-11 ,j+1), &
!           region_code(i-11 ,j+2), &
!           region_code(i-11 ,j+3), &
!           region_code(i-11 ,j+4), &
!           region_code(i-11 ,j+5), &
!           region_code(i-11 ,j+6), &
!           region_code(i-11 ,j+7), &
!           region_code(i-11 ,j+8), &
!           region_code(i-11 ,j+9), &
!           region_code(i-11 ,j+10), &
!           region_code(i-11 ,j+11), &
!           region_code(i-10 ,j-11), &
!           region_code(i-10 ,j-10), &
!           region_code(i-10 ,j-9), &
!           region_code(i-10 ,j-8), &
!           region_code(i-10 ,j-7), &
!           region_code(i-10 ,j-6), &
!           region_code(i-10 ,j-5), &
!           region_code(i-10 ,j-4), &
!           region_code(i-10 ,j-3), &
!           region_code(i-10 ,j-2), &
!           region_code(i-10 ,j-1), &
!           region_code(i-10 ,j), &
!           region_code(i-10 ,j+1), &
!           region_code(i-10 ,j+2), &
!           region_code(i-10 ,j+3), &
!           region_code(i-10 ,j+4), &
!           region_code(i-10 ,j+5), &
!           region_code(i-10 ,j+6), &
!           region_code(i-10 ,j+7), &
!           region_code(i-10 ,j+8), &
!           region_code(i-10 ,j+9), &
!           region_code(i-10 ,j+10), &
!           region_code(i-10 ,j+11), &
!           region_code(i-9 ,j-11), &
!           region_code(i-9 ,j-10), &
!           region_code(i-9 ,j-9), &
!           region_code(i-9 ,j-8), &
!           region_code(i-9 ,j-7), &
!           region_code(i-9 ,j-6), &
!           region_code(i-9 ,j-5), &
!           region_code(i-9 ,j-4), &
!           region_code(i-9 ,j-3), &
!           region_code(i-9 ,j-2), &
!           region_code(i-9 ,j-1), &
!           region_code(i-9 ,j), &
!           region_code(i-9 ,j+1), &
!           region_code(i-9 ,j+2), &
!           region_code(i-9 ,j+3), &
!           region_code(i-9 ,j+4), &
!           region_code(i-9 ,j+5), &
!           region_code(i-9 ,j+6), &
!           region_code(i-9 ,j+7), &
!           region_code(i-9 ,j+8), &
!           region_code(i-9 ,j+9), &
!           region_code(i-9 ,j+10), &
!           region_code(i-9 ,j+11), &
!           region_code(i-8 ,j-11), &
!           region_code(i-8 ,j-10), &
!           region_code(i-8 ,j-9), &
!           region_code(i-8 ,j-8), &
!           region_code(i-8 ,j-7), &
!           region_code(i-8 ,j-6), &
!           region_code(i-8 ,j-5), &
!           region_code(i-8 ,j-4), &
!           region_code(i-8 ,j-3), &
!           region_code(i-8 ,j-2), &
!           region_code(i-8 ,j-1), &
!           region_code(i-8 ,j), &
!           region_code(i-8 ,j+1), &
!           region_code(i-8 ,j+2), &
!           region_code(i-8 ,j+3), &
!           region_code(i-8 ,j+4), &
!           region_code(i-8 ,j+5), &
!           region_code(i-8 ,j+6), &
!           region_code(i-8 ,j+7), &
!           region_code(i-8 ,j+8), &
!           region_code(i-8 ,j+9), &
!           region_code(i-8 ,j+10), &
!           region_code(i-8 ,j+11), &
!           region_code(i-7 ,j-11), &
!           region_code(i-7 ,j-10), &
!           region_code(i-7 ,j-9), &
!           region_code(i-7 ,j-8), &
!           region_code(i-7 ,j-7), &
!           region_code(i-7 ,j-6), &
!           region_code(i-7 ,j-5), &
!           region_code(i-7 ,j-4), &
!           region_code(i-7 ,j-3), &
!           region_code(i-7 ,j-2), &
!           region_code(i-7 ,j-1), &
!           region_code(i-7 ,j), &
!           region_code(i-7 ,j+1), &
!           region_code(i-7 ,j+2), &
!           region_code(i-7 ,j+3), &
!           region_code(i-7 ,j+4), &
!           region_code(i-7 ,j+5), &
!           region_code(i-7 ,j+6), &
!           region_code(i-7 ,j+7), &
!           region_code(i-7 ,j+8), &
!           region_code(i-7 ,j+9), &
!           region_code(i-7 ,j+10), &
!           region_code(i-7 ,j+11), &
!           region_code(i-6 ,j-11), &
!           region_code(i-6 ,j-10), &
!           region_code(i-6 ,j-9), &
!           region_code(i-6 ,j-8), &
!           region_code(i-6 ,j-7), &
!           region_code(i-6 ,j-6), &
!           region_code(i-6 ,j-5), &
!           region_code(i-6 ,j-4), &
!           region_code(i-6 ,j-3), &
!           region_code(i-6 ,j-2), &
!           region_code(i-6 ,j-1), &
!           region_code(i-6 ,j), &
!           region_code(i-6 ,j+1), &
!           region_code(i-6 ,j+2), &
!           region_code(i-6 ,j+3), &
!           region_code(i-6 ,j+4), &
!           region_code(i-6 ,j+5), &
!           region_code(i-6 ,j+6), &
!           region_code(i-6 ,j+7), &
!           region_code(i-6 ,j+8), &
!           region_code(i-6 ,j+9), &
!           region_code(i-6 ,j+10), &
!           region_code(i-6 ,j+11), &
!           region_code(i-5 ,j-11), &
!           region_code(i-5 ,j-10), &
!           region_code(i-5 ,j-9), &
!           region_code(i-5 ,j-8), &
!           region_code(i-5 ,j-7), &
!           region_code(i-5 ,j-6), &
!           region_code(i-5 ,j-5), &
!           region_code(i-5 ,j-4), &
!           region_code(i-5 ,j-3), &
!           region_code(i-5 ,j-2), &
!           region_code(i-5 ,j-1), &
!           region_code(i-5 ,j), &
!           region_code(i-5 ,j+1), &
!           region_code(i-5 ,j+2), &
!           region_code(i-5 ,j+3), &
!           region_code(i-5 ,j+4), &
!           region_code(i-5 ,j+5), &
!           region_code(i-5 ,j+6), &
!           region_code(i-5 ,j+7), &
!           region_code(i-5 ,j+8), &
!           region_code(i-5 ,j+9), &
!           region_code(i-5 ,j+10), &
!           region_code(i-5 ,j+11), &
!           region_code(i-4 ,j-11), &
!           region_code(i-4 ,j-10), &
!           region_code(i-4 ,j-9), &
!           region_code(i-4 ,j-8), &
!           region_code(i-4 ,j-7), &
!           region_code(i-4 ,j-6), &
!           region_code(i-4 ,j-5), &
!           region_code(i-4 ,j-4), &
!           region_code(i-4 ,j-3), &
!           region_code(i-4 ,j-2), &
!           region_code(i-4 ,j-1), &
!           region_code(i-4 ,j), &
!           region_code(i-4 ,j+1), &
!           region_code(i-4 ,j+2), &
!           region_code(i-4 ,j+3), &
!           region_code(i-4 ,j+4), &
!           region_code(i-4 ,j+5), &
!           region_code(i-4 ,j+6), &
!           region_code(i-4 ,j+7), &
!           region_code(i-4 ,j+8), &
!           region_code(i-4 ,j+9), &
!           region_code(i-4 ,j+10), &
!           region_code(i-4 ,j+11), &
!           region_code(i-3 ,j-11), &
!           region_code(i-3 ,j-10), &
!           region_code(i-3 ,j-9), &
!           region_code(i-3 ,j-8), &
!           region_code(i-3 ,j-7), &
!           region_code(i-3 ,j-6), &
!           region_code(i-3 ,j-5), &
!           region_code(i-3 ,j-4), &
!           region_code(i-3 ,j-3), &
!           region_code(i-3 ,j-2), &
!           region_code(i-3 ,j-1), &
!           region_code(i-3 ,j), &
!           region_code(i-3 ,j+1), &
!           region_code(i-3 ,j+2), &
!           region_code(i-3 ,j+3), &
!           region_code(i-3 ,j+4), &
!           region_code(i-3 ,j+5), &
!           region_code(i-3 ,j+6), &
!           region_code(i-3 ,j+7), &
!           region_code(i-3 ,j+8), &
!           region_code(i-3 ,j+9), &
!           region_code(i-3 ,j+10), &
!           region_code(i-3 ,j+11), &
!           region_code(i-2 ,j-11), &
!           region_code(i-2 ,j-10), &
!           region_code(i-2 ,j-9), &
!           region_code(i-2 ,j-8), &
!           region_code(i-2 ,j-7), &
!           region_code(i-2 ,j-6), &
!           region_code(i-2 ,j-5), &
!           region_code(i-2 ,j-4), &
!           region_code(i-2 ,j-3), &
!           region_code(i-2 ,j-2), &
!           region_code(i-2 ,j-1), &
!           region_code(i-2 ,j), &
!           region_code(i-2 ,j+1), &
!           region_code(i-2 ,j+2), &
!           region_code(i-2 ,j+3), &
!           region_code(i-2 ,j+4), &
!           region_code(i-2 ,j+5), &
!           region_code(i-2 ,j+6), &
!           region_code(i-2 ,j+7), &
!           region_code(i-2 ,j+8), &
!           region_code(i-2 ,j+9), &
!           region_code(i-2 ,j+10), &
!           region_code(i-2 ,j+11), &
!           region_code(i-1 ,j-11), &
!           region_code(i-1 ,j-10), &
!           region_code(i-1 ,j-9), &
!           region_code(i-1 ,j-8), &
!           region_code(i-1 ,j-7), &
!           region_code(i-1 ,j-6), &
!           region_code(i-1 ,j-5), &
!           region_code(i-1 ,j-4), &
!           region_code(i-1 ,j-3), &
!           region_code(i-1 ,j-2), &
!           region_code(i-1 ,j-1), &
!           region_code(i-1 ,j), &
!           region_code(i-1 ,j+1), &
!           region_code(i-1 ,j+2), &
!           region_code(i-1 ,j+3), &
!           region_code(i-1 ,j+4), &
!           region_code(i-1 ,j+5), &
!           region_code(i-1 ,j+6), &
!           region_code(i-1 ,j+7), &
!           region_code(i-1 ,j+8), &
!           region_code(i-1 ,j+9), &
!           region_code(i-1 ,j+10), &
!           region_code(i-1 ,j+11), &
!           region_code(i ,j-11), &
!           region_code(i ,j-10), &
!           region_code(i ,j-9), &
!           region_code(i ,j-8), &
!           region_code(i ,j-7), &
!           region_code(i ,j-6), &
!           region_code(i ,j-5), &
!           region_code(i ,j-4), &
!           region_code(i ,j-3), &
!           region_code(i ,j-2), &
!           region_code(i ,j-1), &
!           region_code(i ,j), &
!           region_code(i ,j+1), &
!           region_code(i ,j+2), &
!           region_code(i ,j+3), &
!           region_code(i ,j+4), &
!           region_code(i ,j+5), &
!           region_code(i ,j+6), &
!           region_code(i ,j+7), &
!           region_code(i ,j+8), &
!           region_code(i ,j+9), &
!           region_code(i ,j+10), &
!           region_code(i ,j+11), &
!           region_code(i+1 ,j-11), &
!           region_code(i+1 ,j-10), &
!           region_code(i+1 ,j-9), &
!           region_code(i+1 ,j-8), &
!           region_code(i+1 ,j-7), &
!           region_code(i+1 ,j-6), &
!           region_code(i+1 ,j-5), &
!           region_code(i+1 ,j-4), &
!           region_code(i+1 ,j-3), &
!           region_code(i+1 ,j-2), &
!           region_code(i+1 ,j-1), &
!           region_code(i+1 ,j), &
!           region_code(i+1 ,j+1), &
!           region_code(i+1 ,j+2), &
!           region_code(i+1 ,j+3), &
!           region_code(i+1 ,j+4), &
!           region_code(i+1 ,j+5), &
!           region_code(i+1 ,j+6), &
!           region_code(i+1 ,j+7), &
!           region_code(i+1 ,j+8), &
!           region_code(i+1 ,j+9), &
!           region_code(i+1 ,j+10), &
!           region_code(i+1 ,j+11), &
!           region_code(i+2 ,j-11), &
!           region_code(i+2 ,j-10), &
!           region_code(i+2 ,j-9), &
!           region_code(i+2 ,j-8), &
!           region_code(i+2 ,j-7), &
!           region_code(i+2 ,j-6), &
!           region_code(i+2 ,j-5), &
!           region_code(i+2 ,j-4), &
!           region_code(i+2 ,j-3), &
!           region_code(i+2 ,j-2), &
!           region_code(i+2 ,j-1), &
!           region_code(i+2 ,j), &
!           region_code(i+2 ,j+1), &
!           region_code(i+2 ,j+2), &
!           region_code(i+2 ,j+3), &
!           region_code(i+2 ,j+4), &
!           region_code(i+2 ,j+5), &
!           region_code(i+2 ,j+6), &
!           region_code(i+2 ,j+7), &
!           region_code(i+2 ,j+8), &
!           region_code(i+2 ,j+9), &
!           region_code(i+2 ,j+10), &
!           region_code(i+2 ,j+11), &
!           region_code(i+3 ,j-11), &
!           region_code(i+3 ,j-10), &
!           region_code(i+3 ,j-9), &
!           region_code(i+3 ,j-8), &
!           region_code(i+3 ,j-7), &
!           region_code(i+3 ,j-6), &
!           region_code(i+3 ,j-5), &
!           region_code(i+3 ,j-4), &
!           region_code(i+3 ,j-3), &
!           region_code(i+3 ,j-2), &
!           region_code(i+3 ,j-1), &
!           region_code(i+3 ,j), &
!           region_code(i+3 ,j+1), &
!           region_code(i+3 ,j+2), &
!           region_code(i+3 ,j+3), &
!           region_code(i+3 ,j+4), &
!           region_code(i+3 ,j+5), &
!           region_code(i+3 ,j+6), &
!           region_code(i+3 ,j+7), &
!           region_code(i+3 ,j+8), &
!           region_code(i+3 ,j+9), &
!           region_code(i+3 ,j+10), &
!           region_code(i+3 ,j+11), &
!           region_code(i+4 ,j-11), &
!           region_code(i+4 ,j-10), &
!           region_code(i+4 ,j-9), &
!           region_code(i+4 ,j-8), &
!           region_code(i+4 ,j-7), &
!           region_code(i+4 ,j-6), &
!           region_code(i+4 ,j-5), &
!           region_code(i+4 ,j-4), &
!           region_code(i+4 ,j-3), &
!           region_code(i+4 ,j-2), &
!           region_code(i+4 ,j-1), &
!           region_code(i+4 ,j), &
!           region_code(i+4 ,j+1), &
!           region_code(i+4 ,j+2), &
!           region_code(i+4 ,j+3), &
!           region_code(i+4 ,j+4), &
!           region_code(i+4 ,j+5), &
!           region_code(i+4 ,j+6), &
!           region_code(i+4 ,j+7), &
!           region_code(i+4 ,j+8), &
!           region_code(i+4 ,j+9), &
!           region_code(i+4 ,j+10), &
!           region_code(i+4 ,j+11), &
!           region_code(i+5 ,j-11), &
!           region_code(i+5 ,j-10), &
!           region_code(i+5 ,j-9), &
!           region_code(i+5 ,j-8), &
!           region_code(i+5 ,j-7), &
!           region_code(i+5 ,j-6), &
!           region_code(i+5 ,j-5), &
!           region_code(i+5 ,j-4), &
!           region_code(i+5 ,j-3), &
!           region_code(i+5 ,j-2), &
!           region_code(i+5 ,j-1), &
!           region_code(i+5 ,j), &
!           region_code(i+5 ,j+1), &
!           region_code(i+5 ,j+2), &
!           region_code(i+5 ,j+3), &
!           region_code(i+5 ,j+4), &
!           region_code(i+5 ,j+5), &
!           region_code(i+5 ,j+6), &
!           region_code(i+5 ,j+7), &
!           region_code(i+5 ,j+8), &
!           region_code(i+5 ,j+9), &
!           region_code(i+5 ,j+10), &
!           region_code(i+5 ,j+11), &
!           region_code(i+6 ,j-11), &
!           region_code(i+6 ,j-10), &
!           region_code(i+6 ,j-9), &
!           region_code(i+6 ,j-8), &
!           region_code(i+6 ,j-7), &
!           region_code(i+6 ,j-6), &
!           region_code(i+6 ,j-5), &
!           region_code(i+6 ,j-4), &
!           region_code(i+6 ,j-3), &
!           region_code(i+6 ,j-2), &
!           region_code(i+6 ,j-1), &
!           region_code(i+6 ,j), &
!           region_code(i+6 ,j+1), &
!           region_code(i+6 ,j+2), &
!           region_code(i+6 ,j+3), &
!           region_code(i+6 ,j+4), &
!           region_code(i+6 ,j+5), &
!           region_code(i+6 ,j+6), &
!           region_code(i+6 ,j+7), &
!           region_code(i+6 ,j+8), &
!           region_code(i+6 ,j+9), &
!           region_code(i+6 ,j+10), &
!           region_code(i+6 ,j+11), &
!           region_code(i+7 ,j-11), &
!           region_code(i+7 ,j-10), &
!           region_code(i+7 ,j-9), &
!           region_code(i+7 ,j-8), &
!           region_code(i+7 ,j-7), &
!           region_code(i+7 ,j-6), &
!           region_code(i+7 ,j-5), &
!           region_code(i+7 ,j-4), &
!           region_code(i+7 ,j-3), &
!           region_code(i+7 ,j-2), &
!           region_code(i+7 ,j-1), &
!           region_code(i+7 ,j), &
!           region_code(i+7 ,j+1), &
!           region_code(i+7 ,j+2), &
!           region_code(i+7 ,j+3), &
!           region_code(i+7 ,j+4), &
!           region_code(i+7 ,j+5), &
!           region_code(i+7 ,j+6), &
!           region_code(i+7 ,j+7), &
!           region_code(i+7 ,j+8), &
!           region_code(i+7 ,j+9), &
!           region_code(i+7 ,j+10), &
!           region_code(i+7 ,j+11), &
!           region_code(i+8 ,j-11), &
!           region_code(i+8 ,j-10), &
!           region_code(i+8 ,j-9), &
!           region_code(i+8 ,j-8), &
!           region_code(i+8 ,j-7), &
!           region_code(i+8 ,j-6), &
!           region_code(i+8 ,j-5), &
!           region_code(i+8 ,j-4), &
!           region_code(i+8 ,j-3), &
!           region_code(i+8 ,j-2), &
!           region_code(i+8 ,j-1), &
!           region_code(i+8 ,j), &
!           region_code(i+8 ,j+1), &
!           region_code(i+8 ,j+2), &
!           region_code(i+8 ,j+3), &
!           region_code(i+8 ,j+4), &
!           region_code(i+8 ,j+5), &
!           region_code(i+8 ,j+6), &
!           region_code(i+8 ,j+7), &
!           region_code(i+8 ,j+8), &
!           region_code(i+8 ,j+9), &
!           region_code(i+8 ,j+10), &
!           region_code(i+8 ,j+11), &
!           region_code(i+9 ,j-11), &
!           region_code(i+9 ,j-10), &
!           region_code(i+9 ,j-9), &
!           region_code(i+9 ,j-8), &
!           region_code(i+9 ,j-7), &
!           region_code(i+9 ,j-6), &
!           region_code(i+9 ,j-5), &
!           region_code(i+9 ,j-4), &
!           region_code(i+9 ,j-3), &
!           region_code(i+9 ,j-2), &
!           region_code(i+9 ,j-1), &
!           region_code(i+9 ,j), &
!           region_code(i+9 ,j+1), &
!           region_code(i+9 ,j+2), &
!           region_code(i+9 ,j+3), &
!           region_code(i+9 ,j+4), &
!           region_code(i+9 ,j+5), &
!           region_code(i+9 ,j+6), &
!           region_code(i+9 ,j+7), &
!           region_code(i+9 ,j+8), &
!           region_code(i+9 ,j+9), &
!           region_code(i+9 ,j+10), &
!           region_code(i+9 ,j+11), &
!           region_code(i+10 ,j-11), &
!           region_code(i+10 ,j-10), &
!           region_code(i+10 ,j-9), &
!           region_code(i+10 ,j-8), &
!           region_code(i+10 ,j-7), &
!           region_code(i+10 ,j-6), &
!           region_code(i+10 ,j-5), &
!           region_code(i+10 ,j-4), &
!           region_code(i+10 ,j-3), &
!           region_code(i+10 ,j-2), &
!           region_code(i+10 ,j-1), &
!           region_code(i+10 ,j), &
!           region_code(i+10 ,j+1), &
!           region_code(i+10 ,j+2), &
!           region_code(i+10 ,j+3), &
!           region_code(i+10 ,j+4), &
!           region_code(i+10 ,j+5), &
!           region_code(i+10 ,j+6), &
!           region_code(i+10 ,j+7), &
!           region_code(i+10 ,j+8), &
!           region_code(i+10 ,j+9), &
!           region_code(i+10 ,j+10), &
!           region_code(i+10 ,j+11), &
!           region_code(i+11 ,j-11), &
!           region_code(i+11 ,j-10), &
!           region_code(i+11 ,j-9), &
!           region_code(i+11 ,j-8), &
!           region_code(i+11 ,j-7), &
!           region_code(i+11 ,j-6), &
!           region_code(i+11 ,j-5), &
!           region_code(i+11 ,j-4), &
!           region_code(i+11 ,j-3), &
!           region_code(i+11 ,j-2), &
!           region_code(i+11 ,j-1), &
!           region_code(i+11 ,j), &
!           region_code(i+11 ,j+1), &
!           region_code(i+11 ,j+2), &
!           region_code(i+11 ,j+3), &
!           region_code(i+11 ,j+4), &
!           region_code(i+11 ,j+5), &
!           region_code(i+11 ,j+6), &
!           region_code(i+11 ,j+7), &
!           region_code(i+11 ,j+8), &
!           region_code(i+11 ,j+9), &
!           region_code(i+11 ,j+10), &
!           region_code(i+11 ,j+11), &
!           region_code(i+12 ,j-11), &
!           region_code(i+12 ,j-10), &
!           region_code(i+12 ,j-9), &
!           region_code(i+12 ,j-8), &
!           region_code(i+12 ,j-7), &
!           region_code(i+12 ,j-6), &
!           region_code(i+12 ,j-5), &
!           region_code(i+12 ,j-4), &
!           region_code(i+12 ,j-3), &
!           region_code(i+12 ,j-2), &
!           region_code(i+12 ,j-1), &
!           region_code(i+12 ,j), &
!           region_code(i+12 ,j+1), &
!           region_code(i+12 ,j+2), &
!           region_code(i+12 ,j+3), &
!           region_code(i+12 ,j+4), &
!           region_code(i+12 ,j+5), &
!           region_code(i+12 ,j+6), &
!           region_code(i+12 ,j+7), &
!           region_code(i+12 ,j+8), &
!           region_code(i+12 ,j+9), &
!           region_code(i+12 ,j+10), &
!           region_code(i+12 ,j+11), &
!           region_code(i+13 ,j-11), &
!           region_code(i+13 ,j-10), &
!           region_code(i+13 ,j-9), &
!           region_code(i+13 ,j-8), &
!           region_code(i+13 ,j-7), &
!           region_code(i+13 ,j-6), &
!           region_code(i+13 ,j-5), &
!           region_code(i+13 ,j-4), &
!           region_code(i+13 ,j-3), &
!           region_code(i+13 ,j-2), &
!           region_code(i+13 ,j-1), &
!           region_code(i+13 ,j), &
!           region_code(i+13 ,j+1), &
!           region_code(i+13 ,j+2), &
!           region_code(i+13 ,j+3), &
!           region_code(i+13 ,j+4), &
!           region_code(i+13 ,j+5), &
!           region_code(i+13 ,j+6), &
!           region_code(i+13 ,j+7), &
!           region_code(i+13 ,j+8), &
!           region_code(i+13 ,j+9), &
!           region_code(i+13 ,j+10), &
!           region_code(i+13 ,j+11), &
!           region_code(i+14 ,j-11), &
!           region_code(i+14 ,j-10), &
!           region_code(i+14 ,j-9), &
!           region_code(i+14 ,j-8), &
!           region_code(i+14 ,j-7), &
!           region_code(i+14 ,j-6), &
!           region_code(i+14 ,j-5), &
!           region_code(i+14 ,j-4), &
!           region_code(i+14 ,j-3), &
!           region_code(i+14 ,j-2), &
!           region_code(i+14 ,j-1), &
!           region_code(i+14 ,j), &
!           region_code(i+14 ,j+1), &
!           region_code(i+14 ,j+2), &
!           region_code(i+14 ,j+3), &
!           region_code(i+14 ,j+4), &
!           region_code(i+14 ,j+5), &
!           region_code(i+14 ,j+6), &
!           region_code(i+14 ,j+7), &
!           region_code(i+14 ,j+8), &
!           region_code(i+14 ,j+9), &
!           region_code(i+14 ,j+10), &
!           region_code(i+14 ,j+11))
!         endif
              region_codes(4,82) = 41
              region_codes(5,82) = 41
              region_codes(3,82) = 41
              region_codes(3,81) = 41
              region_codes(3,80) = 41
              region_codes(6,83) = 41
              region_codes(5,79) = 41
              region_codes(4,79) = 41
              region_codes(5,78) = 41
              region_codes(4,78) = 41
              region_codes(3,77) = 41
              region_codes(4,77) = 41
              region_codes(11,83) = 41
              region_codes(10,83) = 41
              region_codes(9,82) = 41
              region_codes(8,81) = 41
              region_codes(7,78) = 41 
              region_codes(143,74) = 41
              region_codes(144,75) = 41
              region_codes(143,78) = 41
              region_codes(143,79) = 41
              region_codes(142,79) = 41
              region_codes(144,79) = 41
              region_codes(143,77) = 41
              region_codes(143,76) = 41
              region_codes(142,76) = 41
              region_codes(143,75) = 41
              region_codes(136,83) = 41
              region_codes(136,82) = 41
              region_codes(137,82) = 41
              region_codes(138,82) = 41
              region_codes(139,82) = 41

              region_codes(142,68) = 42
              region_codes(7,68) = 42
              region_codes(6,68) = 42
              region_codes(8,69) = 42
              region_codes(5,70) = 42
              region_codes(5,71) = 42
              region_codes(5,72) = 42
              region_codes(5,73) = 42
              region_codes(7,71) = 42
              region_codes(6,72) = 42
              
              region_codes(10,68) = 44
              region_codes(10,69) = 44
              region_codes(10,70) = 44
              region_codes(11,67) = 44

              region_codes(113,63) = 32
              region_codes(113,62) = 32
              region_codes(112,62) = 32
              region_codes(108,64) = 32
              region_codes(109,64) = 32
              region_codes(104,64) = 32
              region_codes(103,64) = 32
              region_codes(105,63) = 32
              region_codes(106,62) = 32

              region_codes(115,73) = 35
              region_codes(116,73) = 35 
              region_codes(117,82) = 35
              region_codes(116,82) = 35
              region_codes(118,82) = 35
              region_codes(119,82) = 35
              region_codes(119,81) = 35
              region_codes(118,81) = 35 
              region_codes(119,75) = 35
              region_codes(120,75) = 35
              region_codes(121,75) = 35

              ! 34 
              region_codes(97,66) = 34
              region_codes(98,66) = 34
              region_codes(96,68) = 34
              region_codes(100,65) = 34
              region_codes(101,65) = 34
              region_codes(102,65) = 34
              
              region_codes(85,80) = 36
              region_codes(84,80) = 36
              region_codes(86,80) = 36
              region_codes(87,80) = 36
              region_codes(88,80) = 36
              region_codes(83,79) = 36
              region_codes(78,77) = 36
              region_codes(77,77) = 36
              region_codes(77,76) = 36
              region_codes(76,76) = 36
              region_codes(70,76) = 36

              region_codes(45,58) = 62
              region_codes(46,60) = 62
              region_codes(48,61) = 62
              region_codes(49,62) = 62
              region_codes(49,67) = 62

              !65
              region_codes(58,70) = 65
              region_codes(57,67) = 65
              region_codes(55,66) = 65
              region_codes(59,71) = 65
              region_codes(59,72) = 65
              region_codes(53,65) = 65
              
              !64 
              region_codes(53,67) = 64
              region_codes(53,68) = 64
              region_codes(53,70) = 64
              region_codes(51,69) = 64

              ! 52
              region_codes(34,57) = 52
              region_codes(35,58) = 52
              
              region_codes(33,55) = 52
              region_codes(33,54) = 52
              region_codes(33,53) = 52
              region_codes(34,52) = 52
              region_codes(31,53) = 52
              region_codes(31,54) = 52

              region_codes(30,57) = 52
              region_codes(30,58) = 52
              region_codes(30,59) = 52
              region_codes(31,55) = 52

              !51
              region_codes(38,60) = 51
              region_codes(39,61) = 51
              region_codes(39,62) = 51

              !142
              region_codes(67,80) = 142
              region_codes(66,78) = 142
              region_codes(65,77) = 142
              region_codes(64,76) = 142
              region_codes(65,76) = 142
              region_codes(70,81) = 142
              region_codes(66,81) = 142
              region_codes(67,81) = 142
              region_codes(65,81) = 142

              region_codes(71,81) = 142
              region_codes(64,81) = 142
              region_codes(63,80) = 142
              region_codes(61,80) = 142
              region_codes(56,78) = 142
             !print 77,i,j,ii,jj,l,k,dist,landfrac(jj,ii),region_code(i,j),region_codes(ii,jj) 
         endif
       enddo i_loop
      enddo j_loop 
     enddo ! jj loop 
    enddo ! jj loop 
    close(1)
  call write_output_netcdf(outfile_method, outfile, nlong1, nlat1, &
       long1,lat1,region_codes) 

  deallocate( lat,long, region_code, region_codes)
101 continue
77 format(6(i6,x),4f12.5)
78 format(2f12.5)
 stop 
 end program interpol


