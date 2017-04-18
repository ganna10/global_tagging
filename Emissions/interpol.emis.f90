  program interpol
  use ncroutines
  use module_write_output
!  implicit none
  integer :: i,j,k,it,ncid,sp,m,l,domain,ii,jj
  real, allocatable,dimension(:) :: lon
  real, allocatable,dimension(:) :: lat,time
  integer, allocatable,dimension(:) :: date
  integer, parameter :: nlat=96, nlon=144, ntime=60
  real, allocatable, dimension(:,:) :: region_codes
  real, allocatable, dimension(:,:,:) :: anthro
                                       
  real, allocatable, dimension(:,:,:) :: NAM
  real, allocatable, dimension(:,:,:) :: EUR
  real, allocatable, dimension(:,:,:) :: EAS
  real, allocatable, dimension(:,:,:) :: SAS
  real, allocatable, dimension(:,:,:) :: CAS
  real, allocatable, dimension(:,:,:) :: RBU
  real, allocatable, dimension(:,:,:) :: MDE
  real, allocatable, dimension(:,:,:) :: SEA
  real, allocatable, dimension(:,:,:) :: NAF
  real, allocatable, dimension(:,:,:) :: MCA
  real, allocatable, dimension(:,:,:) :: OCN
  real, allocatable, dimension(:,:,:) :: REST
  character(len=100) :: filename, path,filename1
  character(len = 1000) :: outfile1,outfile2,outfile3
  character(len = 1000) :: outfile4,outfile5,outfile6
  character(len = 1000) :: outfile7,outfile8,outfile9
  character(len = 1000) :: outfile10,outfile11,outfile12
  character(len = 100), parameter :: outfile_method = 'create' 

  path = "/work/users/jco/tagging_global/assign_emissions/NOx_Tagging.20170406"
     print*, trim(path)
     write(filename,&
          '("Tier1_receptor_regions_1.9x2.5.nc")')
    call nf90call(nf90_open(trim(path)//"/"//trim(filename), &
          NF90_NOWRITE,ncid), &
          "Could not open "//trim(path)//"/"//trim(filename))

   allocate(region_codes(nlon,nlat))
   allocate(lat(nlat))
   allocate(lon(nlon))

   call read2das1dnc(ncid,"lat",(/1,1/),(/nlat,1/),lat)
   call read2das1dnc(ncid,"lon",(/1,1/),(/nlon,1/),lon)
   call read3das2dnc(ncid,"region_codes",(/1,1,1/),(/nlon,nlat,1/),region_codes)

    write(filename1,'("emissions_htap2sectors-gfed3_2007-2011_NO_mol_1.9x2.5.ant.nc")')
     print*,filename1
!
   write(outfile1,'("emiss_htap2_2007-2011_NO_mol_1.9x2.5.NAM.nc")')
   write(outfile2,'("emiss_htap2_2007-2011_NO_mol_1.9x2.5.EUR.nc")')
   write(outfile3,'("emiss_htap2_2007-2011_NO_mol_1.9x2.5.SAS.nc")')
   write(outfile4,'("emiss_htap2_2007-2011_NO_mol_1.9x2.5.EAS.nc")')
   write(outfile5,'("emiss_htap2_2007-2011_NO_mol_1.9x2.5.SEA.nc")')
   write(outfile6,'("emiss_htap2_2007-2011_NO_mol_1.9x2.5.NAF.nc")')
   write(outfile7,'("emiss_htap2_2007-2011_NO_mol_1.9x2.5.MDE.nc")')
   write(outfile8,'("emiss_htap2_2007-2011_NO_mol_1.9x2.5.MCA.nc")')
   write(outfile9,'("emiss_htap2_2007-2011_NO_mol_1.9x2.5.RBU.nc")')
   write(outfile10,'("emiss_htap2_2007-2011_NO_mol_1.9x2.5.OCN.nc")')
   write(outfile11,'("emiss_htap2_2007-2011_NO_mol_1.9x2.5.REST.nc")')
   write(outfile12,'("emiss_htap2_2007-2011_NO_mol_1.9x2.5.CAS.nc")')
     call nf90call(nf90_open(trim(path)//"/"//trim(filename1), &
         NF90_NOWRITE,ncid), &
         "Could not open "//trim(path)//"/"//trim(filename1))
!
! get dimensions of area grid and allocate necessary arrays
  allocate(anthro(nlon,nlat,ntime))
  allocate(time(ntime))
  allocate(date(ntime))
  allocate(NAM(nlon,nlat,ntime))
  allocate(EUR(nlon,nlat,ntime))
  allocate(EAS(nlon,nlat,ntime))
  allocate(SAS(nlon,nlat,ntime))
  allocate(CAS(nlon,nlat,ntime))
  allocate(NAF(nlon,nlat,ntime))
  allocate(SEA(nlon,nlat,ntime))
  allocate(RBU(nlon,nlat,ntime))
  allocate(MDE(nlon,nlat,ntime))
  allocate(MCA(nlon,nlat,ntime))
  allocate(OCN(nlon,nlat,ntime))
  allocate(REST(nlon,nlat,ntime))
  call read2das1dnc(ncid,"time",(/1,1/),(/ntime,1/),time)
  call read2das1dnc_cesm(ncid,"date",(/1,1/),(/ntime,1/),date)
  call read4das3dnc(ncid,"anthro",(/1,1,1,1/),(/nlon,nlat,ntime,1/),anthro)
  call nf90call(nf90_close(ncid))
  
! calculate what????
   j_loop: do j=1,96
    i_loop: do i=1,144
     it_loop: do it=1,60
! do north america
      if (region_codes(i,j) .eq. 3) then 
            NAM(i,j,it)=anthro(i,j,it)
      endif 
! do europe
     if (region_codes(i,j) .eq. 4) then
            EUR(i,j,it)=anthro(i,j,it)
      endif
! do east asia
     if (region_codes(i,j) .eq. 6) then
            EAS(i,j,it)=anthro(i,j,it)
      endif
! do south asia
     if (region_codes(i,j) .eq. 5) then
            SAS(i,j,it)=anthro(i,j,it)
      endif
! do central asia
     if (region_codes(i,j) .eq. 15) then
            CAS(i,j,it)=anthro(i,j,it)
      endif
! do south east asia
     if (region_codes(i,j) .eq. 7) then
            SEA(i,j,it)=anthro(i,j,it)
      endif
! do north africa
     if (region_codes(i,j) .eq. 9) then
            NAF(i,j,it)=anthro(i,j,it)
      endif
! do russia
     if (region_codes(i,j) .eq. 14) then
            RBU(i,j,it)=anthro(i,j,it)
      endif
! do middle east
     if (region_codes(i,j) .eq. 11) then
            MDE(i,j,it)=anthro(i,j,it)
      endif
! do mexico, central america
     if (region_codes(i,j) .eq. 12) then
            MCA(i,j,it)=anthro(i,j,it)
      endif
! do ocean
     if (region_codes(i,j) .eq. 2) then
            OCN(i,j,it)=anthro(i,j,it)
      endif
! do the rest 
            REST(i,j,it)=anthro(i,j,it)-NAM(i,j,it)-MCA(i,j,it)-NAF(i,j,it)-SEA(i,j,it)- &
                          EUR(i,j,it)-EAS(i,j,it)-SAS(i,j,it)- &
                          RBU(i,j,it)-MDE(i,j,it)-OCN(i,j,it)-CAS(i,j,it)
 !     endif
!

     enddo it_loop 
    enddo i_loop 
   enddo j_loop 
  call write_output_netcdf(outfile_method, outfile1, nlon, nlat, ntime,lat,lon,time, date, NAM) 
  call write_output_netcdf_EUR(outfile_method, outfile2,nlon, nlat,ntime,lat,lon,time, date, EUR) 
  call write_output_netcdf_EAS(outfile_method, outfile4,nlon,nlat,ntime, lat,lon,time, date, EAS) 
  call write_output_netcdf_SAS(outfile_method, outfile3,nlon, nlat, ntime,lat,lon,time, date, SAS)
  call write_output_netcdf_SEA(outfile_method, outfile5,nlon, nlat, ntime,lat,lon,time, date, SEA)
  call write_output_netcdf_NAF(outfile_method, outfile6,nlon, nlat, ntime,lat,lon,time, date, NAF)
  call write_output_netcdf_RBU(outfile_method, outfile9,nlon, nlat, ntime,lat,lon,time, date, RBU)
  call write_output_netcdf_MCA(outfile_method, outfile8,nlon, nlat, ntime,lat,lon,time, date, MCA)
  call write_output_netcdf_MDE(outfile_method, outfile7,nlon, nlat, ntime,lat,lon,time, date, MDE)
  call write_output_netcdf_OCN(outfile_method, outfile10,nlon, nlat, ntime,lat,lon,time, date, OCN)
  call write_output_netcdf_RST(outfile_method, outfile11,nlon, nlat, ntime,lat,lon,time, date, REST)
  call write_output_netcdf_CAS(outfile_method, outfile12,nlon, nlat, ntime,lat,lon,time, date, CAS)
  deallocate(lat,lon, anthro, date, time, region_codes, NAM, EUR, EAS, SAS, SEA, NAF, MCA, CAS, RBU, MDE, OCN, REST)
101 continue
77 format(2f12.5)
78 format(2(i4,x),f12.5,e12.5)
 stop 
 end program interpol

!************************************************************************
