 module module_write_output
  implicit none
 contains
  subroutine write_output_netcdf(outfile_method, outfile1,outfile2,outfile3, nlon, nlat, ntime, &
       lat,lon,time, date, anthro_NAM,biogenic_NAM,bb_NAM)
    use netcdf,                        only: NF90_CLOBBER, NF90_CREATE, NF90_GLOBAL,   &
         NF90_PUT_ATT, NF90_UNLIMITED, NF90_DEF_DIM, NF90_FLOAT, NF90_DEF_VAR,         &
         NF90_CHAR, NF90_ENDDEF, NF90_WRITE, NF90_OPEN, NF90_INQ_DIMID,                &
         NF90_INQUIRE_DIMENSION, NF90_INQ_VARID, NF90_GET_ATT, NF90_PUT_VAR, NF90_SYNC,&
         NF90_CLOSE, NF90_INT
    use ncroutines 

    implicit none
    !intent-in
    character(len=*), intent(in) :: outfile_method, outfile1, outfile2, outfile3
    integer,          intent(in) :: nlon, nlat, ntime

    real, dimension(nlon,nlat,ntime), intent(in) :: anthro_NAM,bb_NAM,biogenic_NAM

    real, dimension(:), intent(in) :: lat, lon,time
    integer, dimension(:), intent(in) :: date
    !Local variables
    character(len = 1000) :: times,BTIMESTR, inclusion_method
    integer               :: NCIDOUT, DIMID_T, DIMID_NT, DIMID_NLAT, DIMID_NLON, DIMID_NZ
    integer               :: VARID, i,j,k, tdiff_mm, ft


     print*, 'method create'
! write anthro file
    if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile1,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
         call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
! aura replace NF90_FLOAT with NF90_FLOAT 
          call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
       call nf90call(nf90_def_var(ncidout,"anthro",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Aura"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
 
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    call  write3das4dnc(ncidout,"anthro",anthro_NAM,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    print*, 'anthro was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
! write biomass burning
   if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile2,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"bb",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Aura"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
    !     
    call  write3das4dnc(ncidout,"bb",bb_NAM,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    print*, 'biomass burning was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
!
! write biogenic
   if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile3,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"biogenic",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Aura"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
    !
    call  write3das4dnc(ncidout,"biogenic",biogenic_NAM,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    print*, 'biogenic was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
  end subroutine write_output_netcdf
!---------------------------------------------------------------------------------------
  subroutine write_output_netcdf_EUR(outfile_method, outfile4,outfile5,outfile6, nlon, nlat, ntime, &
       lat,lon,time, date, anthro_EUR,biogenic_EUR,bb_EUR)
    use netcdf,                        only: NF90_CLOBBER, NF90_CREATE, NF90_GLOBAL,   &
         NF90_PUT_ATT, NF90_UNLIMITED, NF90_DEF_DIM, NF90_FLOAT, NF90_DEF_VAR,         &
         NF90_CHAR, NF90_ENDDEF, NF90_WRITE, NF90_OPEN, NF90_INQ_DIMID,                &
         NF90_INQUIRE_DIMENSION, NF90_INQ_VARID, NF90_GET_ATT, NF90_PUT_VAR, NF90_SYNC,&
         NF90_CLOSE, NF90_INT
    use ncroutines 

    implicit none
    !intent-in
    character(len=*), intent(in) :: outfile_method, outfile4, outfile5, outfile6
    integer,          intent(in) :: nlon, nlat, ntime

    real, dimension(nlon,nlat,ntime), intent(in) :: anthro_EUR,bb_EUR,biogenic_EUR

    real, dimension(:), intent(in) :: lat, lon,time
    integer, dimension(:), intent(in) :: date
    !Local variables
    character(len = 1000) :: times,BTIMESTR, inclusion_method
    integer               :: NCIDOUT, DIMID_T, DIMID_NT, DIMID_NLAT, DIMID_NLON, DIMID_NZ
    integer               :: VARID, i,j,k, tdiff_mm, ft

! write anthro file
    if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile4,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
      call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
      call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
      call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))

      call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
      call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
      call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
      call nf90call(nf90_def_var(ncidout,"anthro",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
      call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
      call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGARemissions"))
      call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
              (/dimid_nlon/),varid))
      call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
      call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
      call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Aura"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
 
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    call  write3das4dnc(ncidout,"anthro",anthro_EUR,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    print*, 'anthro was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
! write biomass burning
   if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile5,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"bb",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Aura"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
    !     
    call  write3das4dnc(ncidout,"bb",bb_EUR,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    print*, 'biomass burning was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
!
! write biogenic
   if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile6,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"biogenic",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time")) 
!
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Aura"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
    !
    call  write3das4dnc(ncidout,"biogenic",biogenic_EUR,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    print*, 'biogenic was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
  end subroutine write_output_netcdf_EUR
!---------------------------------------------------------------------------
  subroutine write_output_netcdf_EAS(outfile_method, outfile7,outfile8,outfile9, nlon, nlat, ntime, &
       lat,lon,time, date, anthro_EAS,biogenic_EAS,bb_EAS)
    use netcdf,                        only: NF90_CLOBBER, NF90_CREATE, NF90_GLOBAL,   &
         NF90_PUT_ATT, NF90_UNLIMITED, NF90_DEF_DIM, NF90_FLOAT, NF90_DEF_VAR,         &
         NF90_CHAR, NF90_ENDDEF, NF90_WRITE, NF90_OPEN, NF90_INQ_DIMID,                &
         NF90_INQUIRE_DIMENSION, NF90_INQ_VARID, NF90_GET_ATT, NF90_PUT_VAR, NF90_SYNC,&
         NF90_CLOSE, NF90_INT
    use ncroutines 

    implicit none
    !intent-in
    character(len=*), intent(in) :: outfile_method, outfile7, outfile8, outfile9
    integer,          intent(in) :: nlon, nlat, ntime

    real, dimension(nlon,nlat,ntime), intent(in) :: anthro_EAS,bb_EAS,biogenic_EAS

    real, dimension(:), intent(in) :: lat, lon,time
    integer, dimension(:), intent(in) :: date
    !Local variables
    character(len = 1000) :: times,BTIMESTR, inclusion_method
    integer               :: NCIDOUT, DIMID_T, DIMID_NT, DIMID_NLAT, DIMID_NLON, DIMID_NZ
    integer               :: VARID, i,j,k, tdiff_mm, ft

     print*, 'method create'
! write anthro file
    if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile7,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
       call nf90call(nf90_def_var(ncidout,"anthro",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Aura"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
 
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    call  write3das4dnc(ncidout,"anthro",anthro_EAS,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    print*, 'anthro was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
! write biomass burning
   if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile8,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"bb",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Aura"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
    !     
    call  write3das4dnc(ncidout,"bb",bb_EAS,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    print*, 'biomass burning was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
!
! write biogenic
   if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile9,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"biogenic",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))    
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Aura"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
    !
    call  write3das4dnc(ncidout,"biogenic",biogenic_EAS,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    print*, 'biogenic was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
  end subroutine write_output_netcdf_EAS
!-------------------------------------------------------------------------------------------------------------
  subroutine write_output_netcdf_SAS(outfile_method, outfile10,outfile11,outfile12, nlon, nlat, ntime, &
       lat,lon,time, date, anthro_SAS,biogenic_SAS,bb_SAS)
    use netcdf,                        only: NF90_CLOBBER, NF90_CREATE, NF90_GLOBAL,   &
         NF90_PUT_ATT, NF90_UNLIMITED, NF90_DEF_DIM, NF90_FLOAT, NF90_DEF_VAR,         &
         NF90_CHAR, NF90_ENDDEF, NF90_WRITE, NF90_OPEN, NF90_INQ_DIMID,                &
         NF90_INQUIRE_DIMENSION, NF90_INQ_VARID, NF90_GET_ATT, NF90_PUT_VAR, NF90_SYNC,&
         NF90_CLOSE, NF90_INT
    use ncroutines 

    implicit none
    !intent-in
    character(len=*), intent(in) :: outfile_method, outfile10, outfile11, outfile12
    integer,          intent(in) :: nlon, nlat, ntime

    real, dimension(nlon,nlat,ntime), intent(in) :: anthro_SAS,bb_SAS,biogenic_SAS

    real, dimension(:), intent(in) :: lat, lon,time
    integer, dimension(:), intent(in) :: date
    !Local variables
    character(len = 1000) :: times,BTIMESTR, inclusion_method
    integer               :: NCIDOUT, DIMID_T, DIMID_NT, DIMID_NLAT, DIMID_NLON, DIMID_NZ
    integer               :: VARID, i,j,k, tdiff_mm, ft

     print*, 'method create'
! write anthro file
    if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile10,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
       call nf90call(nf90_def_var(ncidout,"anthro",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Aura"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
 
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    call  write3das4dnc(ncidout,"anthro",anthro_SAS,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    print*, 'anthro was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
! write biomass burning
   if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile11,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"bb",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Aura"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
    !     
    call  write3das4dnc(ncidout,"bb",bb_SAS,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    print*, 'biomass burning was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
!
! write biogenic
   if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile12,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"biogenic",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Aura"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
    !
    call  write3das4dnc(ncidout,"biogenic",biogenic_SAS,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    print*, 'biogenic was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
  end subroutine write_output_netcdf_SAS
!-------------------------------------------------------------------
  subroutine write_output_netcdf_RBU(outfile_method, outfile13,outfile14,outfile15, nlon, nlat, ntime, &
       lat,lon,time, date, anthro_RBU,biogenic_RBU,bb_RBU)
    use netcdf,                        only: NF90_CLOBBER, NF90_CREATE, NF90_GLOBAL,   &
         NF90_PUT_ATT, NF90_UNLIMITED, NF90_DEF_DIM, NF90_FLOAT, NF90_DEF_VAR,         &
         NF90_CHAR, NF90_ENDDEF, NF90_WRITE, NF90_OPEN, NF90_INQ_DIMID,                &
         NF90_INQUIRE_DIMENSION, NF90_INQ_VARID, NF90_GET_ATT, NF90_PUT_VAR, NF90_SYNC,&
         NF90_CLOSE, NF90_INT
    use ncroutines 

    implicit none
    !intent-in
    character(len=*), intent(in) :: outfile_method, outfile13, outfile14, outfile15
    integer,          intent(in) :: nlon, nlat, ntime

    real, dimension(nlon,nlat,ntime), intent(in) :: anthro_RBU,bb_RBU,biogenic_RBU

    real, dimension(:), intent(in) :: lat, lon,time
    integer, dimension(:), intent(in) :: date
    !Local variables
    character(len = 1000) :: times,BTIMESTR, inclusion_method
    integer               :: NCIDOUT, DIMID_T, DIMID_NT, DIMID_NLAT, DIMID_NLON, DIMID_NZ
    integer               :: VARID, i,j,k, tdiff_mm, ft

     print*, 'method create'
! write anthro file
    if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile13,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
       call nf90call(nf90_def_var(ncidout,"anthro",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Aura"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
 
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    call  write3das4dnc(ncidout,"anthro",anthro_RBU,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    print*, 'anthro was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
! write biomass burning
   if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile14,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"bb",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Aura"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
    !     
    call  write3das4dnc(ncidout,"bb",bb_RBU,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    print*, 'biomass burning was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
!
! write biogenic
   if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile15,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"biogenic",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time")) 
!
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Aura"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
    !
    call  write3das4dnc(ncidout,"biogenic",biogenic_RBU,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    print*, 'biogenic was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
  end subroutine write_output_netcdf_RBU
!------------------------------------------------------------------------------------------------
  subroutine write_output_netcdf_MDE(outfile_method, outfile16,outfile17,outfile18, nlon, nlat, ntime, &
       lat,lon,time, date, anthro_MDE,biogenic_MDE,bb_MDE)
    use netcdf,                        only: NF90_CLOBBER, NF90_CREATE, NF90_GLOBAL,   &
         NF90_PUT_ATT, NF90_UNLIMITED, NF90_DEF_DIM, NF90_FLOAT, NF90_DEF_VAR,         &
         NF90_CHAR, NF90_ENDDEF, NF90_WRITE, NF90_OPEN, NF90_INQ_DIMID,                &
         NF90_INQUIRE_DIMENSION, NF90_INQ_VARID, NF90_GET_ATT, NF90_PUT_VAR, NF90_SYNC,&
         NF90_CLOSE, NF90_INT
    use ncroutines 

    implicit none
    !intent-in
    character(len=*), intent(in) :: outfile_method, outfile16, outfile17, outfile18
    integer,          intent(in) :: nlon, nlat, ntime

    real, dimension(nlon,nlat,ntime), intent(in) :: anthro_MDE,bb_MDE,biogenic_MDE

    real, dimension(:), intent(in) :: lat, lon,time
    integer, dimension(:), intent(in) :: date
    !Local variables
    character(len = 1000) :: times,BTIMESTR, inclusion_method
    integer               :: NCIDOUT, DIMID_T, DIMID_NT, DIMID_NLAT, DIMID_NLON, DIMID_NZ
    integer               :: VARID, i,j,k, tdiff_mm, ft

     print*, 'method create'
! write anthro file
    if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile16,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
       call nf90call(nf90_def_var(ncidout,"anthro",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Aura"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    call  write3das4dnc(ncidout,"anthro",anthro_MDE,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    print*, 'anthro was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
! write biomass burning
   if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile17,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"bb",NF90_FLOAT, &
             (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Aura"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
    !     
    call  write3das4dnc(ncidout,"bb",bb_MDE,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    print*, 'biomass burning was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
!
! write biogenic
   if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile18,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"biogenic",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Aura"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
    !
    call  write3das4dnc(ncidout,"biogenic",biogenic_MDE,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    print*, 'biogenic was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
  end subroutine write_output_netcdf_MDE
!----------------------------------------------------------------------------------
  subroutine write_output_netcdf_OCN(outfile_method, outfile19,outfile20,outfile21, nlon, nlat, ntime, &
       lat,lon,time, date, anthro_OCN,biogenic_OCN,bb_OCN)
    use netcdf,                        only: NF90_CLOBBER, NF90_CREATE, NF90_GLOBAL,   &
         NF90_PUT_ATT, NF90_UNLIMITED, NF90_DEF_DIM, NF90_FLOAT, NF90_DEF_VAR,         &
         NF90_CHAR, NF90_ENDDEF, NF90_WRITE, NF90_OPEN, NF90_INQ_DIMID,                &
         NF90_INQUIRE_DIMENSION, NF90_INQ_VARID, NF90_GET_ATT, NF90_PUT_VAR, NF90_SYNC,&
         NF90_CLOSE, NF90_INT
    use ncroutines 

    implicit none
    !intent-in
    character(len=*), intent(in) :: outfile_method, outfile19, outfile20, outfile21
    integer,          intent(in) :: nlon, nlat, ntime

    real, dimension(nlon,nlat,ntime), intent(in) :: anthro_OCN,bb_OCN,biogenic_OCN

    real, dimension(:), intent(in) :: lat, lon,time
    integer, dimension(:), intent(in) :: date
    !Local variables
    character(len = 1000) :: times,BTIMESTR, inclusion_method
    integer               :: NCIDOUT, DIMID_T, DIMID_NT, DIMID_NLAT, DIMID_NLON, DIMID_NZ
    integer               :: VARID, i,j,k, tdiff_mm, ft

     print*, 'method create'
! write anthro file
    if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile19,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
       call nf90call(nf90_def_var(ncidout,"anthro",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Aura"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
 
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    call  write3das4dnc(ncidout,"anthro",anthro_OCN,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    print*, 'anthro was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
! write biomass burning
   if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile20,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"bb",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Aura"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
    !     
    call  write3das4dnc(ncidout,"bb",bb_OCN,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    print*, 'biomass burning was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
!
! write biogenic
   if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile21,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"biogenic",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Aura"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
    !
    call  write3das4dnc(ncidout,"biogenic",biogenic_OCN,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    print*, 'biogenic was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
  end subroutine write_output_netcdf_OCN
!-------------------------------------------------------------------------------------
  subroutine write_output_netcdf_RST(outfile_method, outfile22,outfile23,outfile24, nlon, nlat, ntime, &
       lat,lon,time, date, anthro_RST,biogenic_RST,bb_RST)
    use netcdf,                        only: NF90_CLOBBER, NF90_CREATE, NF90_GLOBAL,   &
         NF90_PUT_ATT, NF90_UNLIMITED, NF90_DEF_DIM, NF90_FLOAT, NF90_DEF_VAR,         &
         NF90_CHAR, NF90_ENDDEF, NF90_WRITE, NF90_OPEN, NF90_INQ_DIMID,                &
         NF90_INQUIRE_DIMENSION, NF90_INQ_VARID, NF90_GET_ATT, NF90_PUT_VAR, NF90_SYNC,&
         NF90_CLOSE, NF90_INT
    use ncroutines 

    implicit none
    !intent-in
    character(len=*), intent(in) :: outfile_method, outfile22, outfile23, outfile24
    integer,          intent(in) :: nlon, nlat, ntime

    real, dimension(nlon,nlat,ntime), intent(in) :: anthro_RST,bb_RST,biogenic_RST

    real, dimension(:), intent(in) :: lat, lon,time
    integer, dimension(:), intent(in) :: date
    !Local variables
    character(len = 1000) :: times,BTIMESTR, inclusion_method
    integer               :: NCIDOUT, DIMID_T, DIMID_NT, DIMID_NLAT, DIMID_NLON, DIMID_NZ
    integer               :: VARID, i,j,k, tdiff_mm, ft

     print*, 'method create'
! write anthro file
    if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile22,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
       call nf90call(nf90_def_var(ncidout,"anthro",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Aura"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    call  write3das4dnc(ncidout,"anthro",anthro_RST,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    print*, 'anthro was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
! write biomass burning
   if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile23,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"bb",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Aura"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
    !     
    call  write3das4dnc(ncidout,"bb",bb_RST,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    print*, 'biomass burning was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
!
! write biogenic
   if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile24,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"biogenic",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Aura"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
    !
    call  write3das4dnc(ncidout,"biogenic",biogenic_RST,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    print*, 'biogenic was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
  end subroutine write_output_netcdf_RST
! all
!-------------------------------------------------------------------------------------
  subroutine write_output_netcdf_all(outfile_method, outfile25, nlon, nlat, ntime, &
      lat,lon,time, date, anthro,biogenic,bb,ocean)
    use netcdf,                        only: NF90_CLOBBER, NF90_CREATE, NF90_GLOBAL,   &
         NF90_PUT_ATT, NF90_UNLIMITED, NF90_DEF_DIM, NF90_FLOAT, NF90_DEF_VAR,         &
         NF90_CHAR, NF90_ENDDEF, NF90_WRITE, NF90_OPEN, NF90_INQ_DIMID,                &
         NF90_INQUIRE_DIMENSION, NF90_INQ_VARID, NF90_GET_ATT, NF90_PUT_VAR, NF90_SYNC,&
         NF90_CLOSE, NF90_INT
    use ncroutines

    implicit none
    !intent-in
    character(len=*), intent(in) :: outfile_method, outfile25
    integer,          intent(in) :: nlon, nlat, ntime

    real, dimension(nlon,nlat,ntime), intent(in) :: anthro,bb,biogenic,ocean

    real, dimension(:), intent(in) :: lat, lon,time
    integer, dimension(:), intent(in) :: date
    !Local variables
    character(len = 1000) :: times,BTIMESTR, inclusion_method
    integer               :: NCIDOUT, DIMID_T, DIMID_NT, DIMID_NLAT, DIMID_NLON, DIMID_NZ
    integer               :: VARID, i,j,k, tdiff_mm, ft

     print*, 'method create'
! write anthro file
    if(outfile_method=="create" ) then  !Only define file on the first timestep
       call nf90call(nf90_create(outfile25,NF90_CLOBBER,ncidout))
       ! The global attributes...
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Latitude",nlon))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"Longitude",nlat))
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"time",ntime))
       ! The dimensions...
       call nf90call(nf90_def_dim(ncidout,"time",ntime,dimid_t))
       call nf90call(nf90_def_dim(ncidout,"lon",nlon,dimid_nlon))
       call nf90call(nf90_def_dim(ncidout,"lat",nlat,dimid_nlat))
       ! The data variables...
       call nf90call(nf90_def_var(ncidout,"date",NF90_INT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","YYYYMMDD"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","date"))
       call nf90call(nf90_def_var(ncidout,"time",NF90_FLOAT, &
               (/dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","days since 1990-01-0100:00:00"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","time"))

       call nf90call(nf90_def_var(ncidout,"anthro",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
!
       call nf90call(nf90_def_var(ncidout,"biogenic",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
!
       call nf90call(nf90_def_var(ncidout,"bb",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
!
       call nf90call(nf90_def_var(ncidout,"ocean",NF90_FLOAT, &
            (/dimid_nlon,dimid_nlat,dimid_t/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","molecules/cm2/s"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","HTAP2 EDGAR emissions"))
!
       call nf90call(nf90_def_var(ncidout,"lon",NF90_FLOAT, &
               (/dimid_nlon/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_east"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Longitude"))
       call nf90call(nf90_def_var(ncidout,"lat",NF90_FLOAT, &
               (/dimid_nlat/),varid))
       call nf90call(nf90_put_att(ncidout,varid,"units","degrees_north"))
       call nf90call(nf90_put_att(ncidout,varid,"long_name","Latitude"))
 !
       call nf90call(nf90_put_att(ncidout,NF90_GLOBAL,"TITLE","Generated offline by Aura"))
       call nf90call(nf90_enddef(ncidout))
    end if !End of outfile setup
    call  write1das2dnc_cesm(ncidout,"date",date,(/1,1/),(/ntime,1/))
    call  write1das2dnc(ncidout,"time",time,(/1,1/),(/ntime,1/))
    call  write3das4dnc(ncidout,"anthro",anthro,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write3das4dnc(ncidout,"biogenic",biogenic,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write3das4dnc(ncidout,"bb",bb,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write3das4dnc(ncidout,"ocean",ocean,(/1,1,1,1/),(/nlon,nlat,ntime,1/))
    call  write1das2dnc(ncidout,"lat",lat,(/1,1/),(/nlat,1/))
    call  write1das2dnc(ncidout,"lon",lon,(/1,1/),(/nlon,1/))
    print*, 'anthro was written'
    call nf90call(nf90_sync(ncidout))
    call nf90call(nf90_close(ncidout))
    !
  end subroutine write_output_netcdf_all
!----------------------------------------------

!-------------------------------------------------------------------------------------------------------
 end module 
