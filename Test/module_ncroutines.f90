!tt----------------------------------------------------------------------
! A suite of netCDF routines to read and write data arrays.
!
! Routines in this module:
!   nf90call
!   get_ncatt_real
!   get_ncatt_str
!   get_ncdim_len
!   pack2d
!   read1dnc
!   read1dasscalarnc
!   read1dasscalarnc_r8
!   read2dnc
!   read2dasscalarnc
!   read2das1dnc
!   read3dnc
!   read3das2dnc
!   read4dnc
!   read4das1dnc
!   read4das2dnc
!   read4das3dnc
!   read4das3dnc_ijkt2ikjt
!   unpack1d
!   write1dnc
!   write2dnc
!   write3dnc
!   write4dnc
!   write1das2dnc
!   write1das3dnc
!   write2das3dnc
!   write1das4dnc
!   write2das4dnc
!   write3das4dnc
!   writescalarnc
!   writescalaras1dnc
!
!----------------------------------------------------------------------
module ncroutines
  use netcdf
  implicit none

contains

!----------------------------------------------------------------------
subroutine nf90call(ierr,msg,ncid)
! This subroutine checks if a netCDF error occured
! and quits the program if it did.
!
! ierr: Return value from a netCDF function call
! msg:  A message to print if an error occurs
!----------------------------------------------------------------------
  integer,intent(in)                   :: ierr
  character(len=*),intent(in),optional :: msg
  integer,intent(in),optional          :: ncid

  integer :: i

  if( ierr.ne.NF90_NOERR ) then
     if( present(ncid) ) then
        i = nf90_sync(ncid)
     end if
     print*,nf90_strerror(ierr)
     if( present(msg) ) print*,msg
     stop 'STOP: Stopped due to error associated with netCDF.'
  endif
  return
end subroutine nf90call

!----------------------------------------------------------------------
subroutine get_ncatt_real(ncid,vname,aname,val)
! Returns the attribute named in aname that is associated with the
! variable named in vname for the open netCDF file.
!----------------------------------------------------------------------
  integer,          intent(in)  :: ncid
  character(len=*), intent(in)  :: vname,aname
  real,             intent(out) :: val

  integer :: varid

  call nf90call( nf90_inq_varid(ncid,vname,varid), &
       "Could not find variable "//trim(vname)// &
       " from within get_ncatt_real." )
  call nf90call( nf90_get_att(ncid,varid,aname,val), &
       "Could not find attribute "//trim(aname)// &
       " from within get_ncat_real." )
end subroutine get_ncatt_real

!----------------------------------------------------------------------
subroutine get_ncatt_str(ncid,vname,aname,val)
! Returns the attribute named in aname that is associated with the
! variable named in vname for the open netCDF file. aname must be a
! character string.
!----------------------------------------------------------------------
  integer,          intent(in)  :: ncid
  character(len=*), intent(in)  :: vname,aname
  character(len=*), intent(out) :: val

  integer :: varid

  call nf90call( nf90_inq_varid(ncid,vname,varid), &
       "Could not find variable "//trim(vname)// &
       " from within get_ncatt_str." )
  call nf90call( nf90_get_att(ncid,varid,aname,val), &
       "Could not find attribute "//trim(aname)// &
       " from within get_ncat_str." )
end subroutine get_ncatt_str

!----------------------------------------------------------------------
subroutine get_ncdim_len(ncid,name,len)
! Returns the length of the named dimension in the open netCDF file.
!----------------------------------------------------------------------
  integer,          intent(in)  :: ncid
  character(len=*), intent(in)  :: name
  integer,          intent(out) :: len

  integer :: dimid

  call nf90call( nf90_inq_dimid(ncid,name,dimid), &
       "Could not inquire dimension name "//trim(name) )
  call nf90call( nf90_inquire_dimension(ncid,dimid,len=len) )
end subroutine get_ncdim_len

!----------------------------------------------------------------------
subroutine pack1d(data,add_offset,scale_factor,badreal)
! Applies the add_ofset and scale_factor to the data array.
!
! For the optimum choices of add_offset and scale_factor, to preserve
! the most information in the packed data, use:
!   add_offset   = 0.5*(min+max)
!   scale_factor = (max-min)/ndrv
! where
!   ndrv = (2^16)-1 for NC_SHORT
!       or (2^32)-1 for NC_INT (NF90_INT arrays appear to be the same
!                               size as NF90_FLOAT)
! These formulas are from the NCO documention on pack().
!
!----------------------------------------------------------------------
  real,dimension(:), intent(inout) :: data
  real,              intent(in)    :: add_offset, scale_factor, &
                                      badreal

  where( data /= badreal )
     data = (data - add_offset)/scale_factor
  end where
end subroutine pack1d

!----------------------------------------------------------------------
subroutine pack2d(data,add_offset,scale_factor,badreal)
! Applies the add_ofset and scale_factor to the data array.
!
! For the optimum choices of add_offset and scale_factor, to preserve
! the most information in the packed data, use:
!   add_offset   = 0.5*(min+max)
!   scale_factor = (max-min)/ndrv
! where
!   ndrv = (2^16)-1 for NC_SHORT
!       or (2^32)-1 for NC_INT (NF90_INT arrays appear to be the same
!                               size as NF90_FLOAT)
! These formulas are from the NCO documention on pack().
!
!----------------------------------------------------------------------
  real,dimension(:,:), intent(inout) :: data
  real,                intent(in)    :: add_offset, scale_factor, &
                                        badreal

  where( data /= badreal )
     data = (data - add_offset)/scale_factor
  end where
end subroutine pack2d

!----------------------------------------------------------------------
subroutine read1dnc(ncid,varname,start,count,data)
! Reads the contents of a 1D variable data into data for the
! dimensions defined by start and count.
!----------------------------------------------------------------------
  integer,             intent(in)  :: ncid
  character(len=*),    intent(in)  :: varname
  integer,             intent(in)  :: start,count
  real,dimension(:),   intent(out) :: data

  integer :: varid

  call nf90call(nf90_inq_varid(ncid,varname,varid),varname)
  call nf90call(nf90_get_var(ncid,varid,data,(/start/),(/count/)),varname)
end subroutine read1dnc

!----------------------------------------------------------------------
subroutine read1dasscalarnc(ncid,varname,start,data)
! Reads one element of a 1D variable data into data from the index
! defined by start.
!----------------------------------------------------------------------
  integer,             intent(in)  :: ncid
  character(len=*),    intent(in)  :: varname
  integer,             intent(in)  :: start
  real(kind=8),        intent(out) :: data

  integer :: varid
  real,dimension(1) :: arr

  call nf90call(nf90_inq_varid(ncid,varname,varid),varname)
  call nf90call(nf90_get_var(ncid,varid,arr,(/start/),(/1/)),varname)
  data = arr(1)
end subroutine read1dasscalarnc !_r8

!----------------------------------------------------------------------
subroutine read2dnc(ncid,varname,start,count,data)
! Reads the contents of a 2D variable data into data for the
! dimensions defined by start and count.
!----------------------------------------------------------------------
  integer,              intent(in)  :: ncid
  character(len=*),     intent(in)  :: varname
  integer,dimension(2), intent(in)  :: start,count
  real,dimension(:,:),  intent(out) :: data

  integer :: varid

  call nf90call(nf90_inq_varid(ncid,varname,varid),varname)
  call nf90call(nf90_get_var(ncid,varid,data,(/start/),(/count/)),varname)
end subroutine read2dnc

!----------------------------------------------------------------------
subroutine read2dasscalarnc(ncid,varname,start,data)
! Reads one element of a 2D variable data into data from the index
! defined by start.
!----------------------------------------------------------------------
  integer,              intent(in)  :: ncid
  character(len=*),     intent(in)  :: varname
  integer,dimension(2), intent(in)  :: start
  real,                 intent(out) :: data

  integer :: varid
  real,dimension(1) :: arr

  call nf90call(nf90_inq_varid(ncid,varname,varid),varname)
  call nf90call(nf90_get_var(ncid,varid,arr,start,(/1,1/)),varname)
  data = arr(1)
end subroutine read2dasscalarnc

!----------------------------------------------------------------------
subroutine read2das1dnc(ncid,varname,start,count,data)
! Reads the contents of a 2D variable data into a 1D data for the
! dimensions defined by 2D start and count. If the data is packed,
! it is automatically unpacked.
!----------------------------------------------------------------------
  integer,                 intent(in)  :: ncid
  character(len=*),        intent(in)  :: varname
  integer,dimension(2),    intent(in)  :: start,count
  real,dimension(:),       intent(out) :: data

  integer :: ierro, ierrs, varid
  real :: addoff, missing, scale

  call nf90call(nf90_inq_varid(ncid,varname,varid),varname)
  call nf90call(nf90_get_var(ncid,varid,data,start,count),varname)

  ierro = nf90_get_att(ncid,varid,"add_offset",addoff)
  if( ierro /= NF90_NOERR ) addoff = 0.
  ierrs = nf90_get_att(ncid,varid,"scale_factor",scale)
  if( ierrs /= NF90_NOERR ) scale = 1.
  if( ierro == NF90_NOERR .or. ierrs == NF90_NOERR ) then
     ierro = nf90_get_att(ncid,varid,"missing_value",missing)
     if( ierro == NF90_NOERR ) then
        where( data /= missing )
           data = data*scale + addoff
        end where
     else
        data = data*scale + addoff
     end if
  end if
end subroutine read2das1dnc
!----------------------------------------------------------------------
!aura for cesm
subroutine read2das1dnc_cesm(ncid,varname,start,count,data)
! Reads the contents of a 2D variable data into a 1D data for the
! dimensions defined by 2D start and count. If the data is packed,
! it is automatically unpacked.
!----------------------------------------------------------------------
  integer,                 intent(in)  :: ncid
  character(len=*),        intent(in)  :: varname
  integer,dimension(2),    intent(in)  :: start,count
  integer,dimension(:),       intent(out) :: data

  integer :: ierro, ierrs, varid
  real :: addoff, missing, scale

  call nf90call(nf90_inq_varid(ncid,varname,varid),varname)
  call nf90call(nf90_get_var(ncid,varid,data,start,count),varname)

  ierro = nf90_get_att(ncid,varid,"add_offset",addoff)
  if( ierro /= NF90_NOERR ) addoff = 0.
  ierrs = nf90_get_att(ncid,varid,"scale_factor",scale)
  if( ierrs /= NF90_NOERR ) scale = 1.
  if( ierro == NF90_NOERR .or. ierrs == NF90_NOERR ) then
     ierro = nf90_get_att(ncid,varid,"missing_value",missing)
     if( ierro == NF90_NOERR ) then
        where( data /= missing )
           data = data*scale + addoff
        end where
     else
        data = data*scale + addoff
     end if
  end if
end subroutine read2das1dnc_cesm
!----------------------------------------------------------------------
subroutine read3dnc(ncid,varname,start,count,data)
! Reads the contents of a 3D variable data into data for the
! dimensions defined by start and count.
!----------------------------------------------------------------------
  integer,               intent(in)  :: ncid
  character(len=*),      intent(in)  :: varname
  integer,dimension(3),  intent(in)  :: start,count
  real,dimension(:,:,:), intent(out) :: data

  integer :: varid

  call nf90call(nf90_inq_varid(ncid,varname,varid),varname)
!   print*,'varname',varname
  call nf90call(nf90_get_var(ncid,varid,data,start,count),varname)
   print*,'ma-ta',data !(:,:,:)
end subroutine read3dnc

!----------------------------------------------------------------------
subroutine read3dasscalarnc(ncid,varname,start,data)
! Reads one element of a 2D variable data into data from the index
! defined by start.
!----------------------------------------------------------------------
  integer,              intent(in)  :: ncid
  character(len=*),     intent(in)  :: varname
  integer,dimension(3), intent(in)  :: start
  real,                 intent(out) :: data

  integer :: varid
  real,dimension(1) :: arr

  call nf90call(nf90_inq_varid(ncid,varname,varid),varname)
  call nf90call(nf90_get_var(ncid,varid,arr,start,(/1,1,1/)),varname)
  data = arr(1)
end subroutine read3dasscalarnc

!----------------------------------------------------------------------
subroutine read3das2dnc(ncid,varname,start,count,data)
! Reads the contents of a 3D variable data into a 2D data for the
! dimensions defined by 3D start and count. If the data is packed,
! it is automatically unpacked.
!----------------------------------------------------------------------
  integer,                 intent(in)  :: ncid
  character(len=*),        intent(in)  :: varname
  integer,dimension(3),    intent(in)  :: start,count
  real,dimension(:,:),     intent(out) :: data

  integer :: ierro, ierrs, varid
  real :: addoff, missing, scale

  call nf90call(nf90_inq_varid(ncid,varname,varid),varname)
  call nf90call(nf90_get_var(ncid,varid,data,start,count),varname)

  ierro = nf90_get_att(ncid,varid,"add_offset",addoff)
  if( ierro /= NF90_NOERR ) addoff = 0.
  ierrs = nf90_get_att(ncid,varid,"scale_factor",scale)
  if( ierrs /= NF90_NOERR ) scale = 1.
  if( ierro == NF90_NOERR .or. ierrs == NF90_NOERR ) then
     ierro = nf90_get_att(ncid,varid,"missing_value",missing)
     if( ierro == NF90_NOERR ) then
        where( data /= missing )
           data = data*scale + addoff
        end where
     else
        data = data*scale + addoff
     end if
  end if
end subroutine read3das2dnc

!----------------------------------------------------------------------
subroutine read4dnc(ncid,varname,start,count,data)
! Reads the contents of a 4D variable data into data for the
! dimensions defined by start and count.
!----------------------------------------------------------------------
  integer,                 intent(in)  :: ncid
  character(len=*),        intent(in)  :: varname
  integer,dimension(4),    intent(in)  :: start,count
  real,dimension(:,:,:,:), intent(out) :: data

  integer :: varid

  call nf90call(nf90_inq_varid(ncid,varname,varid),varname)
  call nf90call(nf90_get_var(ncid,varid,data,start,count),varname)
end subroutine read4dnc

!----------------------------------------------------------------------
subroutine read4das2dnc(ncid,varname,start,count,data)
! Reads the contents of a 4D variable data into a 2D data for the
! dimensions defined by 4D start and count.
!----------------------------------------------------------------------
  integer,                 intent(in)  :: ncid
  character(len=*),        intent(in)  :: varname
  integer,dimension(4),    intent(in)  :: start,count
  real,dimension(:,:),     intent(out) :: data

  integer :: varid

  call nf90call(nf90_inq_varid(ncid,varname,varid),varname)
  call nf90call(nf90_get_var(ncid,varid,data,start,count),varname)
end subroutine read4das2dnc

!----------------------------------------------------------------------
subroutine read4das1dnc(ncid,varname,start,count,data)
! Reads the contents of a 4D variable data into a 1D data for the
! dimensions defined by 4D start and count.
!----------------------------------------------------------------------
  integer,                 intent(in)  :: ncid
  character(len=*),        intent(in)  :: varname
  integer,dimension(4),    intent(in)  :: start,count
  real,dimension(:),       intent(out) :: data

  integer :: varid

  call nf90call(nf90_inq_varid(ncid,varname,varid),varname)
  call nf90call(nf90_get_var(ncid,varid,data,start,count),varname)
end subroutine read4das1dnc

!----------------------------------------------------------------------
subroutine read4das3dnc(ncid,varname,start,count,data)
! Reads the contents of a 4D variable data into a 3D data for the
! dimensions defined by 4D start and count.
!----------------------------------------------------------------------
  integer,                 intent(in)  :: ncid
  character(len=*),        intent(in)  :: varname
  integer,dimension(4),    intent(in)  :: start,count
  real,dimension(:,:,:),   intent(out) :: data

  integer :: varid

  call nf90call(nf90_inq_varid(ncid,varname,varid),varname)
  call nf90call(nf90_get_var(ncid,varid,data,start,count),varname)
end subroutine read4das3dnc

!----------------------------------------------------------------------
subroutine read4das3dnc_ijkt2ikjt(ncid,varname, &
                                  i1,j1,k1,t1,  &
                                  ni,nj,nk,nt,dataout)
! Reads the contents of a 4D variable data into a 3D dataout for the
! dimensions defined by 4D start and count. Start is defined from the
! X1 arguments and count is defined using the nX arguments. The input
! array from the file has the dimensions ordered as ijkt (WRF output
! order) and the returned array has the dimensions ordered as ikj (WRF
! internal order).
!
!----------------------------------------------------------------------
  integer,                  intent(in)  :: ncid,           &
                                           i1, j1, k1, t1, &
                                           ni, nj, nk, nt
  character(len=*),         intent(in)  :: varname
  real,dimension(ni,nk,nj), intent(out) :: dataout

  real,dimension(ni,nj,nk) :: datain
  integer,dimension(4) :: start, count
  integer :: j, k, m, varid

  !Here, we assume that there is only one time being read to simplify
  !things. We could check to see which dimension is singular if we
  !ever need something more complicated and want to spend the time.
  if( nt > 1 ) then
     print*,"ERROR: read4das3dnc_ijkt2ikjt currently requires nt=1."
     stop
  end if

  start = (/i1,j1,k1,t1/)
  count = (/ni,nj,nk,nt/)
  call nf90call(nf90_inq_varid(ncid,varname,varid),varname)
  call nf90call(nf90_get_var(ncid,varid,datain,start,count),varname)

  do k = 1,nk
     do j = 1,nj
        dataout(:,k,j) = datain(:,j,k)
     end do
  end do
end subroutine read4das3dnc_ijkt2ikjt

!----------------------------------------------------------------------
subroutine unpack1d(data,add_offset,scale_factor,badreal)
! Reconstructs the unpacked data values for the given add_ofset and
! scale_factor.
!
!----------------------------------------------------------------------
  real,dimension(:), intent(inout) :: data
  real,              intent(in)    :: add_offset, scale_factor, &
                                      badreal

  where( data /= badreal )
     data = data*scale_factor + add_offset
  end where
end subroutine unpack1d

!----------------------------------------------------------------------
subroutine write1dnc(ncid,varname,data,start,count)
! Writes the contents of a 1D variable data to varname for the
! dimensions defined by start and count.
!----------------------------------------------------------------------
  integer,             intent(in) :: ncid
  character(len=*),    intent(in) :: varname
  real,dimension(:),   intent(in) :: data
  integer,             intent(in) :: start,count

  integer :: varid

  call nf90call(nf90_inq_varid(ncid,varname,varid),varname)
  call nf90call(nf90_put_var(ncid,varid,data,(/start/),(/count/)),varname)
end subroutine write1dnc

!----------------------------------------------------------------------
subroutine write2dnc(ncid,varname,data,start,count)
! Writes the contents of a 2D variable data to varname for the
! dimensions defined by start and count.
!----------------------------------------------------------------------
  integer,             intent(in) :: ncid
  character(len=*),    intent(in) :: varname
  real,dimension(:,:), intent(in) :: data
  integer,dimension(2),intent(in) :: start,count

  integer :: varid

  call nf90call(nf90_inq_varid(ncid,varname,varid),varname)
  call nf90call(nf90_put_var(ncid,varid,data,start,count),varname)
end subroutine write2dnc

!----------------------------------------------------------------------
subroutine write3dnc(ncid,varname,data,start,count)
! Writes the contents of a 3D variable data to varname for the
! dimensions defined by start and count.
!----------------------------------------------------------------------
  integer,               intent(in) :: ncid
  character(len=*),      intent(in) :: varname
  real,dimension(:,:,:), intent(in) :: data
  integer,dimension(3),  intent(in) :: start,count

  integer :: varid

  call nf90call(nf90_inq_varid(ncid,varname,varid),varname)
  call nf90call(nf90_put_var(ncid,varid,data,start,count),varname)
end subroutine write3dnc

!----------------------------------------------------------------------
subroutine write4dnc(ncid,varname,data,start,count)
! Writes the contents of a 4D variable data to varname for the
! dimensions defined by start and count.
!----------------------------------------------------------------------
  integer,                 intent(in) :: ncid
  character(len=*),        intent(in) :: varname
  real,dimension(:,:,:,:), intent(in) :: data
  integer,dimension(4),    intent(in) :: start,count

  integer :: varid

  call nf90call(nf90_inq_varid(ncid,varname,varid),varname)
  call nf90call(nf90_put_var(ncid,varid,data,start,count),varname)
end subroutine write4dnc

!----------------------------------------------------------------------
subroutine write1das2dnc(ncid,varname,data,start,count,ierr)
! Writes the contents of a 1D variable data to varname for the
! dimensions defined by 2D start and count.
!
! If ierr is present and the put fails, the error value is returned
! instead of automatically stopping.
!
!----------------------------------------------------------------------
  integer,             intent(in)  :: ncid
  character(len=*),    intent(in)  :: varname
  real,dimension(:),   intent(in)  :: data
  integer,dimension(2),intent(in)  :: start,count
  integer,optional,    intent(out) :: ierr

  integer :: ier,varid

  call nf90call(nf90_inq_varid(ncid,varname,varid),varname)
!  call nf90call(nf90_put_var(ncid,varid,data,start,count),varname)
  ier = nf90_put_var(ncid,varid,data,start,count)
  if( present(ierr) ) then
     ierr = ier
  else
     call nf90call(ier,varname)
  end if
end subroutine write1das2dnc
!---------------------------------------------------------------------
! aura for cesm
subroutine write1das2dnc_cesm(ncid,varname,data,start,count,ierr)
! Writes the contents of a 1D variable data to varname for the
! dimensions defined by 2D start and count.
!
! If ierr is present and the put fails, the error value is returned
! instead of automatically stopping.
!
!----------------------------------------------------------------------
  integer,             intent(in)  :: ncid
  character(len=*),    intent(in)  :: varname
  integer,dimension(:),   intent(in)  :: data
  integer,dimension(2),intent(in)  :: start,count
  integer,optional,    intent(out) :: ierr

  integer :: ier,varid

  call nf90call(nf90_inq_varid(ncid,varname,varid),varname)
!  call nf90call(nf90_put_var(ncid,varid,data,start,count),varname)
  ier = nf90_put_var(ncid,varid,data,start,count)
  if( present(ierr) ) then
     ierr = ier
  else
     call nf90call(ier,varname)
  end if
end subroutine write1das2dnc_cesm
! end aura 
!----------------------------------------------------------------------
subroutine write1das3dnc(ncid,varname,data,start,count,ierr)
! Writes the contents of a 2D variable data to varname for the
! dimensions defined by 3D start and count.
!
! If ierr is present and the put fails, the error value is returned
! instead of automatically stopping.
!
!----------------------------------------------------------------------
  integer,             intent(in)  :: ncid
  character(len=*),    intent(in)  :: varname
  real,dimension(:),   intent(in)  :: data
  integer,dimension(3),intent(in)  :: start,count
  integer,optional,    intent(out) :: ierr

  integer :: ier,varid

  call nf90call(nf90_inq_varid(ncid,varname,varid),varname)
!  call nf90call(nf90_put_var(ncid,varid,data,start,count),varname)
  ier = nf90_put_var(ncid,varid,data,start,count)
  if( present(ierr) ) then
     ierr = ier
  else
     call nf90call(ier,varname)
  end if
end subroutine write1das3dnc

!----------------------------------------------------------------------
subroutine write2das3dnc(ncid,varname,data,start,count)
! Writes the contents of a 2D variable data to varname for the
! dimensions defined by 3D start and count.
!----------------------------------------------------------------------
  integer,             intent(in) :: ncid
  character(len=*),    intent(in) :: varname
  real,dimension(:,:), intent(in) :: data
  integer,dimension(3),intent(in) :: start,count

  integer :: varid

  call nf90call(nf90_inq_varid(ncid,varname,varid),varname)
  call nf90call(nf90_put_var(ncid,varid,data,start,count),varname)
end subroutine write2das3dnc

!----------------------------------------------------------------------
subroutine write1das4dnc(ncid,varname,data,start,count)
! Writes the contents of a 1D variable data to varname for the
! dimensions defined by 4D start and count.
!----------------------------------------------------------------------
  integer,             intent(in) :: ncid
  character(len=*),    intent(in) :: varname
  real,dimension(:),   intent(in) :: data
  integer,dimension(4),intent(in) :: start,count

  integer :: varid

  call nf90call(nf90_inq_varid(ncid,varname,varid),varname)
  call nf90call(nf90_put_var(ncid,varid,data,start,count),varname)
end subroutine write1das4dnc

!----------------------------------------------------------------------
subroutine write2das4dnc(ncid,varname,data,start,count)
! Writes the contents of a 2D variable data to varname for the
! dimensions defined by 4D start and count.
!----------------------------------------------------------------------
  integer,             intent(in) :: ncid
  character(len=*),    intent(in) :: varname
  real,dimension(:,:), intent(in) :: data
  integer,dimension(4),intent(in) :: start,count

  integer :: varid

  call nf90call(nf90_inq_varid(ncid,varname,varid),varname)
  call nf90call(nf90_put_var(ncid,varid,data,start,count),varname)
end subroutine write2das4dnc

!----------------------------------------------------------------------
subroutine write3das4dnc(ncid,varname,data,start,count)
! Writes the contents of a 3D variable data to varname for the
! dimensions defined by 4D start and count.
!----------------------------------------------------------------------
  integer,               intent(in) :: ncid
  character(len=*),      intent(in) :: varname
  real,dimension(:,:,:), intent(in) :: data
  integer,dimension(4),  intent(in) :: start,count

  integer :: varid

  call nf90call(nf90_inq_varid(ncid,varname,varid),varname)
  call nf90call(nf90_put_var(ncid,varid,data,start,count),varname)
end subroutine write3das4dnc

!----------------------------------------------------------------------
subroutine writescalarnc(ncid,varname,data)
! Writes the contents of a scalar variable data to varname.
! 
!----------------------------------------------------------------------
  integer,             intent(in) :: ncid
  character(len=*),    intent(in) :: varname
  real,                intent(in) :: data

  integer :: varid

  call nf90call(nf90_inq_varid(ncid,varname,varid),varname)
  call nf90call(nf90_put_var(ncid,varid,(/data/)),varname)
end subroutine writescalarnc

!----------------------------------------------------------------------
subroutine writescalaras1dnc(ncid,varname,data,start)
! Writes the contents of a 1D variable data to varname at the location
! start.
! 
  integer,             intent(in) :: ncid
  character(len=*),    intent(in) :: varname
  real,                intent(in) :: data
  integer,             intent(in) :: start

  integer :: varid

  call nf90call(nf90_inq_varid(ncid,varname,varid),varname)
  call nf90call(nf90_put_var(ncid,varid,(/data/),(/start/),(/1/)),varname)
end subroutine writescalaras1dnc

end module ncroutines
