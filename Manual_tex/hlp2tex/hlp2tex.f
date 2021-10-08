	program hlp2tex
c+
c	Converter from DEC HLP file to LATEX ..
c
c	Version : 0.1
c	Author  : Th. Proffen (proffen@rsc.anu.edu.au)
c-
	implicit none
c
	integer			ihl,irt
	parameter		(ihl=1, irt=2)
c
	character*200		zeile,header
	character*90		hlpfile
	character*20	 	version
	real			werte
	integer			ianz,ilev,il,len_str,i,ioff
	logical			first,pre,spc,lstart,lf90
c
	integer			iarg,iargc
c
	common /help/ version
c
	first = .true.
	pre   = .false.
	spc   = .false.
	lstart= .false.
	lf90  = .false.
c
	ioff  = 0
c
	iarg=iargc()
	if (iarg.ne.2) then
	  write(*,*) 'Usage: hlp2tex basename title ..'
	  stop
	endif
c
	call getarg(1,hlpfile)
	call getarg(2,header)
c
	lf90=(hlpfile.eq.'lib_f90')
c
	il = len_str(hlpfile)
	open (unit=ihl,file=hlpfile(1:il)//'.hlp',status='old')
	open (unit=irt,file=hlpfile(1:il)//'.tex',status='unknown')
c
	i=0
10	continue
	  read (ihl,5000,end=20) zeile
          IF(zeile(1:2) /= '!%') then
	  i=i+1
	  call hole_zahl (zeile(1:3),ianz,werte,i)
	  if (ianz.ne.0) then
	    ilev = nint(werte)
	    if (lf90) ilev=ilev-1
	    call write_header (irt,zeile,header,ilev,first,pre,ioff,ihl)
	    if (ilev.gt.1) lstart=.true.
	  else
	    if (lstart) call write_line (irt,zeile,pre,spc)
	  endif
	  ENDIF
	goto 10
20	continue
	if (pre) write (irt,5000) '\end{MacVerbatim}'
c
	close (irt)
	close (ihl)
c
5000	format (a)
c
	end
c*****7*****************************************************************
	subroutine check_line (zeile,outline,il)
c+
c	Checks and replaces special characters in line
c-
	implicit	none
c
	character*(*)	zeile,outline
	integer		i,j,il,len_str
c
	outline = ' '
	il = len_str(zeile)
	j=1
	do i=1,il
	  if (zeile(i:i).eq.'{') then
	    outline(j:j+4)='$ \{$'
	    j=j+5
	  elseif (zeile(i:i).eq.'}') then
	    outline(j:j+4)='$\} $'
	    j=j+5
	  elseif (zeile(i:i).eq.'#') then
	    outline(j:j+1)='\#'
	    j=j+2
	  elseif (zeile(i:i).eq.'_') then
	    outline(j:j+1)='\_'
	    j=j+2
	  elseif (zeile(i:i).eq.'$') then
	    outline(j:j+1)='\$'
	    j=j+2
	  elseif (zeile(i:i).eq.'&') then
	    outline(j:j+1)='\&'
	    j=j+2
	  elseif (zeile(i:i).eq.'%') then
	    outline(j:j+1)='\%'
	    j=j+2
	  elseif (zeile(i:i).eq.'|') then
	    outline(j:j+3)='$| $'
	    j=j+4
	  elseif (zeile(i:i).eq.'<') then
	    outline(j:j+3)='$ <$'
	    j=j+4
	  elseif (zeile(i:i).eq.'>') then
	    outline(j:j+3)='$> $'
	    j=j+4
	  elseif (zeile(i:i).eq.'^') then
	    outline(j:j+3)='$**$'
	    j=j+4
	  elseif (zeile(i:i).eq.'\') then
	    outline(j:j+11)='$\backslash$'
	    j=j+12
	  else
	    outline(j:j)=zeile(i:i)
	    j=j+1
	  endif
	enddo
	il = j-1
c
	end
c*****7*****************************************************************
	subroutine write_line (irt,zeile,pre,spc)
c+
c	Write normal line
c-
	implicit	none
c
	character*(*)	zeile
	character*200	outline
	integer		irt,il,len_str
	logical		pre,spc
c
	call check_line (zeile,outline,il)
c
	if(outline(1:2).eq.'!p') then
	  if (.not.pre) write (irt,1) '\begin{MacVerbatim}'
	  pre = .true.
	  il=len_str(zeile)
	  write (irt,1) zeile(4:il)
	elseif(outline(1:2).eq.'!i') then
	  if (pre) write (irt,1) '\end{MacVerbatim}'
	  write (irt,1) '{\it '//outline(4:il)//' \par }'
	  pre = .false.
	  spc = .true.
	elseif(outline(1:2).eq.'!b') then
	  if (pre) write (irt,1) '\end{MacVerbatim}'
	  write (irt,1) '{\bf '//outline(4:il)//' \par }'
	  pre = .false.
	  spc = .true.
	elseif(outline(1:2).eq.'!u') then
	  if (pre) write (irt,1) '\end{MacVerbatim}'
	  write (irt,1) '{\underline {'//
     &	                 outline(4:il)//' \par }'
	  pre = .false.
	  spc = .true.
	elseif(outline(1:2).eq.'!x') then
	  if (pre) write (irt,1) '\end{MacVerbatim}'
	  spc = .false.
	  continue
	elseif(il.lt.4) then
	  if (.not.pre) then
	    write (irt,1) '\par'
	  endif
	else
	  if (pre) write (irt,1) '\end{MacVerbatim}'
	  if (spc) write (irt,1) '\vspace{3pt}'
	  write (irt,1) outline(4:il)//' '
	  pre = .false.
	  spc = .false.
	endif
c
1	format(a)
	end
c*****7*****************************************************************
	subroutine write_header (irt,zeile,tit,ilev,first,pre,ioff,ihl)
c+
c	Writes heading for new HLP file entry
c-
	implicit	none
c
	character*(*)	zeile,tit
	character*80	line
	integer		irt,ilev,ioff,ihl
	logical		first,pre
c
	character*200	typ,outline
	character*14	header(6)
	integer		il,ih,izei,ihead,len_str
c
	header(1) = '\chapter'
	header(2) = '\section'
	header(3) = '\subsection*'
	header(4) = '\subsubsection*'
	header(5) = '\paragraph*'
	header(6) = '\subparagraph*'
c
	ihead = len_str(header(ilev))
	ih    = len_str(tit)
	call check_line (zeile,line,izei)
	if (izei.lt.4) return
c
	if (pre) write (irt,1000) '\end{MacVerbatim}'
	pre = .false.
c
	if (line(4:4).ne.'-') then
	  if (ilev.ne.1) then
	    if (first) then
	      write(irt,1000) header(1)(1:len_str(header(1)))//
     &	                      '{'//tit(1:ih)//' commands}'
	      first = .false.
	    endif
     	    write (irt,1000) header(ilev)(1:ihead)//
     &	                     '{'//line(4:izei)//'}'
	  endif
	else
          read (ihl,1000) typ
          IF(typ(1:2) /= '!%') THEN
	  il=len_str(typ)
	  call check_line (typ,outline,il)
     	  write (irt,1000) header(ilev)(1:ihead)//
     &	                   '{Error '//line(4:izei)//
     &	                   ': '//outline(4:il)//'}'
	  ENDIF
	endif
c
1000	format (a)
c
	end
c*****7*****************************************************************
        integer function len_str(string)
c-
c       determines the position of the last non-blank character
c+
        implicit       none
c
        character*(*)  string
        integer        laenge,i
c
        laenge=len(string)
        i=laenge
        do while (i.gt.0 .and. string(i:i).eq.' ')
          i=i-1
        enddo
        len_str=i
        end
c*****7*****************************************************************
        subroutine hole_zahl(zeile,ianz,werte,iz)
c+
c	Gets numbers from command line and number of values of input.
c-
	implicit	none
c
        character*(*) 	zeile
	integer       	ianz,iz
	real          	werte
c
	if (zeile(1:1).eq.' ' .or. zeile(1:1).eq.'!') then
	  ianz=0
	  werte=0.0
	else
          read(zeile(1:3),*,ERR=10,END=10) werte
          ianz=1
	endif
	return
c
10	continue
	write (*,1000) iz
1000	format ('*** Error reading line ',I5,' ***')
	stop
c
        end
