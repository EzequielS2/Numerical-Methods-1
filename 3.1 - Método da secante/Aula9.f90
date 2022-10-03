! Para este m�todo precisamos das seguintes varia�veis ou entradas

! Uma funcao

! Duas entradas para o valor de Xa e Xb iniciais

! O Xi

! A tolerancia ou erro maximo

! O numero maximo de itera��s


real function f(a)

     implicit none
     
     real a
     
     f=a**2-7  ! Quer dizer a^2-7 (N�o pode ser linear as fun��es)

     return
end function f





 program Aula9
 
         implicit none
         
         real Xa, Xb, xi, Xsolucao, Fder, f, tol_ini, tol
         
         integer i, itmax
         
         
         tol_ini=0.001; x=2; itmax=10      ! x = o chute da solu��o
         
         
        do i=1, itmax
         
         xi= Xb - (f(Xb)*(Xa-Xb))/(f(Xa)-f(Xb))
         
         tol=(Xb-Xa)/Xa
         
         if(tol<tol_ini)   then

              Xsolucao=xi
              exit           ! para o comando (do)
         end if
         
         Xa=Xb
         Xb=xi

        end do
        
        if(i==itmax)   then
        
        print*, "N�o tem solu��o"
        
        else
        
        print*,"A solucao �: ", Xsolucao
        
        end if
end program Aula9
         
         
