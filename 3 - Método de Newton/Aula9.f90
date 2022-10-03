! Para este método precisamos das seguintes variaáveis ou entradas

! Uma funcao

! Calculo da derivada da funcao

! Uma entrada para o valor de X inicial

! O Xi

! A tolerancia ou erro maximo

! O numero maximo de iteraçõs


real function f(a)

     implicit none
     
     real a
     
     f=a**2-7  ! Quer dizer a^2-7 (Não pode ser linear as funções)

     return
end function f


real function Fder(a1)

     implicit none
     
     real a1
     
     Fder= 2*a1 ! Pois a derivada de f é 2*a1
     
     return
end function Fder


 program Aula9
 
         implicit none
         
         real x, xi, Xsolucao, Fder, f, tol_ini, tol
         
         integer i, itmax
         
         
         tol_ini=0.001; x=2; itmax=10      ! x = o chute da solução
         
         
        do i=1, itmax
         
         xi=x-(f(x)/(Fder(x)))
         
         tol=(xi-x)/x
         
         if(tol<tol_ini)   then

              Xsolucao=xi
              exit           ! para o comando (do)
         end if
        
         x=xi
        end do
        
        if(i==itmax)   then
        
        print*, "Não tem solução"
        
        else
        
        print*,"A solucao é: ", Xsolucao
        
        end if
end program Aula9
         
         
