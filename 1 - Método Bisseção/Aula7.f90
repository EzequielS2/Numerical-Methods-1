! Para o método da bisseção temos que ter (Que é obter a solução de uma equação não linear):

! Os valores de [a,b]

! A quantidade máxima de iterações do programa (para o for)

! 3) A tolerância em f(x) (Ou seja, assumir um valor aproximado de zero para a solução - tipo, 0,0001)

! Tolerâcia da solução: tipo, Xn=(a+b)/2

! E, por último, a tolerãncia: (b-a)/2  na função para cada iteração (ela vai ser comparada a tolerância de (3))


real function f(a)

      implicit NONE
      real a
      
      f = 8-4.5*(a-sin(a))
      
      return
end function f



program Aula7

        implicit none

        
        real a, b, Xn, itmax, tol, tol_ini, f
        
        integer i
        
        a=2; b=3; itmax=20; tol_ini=0.0001
        
        if(f(a)*f(b)>0) then

           print*, "Erro: A função tem o mesmo sinal"
           stop
        else
            print*, "i             a                b               Xn(Soulcoes)          f(Xn)               &tolerancia"
        
            do i=1, 20, 1

                 Xn=(a+b)/2

                 tol=(b-a)/2
                 
                 print*, i, a, b, Xn, f(Xn), tol
                 
                 if(f(Xn)==0)  then
                 
                 print*, "Uma solução foi encontrada: ", Xn
                 stop
                 end if

                 if(i==itmax) then
                 
                 print*, "Nenhuma solução encontrada"
                 stop
                 end if
                 
                 if(tol<tol_ini)  then
                 stop
                 end if

                 if(f(a)*f(Xn)<0) then
                 b=Xn
                 else
                 a=Xn
                 end if
            end do
         end if
end program Aula7
                 

        
        
        
        
