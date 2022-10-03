! Para o m�todo da regula... temos que ter (Que � obter a solu��o de uma equa��o n�o linear):

! Os valores de [a,b]

! A quantidade m�xima de itera��es do programa (para o for)

! 3) A toler�ncia em f(x) (Ou seja, assumir um valor aproximado de zero para a solu��o - tipo, 0,0001)

! Toler�cia da solu��o: tipo, Xn=(af(b)-bf(a))/(f(a)-f(b))

! E, por �ltimo, a toler�ncia: (b-a)/2  na fun��o para cada itera��o (ela vai ser comparada a toler�ncia de (3))


real function F(k)

         implicit none
         
         real k
         
         F=2*k-2
         
         return
end function F



program Aula8

        implicit none
        
        real F, x, tol_ini, tol, a, b
        integer itmax, i

        tol_ini=0.001; a=-5; b=2; itmax=5            ! definido os valores de a e b para o intervalo que vamos pegar [a,b]
        
        if(F(a)*F(b)>0) then
        
        print*, "N�o temos solu��o nesse intervalo"
        
        else

            print*, "i          a               b           X            F(X)           tolerancia"
            
            do i=1, itmax
            
               x=(a*F(b)-b*F(a))/(F(b)-F(a))
               
               tol=(b-a)/2
               
               print*, i, a, b, x, F(x), tol
               
               if(F(x)==0) then
               
               print*, "A solu��o �: ", x
               stop
               end if
               
               if(tol<tol_ini) then
               
               stop
               end if
               
               if(i==itmax)  then
               stop
               end if
               
               if(F(a)*F(x)<0)  then
               
               b=x
               else
               
               a=x
               
               end if
         end do
      end if
end program Aula8
               

        
