



! Esse exemplo é do livro



 program Aula27


         implicit none

         real a, b, DTx, Dwx, h, wH, wL, tol, E1, T

         real, parameter :: Ac= 1.6*10**(-5), k=240, Hc=40, P=0.016, Ts=293, e=0.4, Gsb = 5.67*10**(-8), Yb=293
        real, allocatable :: x(:), w1(:), T1(:)

        integer i, tam


        tol = 0.01 ! tolerâcia do erro

        print*, "Forneça os valores do dominio: "

        read*, a, b    ! Atribui o domínio às variáveis a e b


        print*, "Diga o passo de integração: "

        read*, h    ! a largura do passo de integração a h

        tam = (b-a)/h      ! Define o tamanho do vetor

        allocate(x(tam))
        allocate(w1(tam))
        allocate(T1(tam))

        print*, "Forneça o valor inicial de w1: "

        read*, wL  ! Atribui a condição inicial ao primeiro ponto da solução

        print*, "Forneça o valor inicial de T: "

        read*, T  ! Atribui a condição inicial ao primeiro ponto da solução

        print*, "Forneça o valor inicial de w2: "

        read*, wH  ! Atribui a condição inicial ao primeiro ponto da solução


        print*, "Forneça o valor inicial para o resultado que queremos: "

        read*, w1(1)  ! Atribui a condição inicial ao primeiro ponto da solução



        T1(1)=T;  ! Atribui a condição inicial ao primeiro ponto da solução



        x(1) = a; x(tam)=b   ! Atribui a condição inicial ao primeiro ponto da solução





        T1(1)=T

        print*, "------------------------"

        print*, "Valores iniciais 1: ", "x:", x(1), "w1:", w1(1), "T1:", T1(1)

        print*, "------------------------"

        do i=2, tam
        
           x(i) = x(i-1) + h
           
           w1(i) = (wH+wL)/2.0

           call Sol_Sistema2(x, w1, T1, h, tam, Ac, k, Hc, P, Ts, e, Gsb, i)

           E1 = T1(i) - Yb

           if(abs(E)<tol) then
                  exit
           end if

           if(E>0) then

                   wH=w1(i)
           else

           wL = w1(i)

           end if
        end do

        if(i>tam) then

        print*, "A solução não foi obtida em:", tam

        end if


        print*, "Quantidade de elemntos n:", tam




       do i=1, tam

            print*, "i:", i, "x:", x(i), "w1:", w1(i), "Temperatura T1:", T1(i)

            end do


end program Aula27



real function DTx(x, w, T)    ! Primeira 2 EDO

    real x, w, T

     DTx = w

       return
end function DTx



real function Dwx(x, w, T, Ac, k, Hc, P, Ts, e, Gsb)   ! Segunda 2 EDO

      real x, w, T, Ac, k, Hc, P, Ts, e, Gsb

     Dwx = (1/k*Ac)*Hc*P*(T-Ts) + (1/k*Ac)*e*Gsb*P*(T**4-Ts**4)

     return

end function Dwx





subroutine Sol_Sistema2(x, w, T, h, tam, Ac, k, Hc, P, Ts, e, Gsb, i)

        integer i, tam

        real h, Ac, k, Hc, P, Ts, e, Gsb, KT1, Kw1, KT2, Kw2, x(tam), w(tam)

          real, intent(out) :: T(tam)






                KT1 = DTx(x(i), w(i), T(i))




                KT2 = DTx(x(i+1), w(i)+Kw1*h, T(i)+KT1*h)



                T(i) = T(i-1) + (h/2.0)*(KT1+KT2)





end subroutine Sol_Sistema2
