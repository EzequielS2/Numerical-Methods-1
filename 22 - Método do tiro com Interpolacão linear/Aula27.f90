



! Esse exemplo � do livro



 program Aula27


         implicit none

         real a, b, DTx, Dwx, h

         real, parameter :: Ac= 1.6*10**(-5), k=240, Hc=40, P=0.016, Ts=293, e=0.4, Gsb = 5.67*10**(-8)
        real, allocatable :: x(:), w1(:), w2(:), w3(:), T1(:), T2(:), T3(:)

        integer i, tam




        print*, "Forne�a os valores do dominio: "

        read*, a, b    ! Atribui o dom�nio �s vari�veis a e b


        print*, "Diga o passo de integra��o: "

        read*, h    ! a largura do passo de integra��o a h

        tam = (b-a)/h      ! Define o tamanho do vetor

        allocate(x(tam))
        allocate(w1(tam))
        allocate(w2(tam))
        allocate(w3(tam))
        allocate(T1(tam))
        allocate(T2(tam))
        allocate(T3(tam))

        print*, "Forne�a o valor inicial de w1: "

        read*, w1(1)  ! Atribui a condi��o inicial ao primeiro ponto da solu��o

        print*, "Forne�a o valor inicial de T: "

        read*, T1(1)  ! Atribui a condi��o inicial ao primeiro ponto da solu��o

        print*, "Forne�a o valor inicial de w2: "

        read*, w2(1)  ! Atribui a condi��o inicial ao primeiro ponto da solu��o


        T2(1)=T1(1);T3(1)=T1(1)  ! Atribui a condi��o inicial ao primeiro ponto da solu��o



        x(1) = a; x(tam)=b   ! Atribui a condi��o inicial ao primeiro ponto da solu��o
        print*, "------------------------"

        print*, "Valores iniciais 1: ", "x:", x(1), "w1:", w1(1), "T1:", T1(1)

        print*, "Valores iniciais 2: ", "x:", x(1), "w2:", w2(1), "T2:", T2(1)

        print*, "------------------------"

        call Sol_Sistema(x, w1, T1, h, tam, Ac, k, Hc, P, Ts, e, Gsb)

         call Sol_Sistema(x, w2, T2, h, tam, Ac, k, Hc, P, Ts, e, Gsb)



        w3(2) = w1(1) + (293-T1(2))*(w2(1) - w1(1))*(1/(T2(2)-T1(2)))

        print*, "------------------------"

        print*, "Valores iniciais 3: ", "x:", x(1), "w3:", w3(1), "T3:", T2(1)

         print*, "------------------------"

        call Sol_Sistema(x, w3, T3, h, tam, Ac, k, Hc, P, Ts, e, Gsb)



        print*, "Quantidade de elemntos n:", tam




       do i=1, tam

            print*, "i:", i, "x:", x(i), "w1:", w1(i), "Temperatura T1:", T1(i)
            print*, "i:", i, "x:", x(i), "w2:", w2(i), "Temperatura T2:", T2(i)
            print*, "i:", i, "x:", x(i), "w3:", w3(i), "Temperatura T3:", T3(i)
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



subroutine Sol_Sistema(x, w, T, h, tam, Ac, k, Hc, P, Ts, e, Gsb)

        integer i, tam

        real h, Ac, k, Hc, P, Ts, e, Gsb, KT1, KT2, Kw1, Kw2

          real, intent(out) :: x(tam), w(tam), T(tam)


      do i=1, tam

                x(i+1) = x(i) + h

                KT1 = DTx(x(i), w(i), T(i))

                Kw1 = Dwx(x(i), w(i), T(i), Ac, k, Hc, P, Ts, e, Gsb)



                KT2 = DTx(x(i+1), w(i)+Kw1*h, T(i)+KT1*h)

                Kw2 = Dwx(x(i+1), w(i)+Kw1*h, T(i)+KT1*h)




                w(i+1) = w(i) + (h/2.0)*(Kw1+Kw2)

                T(i+1) = T(i) + (h/2.0)*(KT1+KT2)



       end do

end subroutine Sol_Sistema
