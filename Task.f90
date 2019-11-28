MODULE Task
use mpi
implicit none
contains


    !Ищет координаты левого верхнего и правого нижнего углов подматрицы A, имеющей наибольшую сумму элементов.
    SUBROUTINE GetMaxCoordinates( A, x1, y1, x2, y2 )

    real(8), dimension(:,:), intent(in) :: A  !Исходная матрица.
    integer(4), intent(out) :: x1, y1, x2, y2 !Нужные координаты.
    integer(4) n                              !Число строк.
    integer(4) m                              !Число столбцов( элементов в строке ).
    integer(4) Left, Up                       !Левый верхний угол подматрицы.
    integer(4) Right, Down                    !Правый нижний угол подматрицы.
    real(8), allocatable :: current_line(:)   !Строка, с которой мы работаем.
    real(8) current_sum                       !Хранит промежуточную сумму.
    real(8) max_sum                           !Максимальная сумма.

    !Переменные, связанные с MPI.
    integer(4) mpiErr                              !Код ошибки.
    integer(4) mpiSize                             !Размер коммуникатора - число процессов в нем.
    integer(4) mpiRank                             !Номер процесса в коммуникаторе.
    real(8), allocatable, dimension(:) :: max_subA !Хранит в себе копии max_sum от каждого процесса.
    integer(4) numRank_max_subA                    !Позиция наибольшего элемента в max_subA.

        call MPI_COMM_SIZE( MPI_COMM_WORLD, mpiSize, mpiErr ) !Дает число процессов в коммуникаторе.
        call MPI_COMM_RANK( MPI_COMM_WORLD, mpiRank, mpiErr ) !Дает номер процесса в коммуникаторе.

        n = size( A, dim = 2 )
        m = size( A, dim = 1 )

        allocate( current_line(m) )

        max_sum = A(1,1)

        x1 = 1
        y1 = 1

        x2 = 1
        y2 = 1

        !Проходит по строкам. Разделяет итерации между процессами.
        do Up = mpiRank+1, n, mpiSize

            current_line = A(:,Up)

            do Down = Up, n
 
                !Берет строку и складывает ее с остальными строками матрицы. Результат передается GetMaxInArray...
                if( Down > Up ) then

                    current_line = current_line + A(:,Down)

                endif

                call GetMaxInArray( current_line, current_sum, Left, Right )

                !Если текущая сумма больше максимальной, то перезаписывает координаты и контрольную сумму.
                if( current_sum > max_sum ) then

                    max_sum = current_sum

                    x1 = Left
                    y1 = Up

                    x2 = Right
                    y2 = Down

                endif

            end do

        end do

        !Размер буфера равен числу процессов.Каждый процесс запишет туда свою max_sum.
        if( mpiRank == 0 ) then 

            allocate( max_subA(0:mpiSize-1) )

        endif

        !Все процессы посылают процессу 0 max_sum.
        call mpi_gather( max_sum, 1, MPI_REAL8, max_subA, 1 , MPI_REAL8, 0, MPI_COMM_WORLD, mpiErr )

        !Ищет процесс с максимальным значением max_sum.
        if( mpiRank == 0 ) then

            numRank_max_subA = maxloc( max_subA, 1 ) - 1

        endif

        !Оповещение остальных участников игры о победителе.
        call mpi_bcast( numRank_max_subA, 1, MPI_REAL8, 0, MPI_COMM_WORLD, mpiErr )

        !Рассылка результата от numRank_max_subA всем процессам.
        call mpi_bcast( x1, 1, MPI_REAL8, numRank_max_subA, MPI_COMM_WORLD, mpiErr )
        call mpi_bcast( y1, 1, MPI_REAL8, numRank_max_subA, MPI_COMM_WORLD, mpiErr )

        call mpi_bcast( x2, 1, MPI_REAL8, numRank_max_subA, MPI_COMM_WORLD, mpiErr )
        call mpi_bcast( y2, 1, MPI_REAL8, numRank_max_subA, MPI_COMM_WORLD, mpiErr )

        if( mpiRank == 0 ) then

            deallocate( max_subA )

        endif

        deallocate( current_line )

    END SUBROUTINE GetMaxCoordinates


    !Берет на вход подотрезок, на выходе - левая и правая границы подматрицы, а также сумма элементов рабочей строки.
    SUBROUTINE GetMaxInArray( cur_l, Sum, L, R )

    real(8), intent(in), dimension(:) :: cur_l !Отрезок, с которым мы работаем.
    integer(4), intent(out) :: L               !Левая граница подотрезка.
    integer(4), intent(out) :: R               !Правая граница подотрезка.
    real(8), intent(out) :: Sum                !Контрольная сумма.
    real(8) cur_sum                            !Текущая сумма элементов отрезка.
    integer(4) i                               !Счетчик.
    integer(4) minus_pos                       !Запоминает, где последний раз значение суммы было отрицательным.

        Sum = cur_l(1)
        L = 1
        R = 1
        cur_sum = 0
        minus_pos = 0

        !Складывает элементы отрезка.
        do i = 1, size( cur_l )

            cur_sum = cur_sum + cur_l(i)

            if( cur_sum > Sum ) then

                Sum = cur_sum
                L = minus_pos + 1
                R = i

            endif

            !Если текущая сумма сделалась отрицательной на i-том элементе, то приравнивает ее к нулю и minus_pos - к i.
            if( cur_sum < 0 ) then

                cur_sum = 0
                minus_pos = i

            endif

        enddo

    END SUBROUTINE GetMaxInArray


END MODULE Task



