
/*
 * Each new term in the Fibonacci sequence is generated by adding the previous
 * two terms. By starting with 1 and 2, the first 10 terms will be: 1, 2, 3, 5,
 * 8, 13, 21, 34, 55, 89, ... By considering the terms in the Fibonacci sequence
 * whose values do not exceed four million, find the sum of the even-valued
 * terms.
 *
 */



#include <stdio.h>


int main(void) {

    int t1, t2, next_t, n, sum;

    t1 = 1;
    t2 = 2;
    n = 4000000;
    sum = 0;

    printf("Series: ");

    while (t1 < n) {

        printf("%i ", t1);

        if (t1 % 2 == 0) {
            sum += t1;
        };

        next_t = t2 + t1;
        t1 = t2;
        t2 = next_t;

    }

    printf("\n");
    printf("The sum of even-valued term(s) is %i\n", sum);
    return 0;

}