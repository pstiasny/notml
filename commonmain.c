#include <stdio.h>

extern int calcmain();

int main(int argc, char** argv) {
    int result = calcmain();
    printf("program result: %d\n", result);
    return 0;
}
