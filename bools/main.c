#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

typedef uint64_t SNAKEVAL;

const uint64_t BOOL_TAG   = 0x0000000000000001;
const SNAKEVAL BOOL_TRUE  = 9223372036854775807; // These must be the same values
const SNAKEVAL BOOL_FALSE = -1; // as chosen in compile.ml

SNAKEVAL print(SNAKEVAL val) {
  if ((val & BOOL_TAG) == 0) { // val is even ==> number
    printf("%ld", ((int64_t)(val)) / 2); // shift bits right to remove tag
  } else if (val == BOOL_TRUE) {
    printf("true");
  } else if (val == BOOL_FALSE) {
    printf("false");
  } else {
    printf("Unknown value: %#018lx", val); // print unknown val in hex
  }
  return val;
}

const int ERR_NOT_NUMBER = 1;
const int ERR_NOT_BOOLEAN = 2;
// other error codes here

void error(int errCode, SNAKEVAL val) {
  if (errCode == ERR_NOT_NUMBER) {
    fprintf(stderr, "Expected number, but got %010lx\n", val);
  } else if (errCode == ERR_NOT_BOOLEAN) {
    fprintf(stderr, "Expected boolean, but got %010lx\n", val);
  } 

    exit(errCode);
}

SNAKEVAL max(SNAKEVAL a, SNAKEVAL b) {
  if (a & BOOL_TAG)
    error(ERR_NOT_NUMBER, a);

  if (b & BOOL_TAG)
    error(ERR_NOT_NUMBER, b);

  if (a > b)
    return a;
  else
    return b;
}

extern int64_t our_code_starts_here() asm("our_code_starts_here");

int main(int argc, char** argv) {
  int64_t result = our_code_starts_here();
  print(result);
  printf("\n");
  return 0;
}
