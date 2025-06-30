#include <stdio.h>

// void capture_digitalRead () {
//   int edi_val;
//   __asm__ __volatile__ (
//     "mov %%edi, %0" // Assembly instruction: move RAX into the output operand
//     : "=r" (edi_val) // Output operand
//     : // No input operands
//     : // No clobbered registers
//   );
//   printf("digitalRead called. pin=%d\n", edi_val);
// }

void capture_digitalRead(int val) {
   printf("digitalRead called. pin=%d\n", val);
}
void capture_digitalWrite(int val1, int val2) {
   printf("digitalWrite called. pin=%d val=%d\n", val1, val2);
}
void capture_analogRead(int val) {
   printf("analogRead called. pin=%d\n", val);
}
void capture_analogWrite(int val1, float val2) {
   printf("analogWrite called. pin=%d val=%f\n", val1, val2);
}
