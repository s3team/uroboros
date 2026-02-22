#include <stdio.h>
#include <unistd.h>

int digital_values[4] = {0, 0, 0, 0}; 
float analog_values[4] = {0.0, 0.0, 0.0, 0.0};


int digitalRead(int pin) {
    return digital_values[pin & 3]; 
}

void digitalWrite(int pin, int value) {
    digital_values[pin & 3] = value & 1;
}

float analogRead(int pin) {
    return analog_values[pin & 3]; 
}

void analogWrite(int pin, float value) {
    analog_values[pin & 3] = (value < 0.0) ? 0.0 : ((value > 1.0) ? 1.0 : value);
}

int main() {
    int counter = 0;
    int cycle = 0;
    while (1) {
    printf("Cycle: %d\n", cycle);
        int dv = digitalRead(counter);
        dv = dv ^ ((cycle & (1 << counter)) ? 1 : 0); 
        digitalWrite(counter, dv);

        float av = analogRead(counter);
        float p = ((float) cycle) / 256.0;
        av = av + (1.0 - av) * p;
        analogWrite(counter, av);

        counter = (counter + 1) & 3;
        cycle = (cycle + 1) & 255;
        sleep(1);
    }   
}
