#include "cloud.h"
#include "disease.h"
#include<cmath>

void Cloud::integration() {
    int qtd=0;
    if(sporesToBeRemoved>0) { // Remove from cloud spores used to infect tissue
        removeSporesCloud(sporesToBeRemoved);
/*        for (unsigned int i = 0; i < values.size() && sporesToBeRemoved>0; i++) {
            qtd = fmin(values[i],sporesToBeRemoved);
            sporesToBeRemoved -= qtd;
            values[i] = values[i] - qtd;
        }*/
    }
    values.push_back(disease->getDispersionFreequency() * sporesCreated);
    sporesCreated = sporesToBeRemoved = 0;
}

double Cloud::getValue() {
    double sum = 0;
    for (unsigned int i = 0; i < values.size(); i++)
        sum += values[i];

    return sum;
}

void Cloud::removeSporesCloud(double toBeRemoved) {
    double total = getValue();
    for (unsigned int i = 0; i < values.size() && total > 0; i++) {
        values[i] -= (toBeRemoved * (values[i] / total));
    }
}

void Cloud::removeSporesCloudByRain(double percent) {
    double oldValue = 0;
    for (unsigned int i = 0; i < values.size(); i++) {
        oldValue = values[i];
        values[i] = (oldValue * percent);
    }
}
