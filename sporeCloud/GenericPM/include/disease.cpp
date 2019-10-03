#include "disease.h"
#include<cmath>
#include<iostream>

std::vector<Disease*> Disease::listDiseases;

double Disease::getSporulationCrowdingFactor(double proportionDiseaseArea) {
    double a = (1 / (sporulationCrowdingFactorsSet[0] + sporulationCrowdingFactorsSet[1] * pow(proportionDiseaseArea, sporulationCrowdingFactorsSet[2])));
    return (fmin(a,1));
}

int Disease::newLesions(double cloudDensity, double healthyAreaProportion) {
    Utilities util;
    double newLesions = 0;
    double fitWetnessThreshold = getWetnessThreshold();

    if (healthyAreaProportion > 0 && Basic::getWeather()->getWetDur() >= fitWetnessThreshold) {
        newLesions = (cloudDensity * healthyAreaProportion * getInfectionEfficiency() *
                util.temperatureFavorability(Basic::getWeather()->getTMean(),
                                             getTemperatureFavorabilitySet()) *
                util.wetnessFavorability(Basic::getWeather()->getWetDur()));
    }
    return newLesions;
}
