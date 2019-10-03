#ifndef DISEASE_H
#define DISEASE_H

#include "basic.h"
#include "utilities.h"

#include <string>
#include <vector>
#include<algorithm>

class Disease : public Basic {
protected:
    int id;
    std::string cropModel = "CRGRO";
    std::string description = "Soybean Leaf Rust";
    double infectionEfficiency = 0.17;
    double depositionFrequency = 0.15;
    double initialInoculum = 50;
    int vectorSizeCloudF = 5;
    int vectorSizeCloudP = 7;
    int vectorSizeCloudO = 10;
    double dailySporeProductionPerLesion = 3000;
    double cohortAgeSet[4] = {4,20,22,30};
    double sporulationCrowdingFactorsSet[3] = {0.98669, 10.71894, 0.93374}; 
    double maxSporeCloudsDensity = 12000;
    double proportionFromOrganToPlantCloud = 0.20;
    double proportionFromPlantToFieldCloud = 0.30;
    double temperatureFavorabilitySet[3] = {29,10,22.5};
    int latentPeriod = 7;
    int infectionPeriod = 21;
    std::string visibleGrowthFunction = "0.4*exp(-10*exp(-0.4*x))";
    double initialPustuleSize = 0.00001;
    std::string invisibleGrowthFunction = "0.8*exp(-10*exp(-0.4*x))";
    double wetnessThreshold = 6;
    double acumulateFavorability = 35;
    double hostFactor = 1;
    double cardinalTempPhysiologicalLife[4] = {0, 28, 30, 40};
    static std::vector<Disease*> listDiseases;


public:

    Disease() {
        listDiseases.push_back(this);
    }

    static std::vector<Disease*>& getDisease() {
        return listDiseases;
    }
    double getSporulationCrowdingFactor(double proportionDiseaseArea);
    int newLesions(double cloudDensity, double healthyAreaProportion);

    double getProportionFromOrganToPlantCloud() {
        return proportionFromOrganToPlantCloud;
    }

    double getProportionFromPlantToFieldCloud() {
        return proportionFromPlantToFieldCloud;
    }

    void setProportionFromOrganToPlantCloud(double proportionFromOrganToPlantCloud) {
        this->proportionFromOrganToPlantCloud = proportionFromOrganToPlantCloud;
    }

    void setProportionFromPlantToFieldCloud(double proportionFromPlantToFieldCloud) {
        this->proportionFromPlantToFieldCloud = proportionFromPlantToFieldCloud;
    }

    void setDispersionFreequency(double depositionFrequency) {
        this->depositionFrequency = depositionFrequency;
    }

    double getDispersionFreequency() {
        return depositionFrequency;
    }

    void setMaxSporeCloudsDensity(double maxSporeCloudsDensity) {
        this->maxSporeCloudsDensity = maxSporeCloudsDensity;
    }

    double getMaxSporeCloudsDensity() {
        return maxSporeCloudsDensity;
    }

    void setCohortAgeSet(double cohortAgeSet[]) {
        std::copy(cohortAgeSet, cohortAgeSet + 4, this->cohortAgeSet);
    }

    double* getCohortAgeSet() {
        return &cohortAgeSet[0];
    }

    double getDailySporeProductionPerLesion() {
        return dailySporeProductionPerLesion;
    }

    int getId() {
        return id;
    }

    void setId(int id) {
        this->id = id;
    }

    double getWetnessThreshold() {
        return wetnessThreshold;
    }

    void setWetnessThreshold(double wetnessThreshold) {
        this->wetnessThreshold = wetnessThreshold;
    }

    std::string getDescription() {
        return description;
    }

    void setDescription(std::string description) {
        this->description = description;
    }

    int getLatentPeriod() {
        return latentPeriod;
    }

    void setLatentPeriod(int latentPeriod) {
        this->latentPeriod = latentPeriod;
    }

    int getInfectionPeriod() {
        return infectionPeriod;
    }

    void setInfectionPeriod(int infectionPeriod) {
        this->infectionPeriod = infectionPeriod;
    }

    double getInfectionEfficiency() {
        return infectionEfficiency;
    }

    void setInfectionEfficiency(double infectionEfficiency) {
        this->infectionEfficiency = infectionEfficiency;
    }

    double* getTemperatureFavorabilitySet() {
        return &temperatureFavorabilitySet[0];
    }

    void setTemperatureFavorabilitySet(double temperatureFavorabilitySet[]) {
        std::copy(temperatureFavorabilitySet, temperatureFavorabilitySet + 3, this->temperatureFavorabilitySet);
    }

    int getVectorSizeCloudF() {
        return vectorSizeCloudF;
    }

    int getVectorSizeCloudO() {
        return vectorSizeCloudO;
    }

    int getVectorSizeCloudP() {
        return vectorSizeCloudP;
    }

    double getHostFactor() {
        return hostFactor;
    }

    double getInitialPustuleSize() {
        return initialPustuleSize;
    }

    void setInitialPustuleSize(double initialPustuleSize) {
        this->initialPustuleSize = initialPustuleSize;
    }

    std::string getInvisibleGrowthFunction() {
        return invisibleGrowthFunction;
    }

    double getInitialInoculum() {
        return initialInoculum;
    }

    void setInitialInoculum(double initialInoculum) {
        this->initialInoculum = initialInoculum;
    }

    double getAcumulateFavorability() {
        return acumulateFavorability;
    }

    void setVectorSizeCloudF(int vectorSizeCloudF) {
        this->vectorSizeCloudF = vectorSizeCloudF;
    }

    void setVectorSizeCloudP(int vectorSizeCloudP) {
        this->vectorSizeCloudP = vectorSizeCloudP;
    }

    void setVectorSizeCloudO(int vectorSizeCloudO) {
        this->vectorSizeCloudO = vectorSizeCloudO;
    }

    void setDailySporeProductionPerLesion(double dailySporeProductionPerLesion) {
        this->dailySporeProductionPerLesion = dailySporeProductionPerLesion;
    }

    void setSporulationCrowdingFactorsSet(double sporulationCrowdingFactorsSet[]) {
        std::copy(sporulationCrowdingFactorsSet, sporulationCrowdingFactorsSet + 4, this->sporulationCrowdingFactorsSet);
    }

    double* getSporulationCrowdingFactorsSet() {
        return &sporulationCrowdingFactorsSet[0];
    }

    void setInvisibleGrowthFunction(std::string invisibleGrowthFunction) {
        this->invisibleGrowthFunction = invisibleGrowthFunction;
    }

    void setAcumulateFavorability(double acumulateFavorability) {
        this->acumulateFavorability = acumulateFavorability;
    }

    void setHostFactor(double hostFactor) {
        this->hostFactor = hostFactor;
    }

    void setCropModel(std::string cropModel) {
        this->cropModel = cropModel;
    }

    std::string getCropModel() const {
        return cropModel;
    }
    
    void setCardinalTempPhysiologicalLife(double cardinalTempPhysiologicalLife[]) {
        std::copy(cardinalTempPhysiologicalLife, cardinalTempPhysiologicalLife + 4, this->cardinalTempPhysiologicalLife);
    }

    double* getCardinalTempPhysiologicalLife() {
        return &cardinalTempPhysiologicalLife[0];
    }

    void setVisibleGrowthFunction(std::string visibleGrowthFunction) {
        this->visibleGrowthFunction = visibleGrowthFunction;
    }

    std::string getVisibleGrowthFunction() const {
        return visibleGrowthFunction;
    }
};

#endif // DISEASE_H
