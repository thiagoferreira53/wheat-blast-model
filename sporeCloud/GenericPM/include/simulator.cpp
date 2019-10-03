#include "simulator.h"
#include "disease.h"
#include "cropinterface.h"
#include "initialcondition.h"
#include "weather.h"
#include "../../FlexibleIO/Data/FlexibleIO.hpp"

#include<sstream>
#include<vector>
#include<iostream>
#include<string>


Simulator::Simulator() {
    inicialization();
}

Simulator* Simulator::instance = nullptr;

Simulator* Simulator::getInstance() {
    if (instance == nullptr)
        instance = new Simulator();
    return instance;
}

Simulator* Simulator::newInstance() {
    instance=nullptr;
    return getInstance();
}

void Simulator::inicialization() {
    cropinterface = CropInterface::newInstance();
    cropinterface->start();
    inputPST();

    std::vector<Disease*> &diseases = Disease::getDisease();
    for (unsigned int i = 0; i < diseases.size(); i++)
        initialConditions.emplace_back(diseases[i]);
}

void Simulator::inputPST() {
  
  if (Disease::getDisease().size() == 0) {
      Disease *disease = new Disease();
      FlexibleIO *flexibleio = FlexibleIO::getInstance();
      
      std::string str;
      float f; 
      double arraysize3[3], arraysize4[4];
      
      str = flexibleio->getChar("PST", "PESTID#");
      disease->setId(std::stoi(str.substr(2, str.size()),nullptr,0));
      
      disease->setDescription(flexibleio->getChar("PST", "PSTNAME"));
      
      disease->setDailySporeProductionPerLesion((double)flexibleio->getReal("PST", "DSPL"));
      
      f = flexibleio->getRealIndex("PST", "SPE", 1);
      arraysize4[0] = (double) f;
      f = flexibleio->getRealIndex("PST", "SPE", 2);
      arraysize4[1] = (double) f;
      f = flexibleio->getRealIndex("PST", "SPE", 3);
      arraysize4[2] = (double) f;
      f = flexibleio->getRealIndex("PST", "SPE", 4);
      arraysize4[3] = (double) f;
      disease->setCohortAgeSet(arraysize4);
      
      f = flexibleio->getRealIndex("PST", "SCF", 1);
      arraysize3[0] = (double) f;
      f = flexibleio->getRealIndex("PST", "SCF", 2);
      arraysize3[1] = (double) f;
      f = flexibleio->getRealIndex("PST", "SCF", 3);
      arraysize3[2] = (double) f;
      disease->setSporulationCrowdingFactorsSet(arraysize3);
      
      disease->setMaxSporeCloudsDensity((double) flexibleio->getReal("PST", "MSCD"));
      
      disease->setProportionFromOrganToPlantCloud((double) flexibleio->getReal("PST", "SPO2P"));
      
      disease->setProportionFromPlantToFieldCloud((double) flexibleio->getReal("PST", "SPP2F"));
      
      disease->setVectorSizeCloudF(flexibleio->getIntegerIndex("PST", "CCFPO", 1));
      
      disease->setVectorSizeCloudP(flexibleio->getIntegerIndex("PST", "CCFPO", 2));
      
      disease->setVectorSizeCloudO(flexibleio->getIntegerIndex("PST", "CCFPO", 3));
      
      disease->setInitialInoculum((double) flexibleio->getReal("PST", "II"));
      
      disease->setAcumulateFavorability((double) flexibleio->getReal("PST", "AFII"));    
      
      f = flexibleio->getRealIndex("PST", "TFS", 1);
      arraysize3[0] = (double) f;
      f = flexibleio->getRealIndex("PST", "TFS", 2);
      arraysize3[1] = (double) f;
      f = flexibleio->getRealIndex("PST", "TFS", 3);
      arraysize3[2] = (double) f;
      disease->setTemperatureFavorabilitySet(arraysize3);
      
      disease->setInfectionEfficiency((double) flexibleio->getReal("PST", "IE"));
      
      disease->setDispersionFreequency((double) flexibleio->getReal("PST", "DF"));
      
      disease->setInitialPustuleSize((double) flexibleio->getReal("PST", "IPS"));
      
      disease->setLatentPeriod(flexibleio->getInteger("PST", "LP"));
      
      disease->setInfectionPeriod(flexibleio->getInteger("PST", "IP"));
      
      disease->setWetnessThreshold((double) flexibleio->getReal("PST", "WT"));
      
      disease->setHostFactor((double) flexibleio->getReal("PST", "HF"));
      
      disease->setInvisibleGrowthFunction(flexibleio->getChar("PST", "IGF"));
      
      disease->setVisibleGrowthFunction(flexibleio->getChar("PST", "VGF"));
  }
  
}

void Simulator::integration() {
    InitialCondition *ic;
    for (unsigned int i = 0; i < initialConditions.size(); i++) {
        ic = &initialConditions[i];
        ic->integration();
    }
    Plant *p;
    for (unsigned int i = 0; i < plants.size(); i++) {
        p = &plants[i];
        p->integration();
    }
}

void Simulator::output() {
    InitialCondition *ic;
    for (unsigned int i = 0; i < initialConditions.size(); i++) {
        ic = &initialConditions[i];
        ic->output();
    }
    Plant *p;
    for (unsigned int i = 0; i < plants.size(); i++) {
        p = &plants[i];
        p->output();
    }
}
/**
 * Function Rate: Responsible call, recursively, the rates for each part of the plant
 */
void Simulator::rate() {
    InitialCondition *ic;
    Plant *p;

    /** If Planting Date is the current day, instantiate the Plant */
    if (CropInterface::getInstance()->getPlantingDate() == getCurrentYearDoy()) {
        printf("NEW Plant: PlantingDate: %i - CurrentYearDoy: %i \n",CropInterface::getInstance()->getPlantingDate(), getCurrentYearDoy());
        plants.emplace_back();
    }    
    /** For each Initial Condition call the rate function */
    for (unsigned int i = 0; i < initialConditions.size(); i++) {
        ic = &initialConditions[i];
        ic->rate();
    }
    /** For each Plant, call the rate function */
    for (unsigned int i = 0; i < plants.size(); i++) {
        p = &plants[i];
        p->rate();
    }
}

void Simulator::updateCurrentYearDoy(int yearDoy) {
    while(util.addOneDay(getCurrentYearDoy()) < yearDoy) { // Need to be synchronized. There is a gap.
//        printf("Synchronizing: YearDoy: %i - CurrentYearDoy: %i \n",yearDoy, getCurrentYearDoy());
        setCurrentYearDoy(util.addOneDay(getCurrentYearDoy()));
        Weather::getInstance()->update();
        rate();
        integration();
    }
    setCurrentYearDoy(yearDoy);
}

bool Simulator::allPlantsSenesced() {
    if (plants.size() == 0)
        return false;
    for (unsigned int i = 0; i < plants.size(); i++) {
        Plant *p = &plants[i];
        if (p->isAlive())
            return false;
    }
    return true;
}


