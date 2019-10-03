#include <iostream>
#include "include/simulator.h"
#include <cmath>
#include <vector>
#include <fstream>
//#include "../FlexibleIO/Input/Input.hpp"
#include "../FlexibleIO/Data/FlexibleIO.hpp"
#include "../includes/json.hpp"

using json = nlohmann::json;

using namespace std;

double AREALF;
vector<double> outSpore;

extern "C"
{
    // Coupling Functions
    int couplingInit(int YRDOY, int YRPLT);
    int couplingRate(int YRDOY);
    double couplingIntegration(int YRDOY);
    int couplingOutput(int *doy);
    void inicia(string json, int *YRDOY, int *YRPLT, string pst, int *size, double *arf);
}

float CLWp, SLAp;
vector<int> d;
void inputJson2(std::string str, std::string pst)
{
    json jf = json::parse(str);
    json jp = json::parse(pst);
    FlexibleIO *flexibleio = FlexibleIO::getInstance();
    string j;
    for (int i = 0; i < jf.size(); i++)
    {
        std::ostringstream date, srad, tmax, tmin, rain, rh90;
        date << jf[i]["date"];
        string day = date.str();
        j = date.str();
        std::istringstream ss(j);
        int ax = jf[i]["date"];
        d.push_back(ax);

        srad << jf[i]["srad"];
        tmax << jf[i]["tmax"];
        tmin << jf[i]["tmin"];
        rain << jf[i]["rain"];
        rh90 << jf[i]["rh90"];

        flexibleio->setCharYrdoyMemory("WTH", date.str(), "SRAD", srad.str());
        flexibleio->setCharYrdoyMemory("WTH", date.str(), "TMAX", tmax.str());
        flexibleio->setCharYrdoyMemory("WTH", date.str(), "TMIN", tmin.str());
        flexibleio->setCharYrdoyMemory("WTH", date.str(), "RAIN", rain.str());
        flexibleio->setCharYrdoyMemory("WTH", date.str(), "RH90", rh90.str());
    }
    std::ostringstream var;
    flexibleio->setCharMemory("PST", "PESTID#", jp[0]["PESTID#"]);
    flexibleio->setCharMemory("PST", "PSTNAME", jp[0]["PSTNAME"]);

    var << jp[0]["DSPL"];
    flexibleio->setCharMemory("PST", "DSPL", var.str());

    for (int i = 0; i < jp[0]["SPE"].size(); i++)
    {
        var.str("");
        var.clear();
        var << jp[0]["SPE"][i];
        int index = 1 + i;
        flexibleio->setCharIndexMemory("PST", "SPE", var.str(), i + 1);
    }
    for (int i = 0; i < jp[0]["SCF"].size(); i++)
    {
        var.str("");
        var.clear();
        var << jp[0]["SCF"][i];
        int index = 1 + i;
        flexibleio->setCharIndexMemory("PST", "SCF", var.str(), i + 1);
    }
    var.str("");
    var.clear();
    var << jp[0]["MSCD"];
    flexibleio->setCharMemory("PST", "MSCD", var.str());

    //var.str("");
    //var.clear();
    //var << jp[0]["ASR"];
    //flexibleio->setCharMemory("PST", "ASR", var.str());

    var.str("");
    var.clear();
    var << jp[0]["SPO2P"];
    flexibleio->setCharMemory("PST", "SPO2P", var.str());

    var.str("");
    var.clear();
    var << jp[0]["SPP2F"];
    flexibleio->setCharMemory("PST", "SPP2F", var.str());

    for (int i = 0; i < jp[0]["CCFPO"].size(); i++)
    {
        var.str("");
        var.clear();
        var << jp[0]["CCFPO"][i];
        int index = 1 + i;
        flexibleio->setCharIndexMemory("PST", "CCFPO", var.str(), i + 1);
    }

    var.str("");
    var.clear();
    var << jp[0]["II"];
    flexibleio->setCharMemory("PST", "II", var.str());

    var.str("");
    var.clear();
    var << jp[0]["AFII"];
    flexibleio->setCharMemory("PST", "AFII", var.str());

    for (int i = 0; i < jp[0]["TFS"].size(); i++)
    {
        var.str("");
        var.clear();
        var << jp[0]["TFS"][i];
        int index = 1 + i;
        flexibleio->setCharIndexMemory("PST", "TFS", var.str(), i + 1);
    }

    var.str("");
    var.clear();
    var << jp[0]["IE"];
    flexibleio->setCharMemory("PST", "IE", var.str());

    var.str("");
    var.clear();
    var << jp[0]["DF"];
    flexibleio->setCharMemory("PST", "DF", var.str());

    var.str("");
    var.clear();
    var << jp[0]["IPS"];
    flexibleio->setCharMemory("PST", "IPS", var.str());

    var.str("");
    var.clear();
    var << jp[0]["LP"];
    flexibleio->setCharMemory("PST", "LP", var.str());

    var.str("");
    var.clear();
    var << jp[0]["IP"];
    flexibleio->setCharMemory("PST", "IP", var.str());

    var.str("");
    var.clear();
    var << jp[0]["WT"];
    flexibleio->setCharMemory("PST", "WT", var.str());

    var.str("");
    var.clear();
    var << jp[0]["HF"];
    flexibleio->setCharMemory("PST", "HF", var.str());

    flexibleio->setCharMemory("PST", "IGF", jp[0]["IGF"]);

    flexibleio->setCharMemory("PST", "VGF", jp[0]["VGF"]);
    //return 1;
}

void inicia(string json, int *YRDOY, int *YRPLT, string pst, int *size, double *arf)
{
    string s = json, p = pst;
    AREALF = *arf;
    FlexibleIO *flexibleio = FlexibleIO::getInstance();
    //json jf = json::parse(s);
    inputJson2(s, p);

    couplingInit(*YRDOY, *YRPLT);
    //Input::inputJson(s,p);
    std::cout << "CODIGO NOVO!\n";
    //std::vector<double> cloudf;
    double f;
    for (int i = 0; i < *size; i++)
    {
        int aux, *aux2;

        couplingRate(d[i]);
        //couplingIntegration(YRDOY);
        f = couplingIntegration(d[i]);
        //std::cout<<*YRDOY<<" CLOUD FIELD :: "<<f<<std::endl;
        //outCloudF[0][i] = f;
        cout<<f<<endl;
        outSpore.push_back(f);
        //std::cout<< outCloudF[0][i] <<std::endl;
        //*YRDOY+=1;
    }
}

// Coupling Functions Implementation

int couplingInit(int YRDOY, int YRPLT)
{
    // Get an instance of Simulator
    FlexibleIO *flexibleio = FlexibleIO::getInstance();
    Simulator *s = Simulator::newInstance();
    // Set the start day for Disease Model
    s->setCurrentYearDoy(YRDOY);
    // Set the sowing/planting date
    s->getCropInterface()->setPlantingDate(YRPLT);
    s->getCropInterface()->setOrganArea(1, AREALF);

    //    printf("YRDOY: %i YRPLT: %i\n",
    //            *YRDOY, *YRPLT);
    CLWp = 0;
    SLAp = 0;
    //s->output();
    return (1);
}

int couplingRate(int YRDOY)
{
    // Temporary variable used for computations
    float temp = 0, newOrgan = 0;
    // Get an instance of Simulator
    Simulator *s = Simulator::getInstance();
    newOrgan = s->getCropInterface()->getOrgansQtd() + 1;
    // Set the current YearDOY for next Disease step computation
    s->updateCurrentYearDoy(YRDOY);
    // Set the current Leaf area for a specific organ (one big leaf for awhile)
    // if(*CLW-CLWp > 0) {
    //     s->getCropInterface()->setOrganArea(newOrgan, ((*CLW-CLWp) * *SLA));
    // }
    // Set the daily senescence area
    // if(*SLDOT > 0) {
    //     s->getCropInterface()->setDailySenescenceArea(*SLDOT * *SLA);
    // }
    // CLWp = *CLW;
    //printf("CLW: %f SLDOT: %f SLA: %f AREALF: %f Calc: %f\n", *CLW,*SLDOT,*SLA,*AREALF,(*CLW * *SLA));

    // Set the daily senescence area for the organ (using SLA)
    //temp = s->getCropInterface()->getSenescenceOrganArea(1)+(*SLDOT * *SLA);
    //s->getCropInterface()->setSenescenceOrganArea(1, temp);
    // Disease Reduction Due Senescence/Defoliation (using SLA)

    /*temp = 1 - ((*SLDOT * *SLA) / s->getCropInterface()->getPreviewsOrganArea(1));
    //s->getCropInterface()->setRatioDueDefoliation(1, temp);
    printf("YRDOY: %i AREALF: %f SLDOT: %f SLA: %f Ratio: %f\n", *YRDOY,*AREALF,*SLDOT,*SLA,temp);
    printf("DAS: %i OrganArea: %f POrganArea: %f CLW: %f\n", *DAS,s->getCropInterface()->getOrganArea(1),s->getCropInterface()->getPreviewsOrganArea(1),*CLW);
    */

    // Feed the Disease Model with weather information
    Weather::getInstance()->update();
    // Disease Simulator Rate
    s->rate();
    //printf("Returning rate\n");
    //s->output();
    return (1);
}

double couplingIntegration(int YRDOY)
{
    // Temporary variable used for computations
    float dArea = 0, tArea = 0, pDArea = 0, sArea = 0, pclaCalc = 0;
    double cloudField = 0;

    // Get an instance of Simulator
    Simulator *s = Simulator::getInstance();

    // Call the Disease Model Integration function
    s->integration();

    if (s->getPlants().size() > 0 && s->getPlants()[0].getOrgans().size() > 0)
    {
        dArea = s->getPlants()[0].getDiseaseArea();
        tArea = s->getPlants()[0].getTotalArea();
        sArea = s->getPlants()[0].getSenescenceArea();
        pDArea = (dArea / (tArea - sArea) * 100);
        //printf("YRDOY: %i Plant Total Area: %f Disease Area: %f Senescence Area: %f AREALF: %f pDArea: %f\n", *YRDOY, tArea,dArea,sArea,*AREALF,pDArea);
        if (pDArea > 99.9)
        {
            pDArea = 99.9;
        }
        //*PDLA = pDArea;
        for (int i = 0; i < s->getPlants()[0].getOrgans().size(); i++)
        {
            if (s->getPlants()[0].getOrgans().at(i).getVisibleLesions() /
                    (s->getPlants()[0].getOrgans().at(i).getTotalArea() - s->getPlants()[0].getOrgans().at(i).getSenescenceArea()) >=
                10)
            {
                pclaCalc += fmax(0, s->getPlants()[0].getOrgans().at(i).getTotalArea() - s->getPlants()[0].getOrgans().at(i).getSenescenceArea());
            }
        }
        cloudField = s->getPlants()[0].getCloudsP()[0].getCloudF()->getValue();
        printf("pclaCalc: %f def: %f\n", pclaCalc, pclaCalc / tArea * 100);
        //*PLFAD =  pclaCalc/tArea*100;
    }

    //printf("YRDOY: %i PCLMT: %f\n", *YRDOY,*PCLMT);
    //printf("Returning integration\n");
    //s->output();
    return (cloudField);
}
int couplingOutput(int *doy)
{
    // Get an instance of Simulator
    Simulator *s = Simulator::getInstance();
    // Request disease outputs to be written in files
    s->output();

    return (1);
}
int main(int argc, char *argv[])
{
    cout << "estou vivo!!!!" << endl;
    ifstream infileW("dataJson.json");
    ifstream infileP("pst.json");

    string weather,pst,aux;
    while (std::getline(infileW, aux)){
        cout<<weather<<endl;
         weather = aux;
    }
    while (std::getline(infileP, aux)){
        pst = aux;
    }


    /*
    2018337
    2018337
    91
    100000
*/
    int yrdoy,yrplt,size;
    double arf;
    int *yrd,*yplt,*zs;
    double *area;
    yrdoy = strtol(argv[1], NULL, 10);
    yrplt = strtol(argv[2], NULL, 10);
    size = strtol(argv[3], NULL, 10);
    arf =100000;

    yrd  = &yrdoy;
    yplt = &yrplt;
    zs   = &size;
    area = &arf;

    inicia(weather,yrd,yplt, pst,zs,area);
    
    ofstream ofs ("spore.out", ofstream::out | ofstream::trunc);

    ofs <<"sporeCloud\n";
        for(int i = 0; i < outSpore.size(); i++)
    {
     ofs <<outSpore[i]<<endl;   
    }

    //void inicia(string json, int *YRDOY, int *YRPLT, string pst, int *size, double *arf);

}