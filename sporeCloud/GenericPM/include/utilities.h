#ifndef UTILITIES_H
#define UTILITIES_H

#include<string>
#include "../TinyExpr/tinyexpr.h"

class Utilities {
public:

    static double trapezoidalFunction(double value, double v[]);
    static double temperatureFavorability(double temp, double cardinalTemperatures[]);
    static double wetnessFavorability(double wetDuration);
    static double getHealthAreaProportion(double diseaseArea, double totalArea, double senescedArea);
    static std::string formatDouble(double value);
    static std::string formatDouble(double value, int decimals);
    static int addOneDay(int yearDoy);
    static bool isLeapYear(int year);

    static double growthFunction(std::string expression_string, double value) {
        te_variable vars[] = {{"x", &value}};
        int err;
        te_expr *expr = te_compile(expression_string.c_str(), vars, 1, &err);
        return te_eval(expr);
    }

};

#endif // UTILITIES_H
