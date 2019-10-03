rm *.o


gcc -c GenericPM/TinyExpr/tinyexpr.c -o tinyexpr.o
gcc -c GenericPM/include/cloud.cpp -o cloud.o -std=c++0x -std=gnu++0x
gcc -c GenericPM/include/cloudf.cpp -o cloudf.o -std=c++0x -std=gnu++0x
gcc -c GenericPM/include/cloudo.cpp -o cloudo.o -std=c++0x -std=gnu++0x
gcc  -c GenericPM/include/cloudp.cpp -o cloudp.o -std=c++0x -std=gnu++0x
gcc -c GenericPM/include/cropinterface.cpp -o cropinterface.o -std=c++0x -std=gnu++0x
gcc -c GenericPM/include/disease.cpp -o disease.o -std=c++0x -std=gnu++0x
gcc -c GenericPM/include/initialcondition.cpp -o initialcondition.o -std=c++0x -std=gnu++0x
gcc -c GenericPM/include/lesioncohort.cpp -o lesioncohort.o -std=c++0x -std=gnu++0x
gcc -c GenericPM/include/organ.cpp -o organ.o -std=c++0x -std=gnu++0x
gcc -c GenericPM/include/plant.cpp -o plant.o -std=c++0x -std=gnu++0x
gcc -c GenericPM/include/simulator.cpp -o simulator.o -std=c++0x -std=gnu++0x
gcc -c GenericPM/include/utilities.cpp -o utilities.o -std=c++0x -std=gnu++0x
gcc -c GenericPM/include/weather.cpp -o weather.o -std=c++0x -std=gnu++0x
gcc -c FlexibleIO/Interface/CInterface.cpp -o CInterface.o -std=c++0x -std=gnu++0x
gcc -fPIC -c FlexibleIO/Data/FlexibleIO.cpp -o FlexibleIO.o -std=c++0x -std=gnu++0x
gcc -fPIC -c FlexibleIO/Input/InputWeather.cpp -o InputWeather.o -std=c++0x -std=gnu++0x
gcc -fPIC -c FlexibleIO/Input/InputGenPest.cpp -o InputGenPest.o -std=c++0x -std=gnu++0x
gcc -fPIC -c FlexibleIO/Util/Util.cpp -o Util.o -std=c++0x -std=gnu++0x
gcc -fPIC -c GenericPM/cinterface.cpp -o cinterface.o -std=c++0x -std=gnu++0x


gcc *.o -o teste2 -lstdc++

mv *.o os
