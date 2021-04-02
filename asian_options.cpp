#include<iostream>
#include<cmath>
#include<ctime>
#include<fstream>
#include <numeric>
#include<vector>
using namespace std;
int START_s = clock();

///////////////////////////////////////////////////////////////////////////////////////////////////


double rand_n() //Box Muller Method from the Applied Computational Finance handout
{
    double mean = 0;
    double std = 1;
    static int iset = 0;
    static double gset;
    double fac, r,v1, v2;
    // create two normally-distributed numbers
    if (iset == 0)
    {
        r = 0;
        do
        { //compute two possibles
            v1 = 2.0 * rand()/RAND_MAX - 1.0;
            v2 = 2.0 * rand()/RAND_MAX - 1.0;
            // they define radius
            r = v1*v1 + v2*v2;
        }
        while (r >= 1.0 || r==0.0);
        // in unit circle? if not try again
        fac = sqrt((-2*log(r))/r); // Box-Muller transform
        gset = (v1 * fac) ;
        iset = 1; // save one
        v2 =v2*fac*std+mean; // scale and return one
        return v2;
    }
    else
    {
        iset = 0;
        return (gset*std)+mean;
        //scale and return the saved one
    }
}

//Alternative method to simulate random number attempted to reduce time of the simulations. However it was not able to optimise to give faster results.

////Generation Random Numbers with mean 0, variance 1
//double rand_n() {
//    
//    double r,v1,v2,fac;
//    
//    r=2;
//    while (r>=1) {
//        v1=(2*((double)rand()/(double)RAND_MAX)-1);
//        v2=(2*((double)rand()/(double)RAND_MAX)-1);
//        r=v1*v1+v2*v2;
//    }
//    fac=sqrt(-2*log(r)/r);
//    double x = v2 * fac;
//    return(x);
//    
//}


///////////////////////////////////////////////////////////////////////////////////////////////////



//Introducing a vector of a 'double' type, specifing its characteristics.
vector<double> Euler_Maruyama(double S0, double dt, double vol, double IR, double T, int RANDSEED)
{
    vector<double> SimulationPath;
    int i = 0;
    SimulationPath.push_back(S0); // uses vector library
    //from cpp reference: push_back Adds a new element at the end of the vector, after its current last element. The content of val is copied (or moved) to the new element.
    //This effectively increases the container size by one
    double St = S0;
    for (i = 1; i < T; i++)		//Defining 'for' loop conditions.
    {
        rand_n();
        //St = St + r*St*dt + vol*St*sqrt(dt)*rand_n() + 0.5*vol*vol*St*(dt*rand_n()*rand_n() - dt);
        St = St + IR*St*dt + vol*St*sqrt(dt)*rand_n();
        //St = St * exp((r-0.5*pow(vol,2)*dt+vol*sqrt(dt)*rnd));
        SimulationPath.push_back(St);
    }
    return SimulationPath;
}


//////////////////////////////////////////////////////////////////////////////////////////////////


double DiscretePayoff(vector<double> v)
{
    double AVERAGE = accumulate(v.begin(), v.end(), 0.0) / v.size();
    return AVERAGE;
}
double ContinousPayoff(vector<double> v)
{
    vector<double> logv = v;
    for (int i = 0; i < v.size(); i++)
    {
        logv[i] = log(v[i]);
    }
    double AVERAGE = accumulate(logv.begin(), logv.end(), 0.0) / v.size();
    return exp(AVERAGE);
}


///////////////////////////////////////////////////////////////////////////////////////////////////


double AsianOptionPricing(double S0, double E, double dt, double vol, double IR, int T, int SamplingSTEPSIZE, int NumberOfPaths, int type_declaration, int DiscreteContinous)
{
    double Price = 0.0;
    for (int i = 0; i < NumberOfPaths; i++)
    {
        vector<double> SimulationPath, SamplePath;
        SimulationPath = Euler_Maruyama(S0, dt, vol, IR, T, i);
        for (int j = 1; j < floor(T*1.0 / (SamplingSTEPSIZE*1.0)); j++)
            //The 'floor' rounds the number downwards to the closest integer value.
        {
            SamplePath.push_back(SimulationPath[j*SamplingSTEPSIZE]);
        }
        SamplePath.push_back(SimulationPath[T - 1]);
        double temporarypayoff = 0.0;
        if (DiscreteContinous == 0)
        {
            temporarypayoff = DiscretePayoff(SamplePath);
        }
        else
        {
            temporarypayoff = ContinousPayoff(SamplePath);
        }
        double ST;
        ST = SamplePath[SamplePath.size() - 1];
        switch (type_declaration)
        //'Switch function' aims to transfer the control (to switch)
        //to one of the below cases, depending on the value of a condition.
        {
                //Defining conditions depending on the type of the Asian Option which is priced.
            case 1: Price = (Price*i + exp(-IR*T*dt)*(temporarypayoff > E)*(temporarypayoff - E)) / ((i + 1)*1.0); break;
                // Condition for the Fixed Strike Asian Call Option.
            case 2: Price = (Price*i + exp(-IR*T*dt)*(temporarypayoff < E)*(E - temporarypayoff)) / ((i + 1)*1.0); break;
                // Condition for the Fixed Strike Asian Put Option.
            case 3: Price = (Price*i + exp(-IR*T*dt)*(ST > E*temporarypayoff)*(ST - E*temporarypayoff)) / ((i + 1)*1.0); break;
                // Condition for the Floating Strike Asian Call Option.
            case 4: Price = (Price*i + exp(-IR*T*dt)*(ST < E*temporarypayoff)*(E*temporarypayoff - ST)) / ((i + 1)*1.0); break;
                // Condition for the Floating Strike Asian Put Option.
        }
    }
    return Price;
}



///////////////////////////////////////////////////////////////////////////////////////////////////

int main(){
    int NumberOfSimulations = 100000; //Higher Iterations, more accuracy
    double S0 = 100.0; //Initial Stock Price
    double E = 100.0; //Strike Price of the option
    double vol = 0.2; //Fixed Volatility
    double IR = 0.05; //Fixed Interest Rate
    double T = 252; //252 Days where Market opens a year
    double dt = 1.0 / (T*1.0);
    srand(time(NULL));
    ofstream myfile;
    
    //////////////////////////////////////////////////////////////////////////////////////////////
    
    cout << "The following are the prices of Asian Options given their parameters:" << endl;
    cout << "The Parameters are in the following order" << endl;
    cout << "Continous or Discrete Sampling / Call or Put Option / Fixed or Floating Strike / Arithmetic or Geometric Sampling" << endl;

    //Average of 22 working days per month
    //This creates a lag when sampling
    
    
    //////////////////////////////////////////////////////////////////////////////////////////////
    
    
    int DailyMonth = 1; //Daily Continous Sampling = 1
    int type_declaration;
    int ArithmeticGeometric;
    double Result;
    //Note:
    // type_declaration 1 = Fixed Strike Asian Call
    // type_declaration 2 = Fixed Strike Asian Put
    // type_declaration 3 = Floating Strike Asian Call
    // type_declaration 4 = Floating Strike Asian Put
    
    //ArithmeticGeometric 0 = Arithmetic Sampling
    //ArithmeticGeometric 1 = Geometric Sampling
    
    //Call Options
    type_declaration = 1;
    ArithmeticGeometric = 0;
    Result = AsianOptionPricing(S0, E, dt, vol, IR, T, DailyMonth, NumberOfSimulations, type_declaration, ArithmeticGeometric);
    cout << "Continous/Call/Fixed/Arithmetic \t" << Result<< endl;
    type_declaration = 3;
    ArithmeticGeometric = 0;
    Result = AsianOptionPricing(S0, S0 / E, dt, vol, IR, T, DailyMonth, NumberOfSimulations, type_declaration, ArithmeticGeometric);
    cout << "Continous/Call/Floating/Arithmetic \t" << Result << endl;
    type_declaration = 1;
    ArithmeticGeometric = 1;
    Result = AsianOptionPricing(S0, E, dt, vol, IR, T, DailyMonth, NumberOfSimulations, type_declaration, ArithmeticGeometric);
    cout << "Continous/Call/Fixed/Geometric \t" << Result << endl;
    type_declaration = 3;
    ArithmeticGeometric = 1;
    Result = AsianOptionPricing(S0, S0 / E, dt, vol, IR, T, DailyMonth, NumberOfSimulations, type_declaration, ArithmeticGeometric);
    cout << "Continous/Call/Floating/Arithmetic \t" << Result << endl;
    //Put Options
    type_declaration = 2;
    ArithmeticGeometric = 0;
    Result = AsianOptionPricing(S0, E, dt, vol, IR, T, DailyMonth, NumberOfSimulations, type_declaration, ArithmeticGeometric);
    cout << "Continous/Put/Fixed/Arithmetic \t" << Result << endl;
    type_declaration = 4;
    ArithmeticGeometric = 0;
    Result = AsianOptionPricing(S0, S0 / E, dt, vol, IR, T, DailyMonth, NumberOfSimulations, type_declaration, ArithmeticGeometric);
    cout << "Continous/Put/Floating/Arithmetic \t" << Result << endl;
    type_declaration = 2;
    ArithmeticGeometric = 1;
    Result = AsianOptionPricing(S0, E, dt, vol, IR, T, DailyMonth, NumberOfSimulations, type_declaration, ArithmeticGeometric);
    cout << "Continous/Put/Fixed/Geometric \t" << Result << endl;
    type_declaration = 4;
    ArithmeticGeometric = 1;
    Result = AsianOptionPricing(S0, S0 / E, dt, vol, IR, T, DailyMonth, NumberOfSimulations, type_declaration, ArithmeticGeometric);
    cout << "Continous/Put/Floating/Arithmetic \t" << Result << endl;
    
//////////////////////////////////////////////////////////////////////////////////////////////////
    //Using Monthly lags (Discrete Sampling)
    DailyMonth = 22;
    //Call Options
    type_declaration = 1;
    ArithmeticGeometric = 0;
    Result = AsianOptionPricing(S0, E, dt, vol, IR, T, DailyMonth, NumberOfSimulations, type_declaration, ArithmeticGeometric);
    cout << "Discrete/Call/Fixed/Arithmetic \t" << Result<< endl;
    type_declaration = 3;
    ArithmeticGeometric = 0;
    Result = AsianOptionPricing(S0, S0 / E, dt, vol, IR, T, DailyMonth, NumberOfSimulations, type_declaration, ArithmeticGeometric);
    cout << "Discrete/Call/Floating/Arithmetic \t" << Result << endl;
    type_declaration = 1;
    ArithmeticGeometric = 1;
    Result = AsianOptionPricing(S0, E, dt, vol, IR, T, DailyMonth, NumberOfSimulations, type_declaration, ArithmeticGeometric);
    cout << "Discrete/Call/Fixed/Geometric \t" << Result << endl;
    type_declaration = 3;
    ArithmeticGeometric = 1;
    Result = AsianOptionPricing(S0, S0 / E, dt, vol, IR, T, DailyMonth, NumberOfSimulations, type_declaration, ArithmeticGeometric);
    cout << "Discrete/Call/Floating/Arithmetic \t" << Result << endl;
    //Put Options
    type_declaration = 2;
    ArithmeticGeometric = 0;
    Result = AsianOptionPricing(S0, E, dt, vol, IR, T, DailyMonth, NumberOfSimulations, type_declaration, ArithmeticGeometric);
    cout << "Discrete/Put/Fixed/Arithmetic \t" << Result << endl;
    type_declaration = 4;
    ArithmeticGeometric = 0;
    Result = AsianOptionPricing(S0, S0 / E, dt, vol, IR, T, DailyMonth, NumberOfSimulations, type_declaration, ArithmeticGeometric);
    cout << "Discrete/Put/Floating/Arithmetic \t" << Result << endl;
    type_declaration = 2;
    ArithmeticGeometric = 1;
    Result = AsianOptionPricing(S0, E, dt, vol, IR, T, DailyMonth, NumberOfSimulations, type_declaration, ArithmeticGeometric);
    cout << "Discrete/Put/Fixed/Geometric \t" << Result << endl;
    type_declaration = 4;
    ArithmeticGeometric = 1;
    Result = AsianOptionPricing(S0, S0 / E, dt, vol, IR, T, DailyMonth, NumberOfSimulations, type_declaration, ArithmeticGeometric);
    cout << "Discrete/Put/Floating/Arithmetic \t" << Result << endl;
    cout << "" << endl;

	int STOP_s = clock();
    double time_taken = (STOP_s - START_s) / double(CLOCKS_PER_SEC);
    cout << "" << endl;
    cout << "Time of simulation in seconds: " << time_taken << endl;

}
//
//
