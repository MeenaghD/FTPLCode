#include <iostream>
#include <fstream>
#include <string>
#include <ctime>
#include <cstdlib>

using namespace std;
double zint_rate;
double zint_rate_base;
double zint_rate_diff;
int rsign;
int current_period;

int main ( int argc, char**argv ) {
    
ifstream infile1("current_r",ios::in);
ifstream infile2("current_r_base",ios::in);
ifstream infile3("current_period",ios::in);
ofstream outfile("r_sign",ios::out);

 infile1 >> zint_rate;
 infile2 >> zint_rate_base;
 infile3 >> current_period;
 //zint_rate_diff=zint_rate-zint_rate_base-0.0172*current_period+1.802573;
 //zint_rate_diff=zint_rate-zint_rate_base+0.0;
 zint_rate_diff=zint_rate;
 if ( zint_rate_diff < 0.0625 )
   {
     rsign = 1;
   }

 outfile << rsign;

infile1.close();
infile2.close();
outfile.close();
return 0;
}
