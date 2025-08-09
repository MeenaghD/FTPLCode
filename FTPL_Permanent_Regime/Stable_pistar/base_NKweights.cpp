#include <iostream>
#include <fstream>
#include <string>
#include <ctime>
#include <cstdlib>
#include <cmath>

using namespace std;
double NKwage[234];
double NKprice[234];
double coef[28];
int nlag;

int main ( int argc, char**argv ) {
    
ifstream infile1("NKweights.data",ios::in);
ifstream infile2("coef.data",ios::in);
ifstream infile3("nlag",ios::in);
ofstream outfile1("new_coef.data",ios::out);
ofstream outfile3("NKwage_coef",ios::out);
ofstream outfile4("NKprice_coef",ios::out);
ofstream outfile5("NKwage_coef_base",ios::out);
ofstream outfile6("NKprice_coef_base",ios::out);

 outfile1.precision(10);

 infile3 >> nlag;
 for(int i1=0; i1<234; i1++)
   {
     infile1 >> NKwage[i1] >> NKprice[i1];
   }

 for(int i2=0; i2<28; i2++)
   {
     infile2 >> coef[i2];
   }

 coef[15]=NKwage[nlag-1];
 coef[16]=NKprice[nlag-1];

 for(int ii=0; ii<28; ii++)
   {
     outfile1 << coef[ii] << endl;
   }

 outfile3 << coef[15] << endl;
 outfile4 << coef[16] << endl;
 outfile5 << coef[15] << endl;
 outfile6 << coef[16] << endl;

infile1.close();
infile2.close();
infile3.close();
outfile1.close();
return 0;
}
