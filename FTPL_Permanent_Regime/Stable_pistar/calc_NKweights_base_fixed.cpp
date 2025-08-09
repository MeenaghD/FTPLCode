#include <iostream>
#include <fstream>
#include <string>
#include <ctime>
#include <cstdlib>
#include <cmath>

using namespace std;
double act_data[234][18];
double MA_pi[234];
double NKwage[234];
double NKprice[234];
double exp(double x);
double coef[28];
double abs(double x);
double sum;
double sum1;

int main ( int argc, char**argv ) {
    
ifstream infile1("act_data.data",ios::in);
ifstream infile2("coef.data",ios::in);
ofstream outfile1("NKweights.data",ios::out);

 outfile1.precision(10);

 /* for(int i1=0; i1<234; i1++)
   {
     for(int i2=0; i2<16; i2++)
       {
	 infile1 >> act_data[i1][i2];
       }
   }


 for(int ii=0; ii<28; ii++)
   {
     infile2 >> coef[ii];
   }
 */
 for(int i3=0; i3<234; i3++)
   {
     /*     if(i3<16)
       {
	 for(int i4=i3; i4<=i3; i4++)
	   {
	     sum +=act_data[i4][4]*act_data[i4][4];
	   }
	 MA_pi[i3]=sum/(i3+1);
       }
     else
       {
	     sum1=0;
	 for(int i5=i3-15; i5<=i3; i5++)
	   {
	     sum1 += act_data[i5][4]*act_data[i5][4];
	   }
	 MA_pi[i3]=sum1/(16);
       }	   
     NKwage[i3]=exp(-coef[21]*MA_pi[i3]);
     NKprice[i3]=exp(-coef[22]*MA_pi[i3]);
     outfile1 << NKwage[i3] << ' ' << NKprice[i3] << endl;
     */
     //     outfile1 << 0.8 << ' ' << 0.8 << endl;
     // using the weight given from average inflation of 0.806505
     outfile1 << 0.954715527 << ' ' << 0.966767477 << endl;
   }



infile1.close();
infile2.close();
outfile1.close();

return 0;
}
