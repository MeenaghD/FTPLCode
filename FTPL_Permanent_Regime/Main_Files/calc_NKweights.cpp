#include <iostream>
#include <fstream>
#include <string>
#include <ctime>
#include <cstdlib>
#include <cmath>

using namespace std;
double lagged_pi[234];
double lagged_pi_base[234];
double NKwage;
double NKprice;
//double NKwage_lag;
//double NKprice_lag;
double NKwage_base;
double NKprice_base;
double CalvoP;
double CalvoW;
double exp(double x);
double coef[28];
double abs(double x);
double ave_NCpi;
double sum;
double sum1;
int nlag;

int main ( int argc, char**argv ) {
    
ifstream infile1("lagged_pi",ios::in);
ifstream infile6("lagged_pi_base",ios::in);
ifstream infile2("coef.data",ios::in);
ifstream infile3("nlag",ios::in);
ifstream infile4("NKwage_coef_base",ios::out);
ifstream infile5("NKprice_coef_base",ios::out);
//ifstream infile7("NKwage_lag",ios::out);
//ifstream infile8("NKprice_lag",ios::out);
ofstream outfile1("new_coef.data",ios::out);
ofstream outfile2("MA_pi",ios::out);
ofstream outfile3("NKwage_coef",ios::out);
ofstream outfile4("NKprice_coef",ios::out);
//ofstream outfile5("NKwage_lag_new",ios::out);
//ofstream outfile6("NKprice_lag_new",ios::out);

 outfile1.precision(10);
 outfile2.precision(10);

 infile3 >> nlag;
 for(int ii=0; ii<28; ii++)
   {
     infile2 >> coef[ii];
   }

 for(int ij=0; ij<nlag; ij++)
   {
     infile1 >> lagged_pi[ij];
   }
 for(int ij=0; ij<nlag; ij++)
   {
     infile6 >> lagged_pi_base[ij];
   }

  infile4 >> NKwage_base;
  infile5 >> NKprice_base;
  //  infile7 >> NKwage_lag;
  //  infile8 >> NKprice_lag;
  //  cout << NKwage_lag;

 /* for(int i0=0; i0<nlag; i0++)
   {
     if(lagged_pi[i0]>3.0)
       {
	 lagged_pi[i0]=3.0;
       }
     else if (lagged_pi[i0]<-3.0)
       {
	 lagged_pi[i0]=-3.0;
       }
   }
 */ 
 for(int i1=0; i1<nlag; i1++)
   {
    if (i1<16)
    {
      for (int i2=i1; i2<=i1; i2++)
	{
	            sum += lagged_pi[i2]*lagged_pi[i2];
	  //          sum += std::abs(lagged_pi[i2]-lagged_pi_base[i2]);
          //sum += (lagged_pi[i2]-lagged_pi_base[i2])*(lagged_pi[i2]-lagged_pi_base[i2]);
        }
     ave_NCpi=sum/(i1+1);
   }

   else
   {
     sum1=0;
     for(int i3=i1-15; i3<=i1; i3++)
       {
	 	  sum1 += lagged_pi[i3]*lagged_pi[i3];
	 //	 sum1 += std::abs(lagged_pi[i3]-lagged_pi_base[i3]);
	 //sum1 += (lagged_pi[i3]-lagged_pi_base[i3])*(lagged_pi[i3]-lagged_pi_base[i3]);
       }
     ave_NCpi=sum1/16;
   }
  if (ave_NCpi>9.0)
   {
     ave_NCpi=9.0;
   }
}

 outfile2 << ave_NCpi << endl;

 NKwage=exp(-coef[21]*std::abs(ave_NCpi));
 NKprice=exp(-coef[22]*std::abs(ave_NCpi));

 if (NKwage<0.1)
   {
     NKwage=0.1;
   }
 if (NKprice<0.1)
   {
     NKprice=0.1;
   }
 if (NKwage>0.999)
   {
     NKwage=0.999;
   }
 if (NKprice>0.999)
   {
     NKprice=0.999;
   }
 
   if (NKwage-NKwage_base < -0.2)
   {
     NKwage=NKwage_base-0.2;
   }
 if (NKprice-NKprice_base < -0.2)
   {
     NKprice=NKprice_base-0.2;
   }


 // cout << NKwage << endl << NKwage_lag << endl;
 /*
   if (NKwage-NKwage_lag < -0.01)
   {
     NKwage=NKwage_lag-0.01;
   }
 if (NKprice-NKprice_lag < -0.01)
   {
     NKprice=NKprice_lag-0.01;
   }
*/
 // cout << NKwage;
 coef[15]=NKwage;
 coef[16]=NKprice;

 for(int ii=0; ii<28; ii++)
   {
     outfile1 << coef[ii] << endl;
   }

 outfile3 << NKwage << endl;
 outfile4 << NKprice << endl;
 // outfile5 << NKwage << endl;
 // outfile6 << NKprice << endl;

infile1.close();
infile2.close();
infile3.close();
infile4.close();
infile5.close();
infile6.close();
//infile7.close();
//infile8.close();
outfile1.close();
outfile2.close();
outfile3.close();
outfile4.close();
//outfile5.close();
//outfile6.close();
return 0;
}
