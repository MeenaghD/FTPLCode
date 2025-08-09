#include <iostream>
#include <fstream>
#include <string>
#include <ctime>
#include <cstdlib>

using namespace std;
double all_shocks[230][13];
double shocks_noncrisis[202][13];
double shocks_crisis[28][13];
int rnums_noncrisis[300];
int rnums_crisis[300];
int main ( int argc, char**argv ) {
    
ifstream infile("shocks.data",ios::in);
ofstream outfile1("bootshocks_noncrisis.data",ios::out);
ofstream outfile2("bootshocks_crisis.data",ios::out);

// Bootstrapping the crisis and non-crisis shocks seperately

srand(atoi(argv[1]));
for (int i=0; i<300; i++)
{
    rnums_noncrisis[i]=(rand()%202);
    rnums_crisis[i]=(rand()%28);
}

for(int ii=0; ii<230; ii++)
{
        for(int ij=0; ij<13; ij++)
        {
              infile >> all_shocks[ii][ij];
        }
}

// Shocks run from 1959.4-2017.1.
// ZLB period is 2009.1-2015.4, which is obs. 198-225
// Bootstrapping noncrisis period 1-197 and 226-230.
 for(int ii=0; ii<197; ii++)
   {
     for(int ij=0; ij<13; ij++)
       {
	 shocks_noncrisis[ii][ij]=all_shocks[ii][ij];
       }
   }

 for(int ii=197; ii<202; ii++)
   {
     for(int ij=0; ij<13; ij++)
       {
	 shocks_noncrisis[ii][ij]=all_shocks[ii+28][ij];
       }
   }

// Bootstrapping crisis period 198-225
 for(int ii=0; ii<28; ii++)
   {
     for(int ij=0; ij<13; ij++)
       {
	 shocks_crisis[ii][ij]=all_shocks[ii+197][ij];
       }
   }

for(int i=0; i<300; i++)
{
        for(int j=0; j<13; j++)
        {
         outfile1<<' '<<shocks_noncrisis[rnums_noncrisis[i]][j]; 
         outfile2<<' '<<shocks_crisis[rnums_crisis[i]][j]; 
         }
         outfile1<<endl;
         outfile2<<endl;
}

infile.close();
outfile1.close();
outfile2.close();
return 0;
}
