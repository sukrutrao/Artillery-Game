#include <iostream>
#include <fstream>
using namespace std;


int main () {
	ofstream myfile;
	int y,x,k,t;
	t = 0;
	cout<<"Check if working \n";
	myfile.open ("Level1.txt");
	for(y=100;y>=-100;y=y-1)
	{for(x=-200;x<=200;x=x+1,t++)
	{
		k=0;
		
		if(y<=-50) k=1;
		if(y<=x && y>=-50 && y<=0 && x<=40 ) k=1;
		if( ( (x-20)*(x-20) + (y+5)*(y+5)<=(20*20) ) && y>=0 ) k=1;
		if((x+y)<=40 && y>=-50 && y<=0 && x>=0 ) k=1;
		
		/*
		if((-90*y) <= (-47*x) -15145) k=1;
		if(-35*y <= -53*x -22730) k=1; 
		if(-65*y <= -32*x + ( -12320 )) k=1;
		if(-73*y <= 34*x + ( 13090 )) k=1;
		if(-27*y <= 51*x + ( 16830 )) k=1;
		if(-45*y <= 3*x + ( 4680 )) k=1;
		if(-101*y <= -51*x + ( -3352 )) k=1;
		if(-67*y <= -116*x + ( -13645 )) k=1;
		if(-72*y <= -36*x + ( -8280 )) k=1;
		if(-69*y <= 32*x + ( -7935 )) k=1;
		if(-78*y <= 138*x + ( -15996 )) k=1;
		if(-76*y <= 37*x + ( -1259 )) k=1;
		if(-55*y <= 0*x + ( 5060 )) k=1;
		if(-114*y <= -58*x + ( 26612 )) k=1;
		if(-70*y <= 34*x + ( -10948 )) k=1;
		if(-38*y <= 69*x + ( -29294 )) k=1;
		if(-70*y <= 33*x + ( -6910 )) k=1;
		if(-30*y <= 0*x + ( 5100 )) k=1;	
		*/	
		
		//if(t%3 == 0)
		myfile << x/3 << " " << y/3 << " " << k/3 << " ";
		
	}myfile <<"#\n";
	}
	myfile << "$\n";
	myfile.close();
  return 0;
}
