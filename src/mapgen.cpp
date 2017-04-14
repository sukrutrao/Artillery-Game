#include <iostream>
#include <fstream>
using namespace std;

int main () {
	ofstream myfile;
	int y,x,k;
	cout<<"Check if working \n";
	myfile.open ("Level1.txt");
	for(y=350;y>=-350;y=y-1)
	{for(x=-600;x<=600;x=x+1)
	{
		k=0;
		
		if(y<=-175) k = 1;
		if(y<=x && y>=-175 && y<=0 && x<=240 ) k=1;
		if( ( (x-120)*(x-120) + (y+5)*(y+5)<=(20*20) ) && y>=0 ) k=1;
		if((x+y)<=40 && y>=-50 && y<=0 && x>=0 ) k=1;		

		myfile << x << " " << y << " " << k << " ";
		
	}myfile <<"#\n";
	}
	myfile << "$\n";
	myfile.close();
  return 0;
}
