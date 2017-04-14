#include <iostream>
#include <fstream>
using namespace std;


int main () {
	ofstream myfile;
	int y,x,k;
	cout<<"Check if working \n";
	myfile.open ("Level1.txt");
	for(y=100;y>=-100;y=y-1)
	{for(x=-200;x<=200;x=x+1)
	{
		k=0;
		
		if(y<=-50) k=1;
		if(y<=x && y>=-50 && y<=0 && x<=40 ) k=1;
		if( ( (x-20)*(x-20) + (y+5)*(y+5)<=(20*20) ) && y>=0 ) k=1;
		if((x+y)<=40 && y>=-50 && y<=0 && x>=0 ) k=1;		

		myfile << x << " " << y << " " << k << " ";
		
	}myfile <<"#\n";
	}
	myfile << "$\n";
	myfile.close();
  return 0;
}
