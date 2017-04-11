#include <iostream>
#include <fstream>
using namespace std;

int main () {
	ofstream myfile;
	int y,x,k;
	cout<<"Check if working \n";
	myfile.open ("Level1.txt");
	for(y=100;y>=-100;y=y-2)
	for(x=-100;x<=100;x=x+2)
	{
		k=0;
		if(y<=-50)
		k=1;
		if(y<=x && y>=-50 && y<=0)
		k=1;
		if( ( (x-20)*(x-20) + y*y<=(20*20) ) && y>=0 )
		k=1;
		if((x+y)<=40 && y>=-50 && y<=0 )
		k=1;		

		myfile << x << " " << y << " " << k << "\n";
		
	}
	myfile << "$\n";
	myfile.close();
  return 0;
}
