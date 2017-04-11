#include <iostream>
#include <time.h>
#include <stdlib.h>


using namespace std;


double func(double a)
{
    if(a < 0.0000000001) return (-1)*a;
    else return  a;
}
int main (int argc, char const* argv[])
{

    int x = -100 , y = 100 , op = -30 , lo = 30;    
    srand(time(NULL));
    cout<<"[";
     while(y>=-100)
    {
        x = -100;
        cout<<"[";
        while(x<=100)
        {
            if(-40 > y && y >= -50)
            {

                if(x>op && x < lo)
                {
                    cout << "Tile{tilePosition=(Position("<<(x/100.0)<<") ("<<(y/100.0)<<")),isObstacle=True},";
                }
                else
                {
                    if (x == 100 )
                        cout << "Tile{tilePosition=(Position("<<(x/100.0)<<") ("<<(y/100.0)<<")),isObstacle=False}";
                    else 
                        cout << "Tile{tilePosition=(Position("<<(x/100.0)<<") ("<<(y/100.0)<<")),isObstacle=False},";
                }
            }
            else if(y < -50)    
            {
                if (x == 100 )
                    cout << "Tile{tilePosition=(Position("<<(x/100.0)<<") ("<<(y/100.0)<<")),isObstacle=True}";
                else
                    cout << "Tile{tilePosition=(Position("<<(x/100.0)<<") ("<<(y/100.0)<<")),isObstacle=True},";
            }
            /*else if(y > 50)    
            {
                if (x == 100 )
                    cout << "Tile{tilePosition=(Position("<<(x/100.0)<<") ("<<(y/100.0)<<")),isObstacle=True}";
                else
                    cout << "Tile{tilePosition=(Position("<<(x/100.0)<<") ("<<(y/100.0)<<")),isObstacle=True},";
            }*/
            else
            {
               if (x == 100 )
                    cout << "Tile{tilePosition=(Position("<<(x/100.0)<<") ("<<(y/100.0)<<")),isObstacle=False}";
                else
                    cout << "Tile{tilePosition=(Position("<<(x/100.0)<<") ("<<(y/100.0)<<")),isObstacle=False},";      
            }

            x += 1;
        }
        
        if(-40 > y && y > -50)
        {
            lo+=1;
            op-=1;
            
        }
                    
       if (y == -100 )
            cout<<"]";
        else
           cout<<"],";
        y -= 1;
    }
    cout<<"]";
    return 0;
}

