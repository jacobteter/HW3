#include <stdio.h>
#include "Move.h"
#include <R.h>

#define BLUE 1 // ones move up
#define RED 2 // twos move right

int rows, cols;     
int *map;           

/* more efficient would be to rewrite BLUE / RED / EMPTY cars as char datatypes, as this would take 8 bits rather than 32 bits */

int getMap(int r, int c);
void setMap(int r, int c, int v);
void moveRow(int r);
void moveCol(int c);
void move(int *map, int *time, int *row, int *col);
void printMap(int * map, int row, int col);
int rm(int x, int y);
  
inline int getMap(int r, int c){  //"inline" avoids function call by compiler inserting code in calling function. see: http://en.wikipedia.org/wiki/Inline_function
  return *(map+rows*c+r);
}

inline void setMap(int r, int c, int v){
  *(map+rows*c+r)=v;
}

void moveRow(int r){ /*moves all unblocked red cars on row r*/
  int bc,c;
  for(bc=0;bc<cols;bc++)
    if(getMap(r,bc)==0)
      break;    // drop out of for loop
  if(bc==cols)
    return;  // the row has no blanks else bc is a blank cell
  for(c=1;c<cols;c++){  //process cols-1 cells starting at bc
    if(getMap(r,rm(bc-1,cols))==RED && getMap(r,bc)==0){
      setMap(r,bc,RED);
      setMap(r,rm(bc-1,cols),0);
    }
    bc=rm(bc-1,cols);   
  }                                  
}  

void moveCol(int c){
  int br,r;
  for(br=0;br<rows;br++)
    if(getMap(br,c)==0)
      break;
  if(br==rows)
    return;      // the column has no blanks else row br is a blank cell 
  for(r=1;r<rows;r++){    // process rows-1 cells starting at br
    if(getMap((br+1)%rows,c)==BLUE && getMap(br,c)==0){
      setMap(br,c,BLUE);
      setMap((br+1)%rows,c,0);
    }
    br=(br+1)%rows;
  }
}  

void move(int * m, int *time, int *row, int *col){
  int t,r,c;
  extern int rows, cols;   //extern means identifiers are globals
  extern int *map;
  rows=*row;
  cols=*col;
  map=m;
  for(t=1; t<*time+1; t++)
    if (t%2==0)
      for(r=0; r<*row; r++)
        moveRow(r);      
    else
      for(c=0; c<*col; c++)
  	moveCol(c);
}   

void printMap(int * map, int row, int col){
  int r,c;
  for(r=0; r<row; r++){
    for(c=0; c<col; c++)
      Rprintf("%d", *(map+row*c+r));
    Rprintf("\n");
  }
  Rprintf("\n");
}

inline int rm(int x, int y){  /* returns remainder of x divided by y, y>0 */
  while(x<0)
    x+=y; // this keeps adding y to x, while in the while loop
  return x%y;
}


