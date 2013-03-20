#include <stdio.h>
#include "Move.h"
#include <R.h>

#define BLUE 1 // ones move up
#define RED 2 // twos move right

/* more efficient would be to rewrite BLUE / RED / EMPTY cars as char datatypes */

int fullRow(int *map, int r, int row, int col);
int fullCol(int *map, int c, int row, int col);
void moveRow(int *map, int r, int row, int col);
void moveCol(int *map, int c, int row, int col);
void move(int *map, int *time, int *row, int *col);
void printMap(int * map, int row, int col);
int rm(int x, int y);
  
int fullRow(int *map, int r, int row, int col){ /*returns 1 if all cells on row r have cars*/
  int c;
  for(c=0; c<col; c++)
    if(*((map+row*c+r)) == 0)
      return 0;             /* in C, 0 and only zero, is interpreted as false */
    return 1;                 /* true */
}

int fullCol(int * map, int c, int row, int col){ /*returns 1 if all cells on col c have cars*/
  int r;
  for(r=0; r<row; r++)
    if(*((map+row*c+r)) == 0)
      return 0;
  return 1;
}

void 
moveRow(int * map, int r, int row, int col){ /*moves all unblocked red cars on row r*/
  int cc,c=0; // c is the offset from the beginning of the row...(map + row*c+r)
  if(fullRow(map, r, row, col))  /*no blanks*/
    return;
  while(c<col){
    while(c<col && (*(map+row*c+r)==0 || *(map+row*c+r)==BLUE)) // the star in front dereferences the location its looking at. 
      c++;                            /* pass over blanks and blue cars*/
    if(c==col)
      return;                         /*no more red cars*/
        /*map[r][c] is a red car*/
    cc=c;                    /*find next non-red cell*/ // c has been marked as the first red car encountered
    while(*(map+row*cc+r)==RED){ // this one's hard...
      cc=(cc+1)%col; // at the end of this loop, cc is the cell ahead of (with wrap around) the front of a limo of red cars
      if(cc==0 && *(map+r)!=0) //red caravan at end of row can't move
	return;
  
    }
    if(*(map+row*cc+r)==0){ /*cars from c to (cc-1)%col can move*/
      *(map+row*c+r)=0;
      *(map+row*cc+r)=RED;
      if(cc==0)
	return;
    }
    /* else cars from c to cc are blocked*/
    c=cc+1;
//    Rprintf("***** %d     %d \n",r,c);
  }                                                
}  

void 
moveCol(int * map, int c, int row, int col){
  int r=row-1;
  int rr;
  if(fullCol(map, c, row, col))  /*no blanks*/ 
    return;
  while(r>-1){
    while(r>-1 && (*(map + row*c+r)==0 || *(map + row*c+r)==RED))
      r--;                            /* pass over blanks and red cars*/
    if(r==-1)
      return;                         /*no more blue cars*/
      /*map[r][c] is a blue car*/
    rr=r;                    /*find next non-blue cell*/
    while(*(map+row*c+rr)==BLUE){
      if(rr==0 && *(map+row*c+row-1)!=0)  /*blue caravan at top of column can't move*/
	return; 
      rr=rm(rr-1,row); /* negative mod, instead use the increment function */
    }
    if(*(map+row*c+rr)==0){ /*cars from r to rr can move*/
      *(map+row*c+r)=0;
      *(map+row*c+rr)=BLUE;
    }
    /* else cars from r to rr are blocked*/
    r=rr-1;
//    Rprintf("***** %d     %d \n",r,c);      
  }
}  

void move(int * map, int *time, int *row, int *col){
  int t,r,c;
  for(t=1; t<*time+1; t++)
    if (t%2==0)
      for(r=0; r<*row; r++)
        moveRow(map,r,*row,*col);      
    else
      for(c=0; c<*col; c++)
  	moveCol(map,c,*row,*col);
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

int rm(int x, int y){  /* returns remainder of x divided by y, y>0 */
  while(x<0)
    x+=y; // this keeps adding y to x, while in the while loop
  return x%y;
}


