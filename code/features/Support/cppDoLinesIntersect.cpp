#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

// Functions required to check whether two segments intersect

// [[Rcpp::export]]
NumericVector getBoundingBox(NumericVector P0, NumericVector P1) {
  // P0, P1 each have c(x,y)
  // ll = lower left
  // ur = upper right
  double llx = std::min(P0[0], P1[0]);
  double lly = std::min(P0[1], P1[1]);
  double urx = std::max(P0[0], P1[0]);
  double ury = std::max(P0[1], P1[1]);
  
  NumericVector bb = NumericVector::create(llx, lly, urx, ury);
  
  return bb;
}

// [[Rcpp::export]]
bool doBoxesIntersect(NumericVector box1, NumericVector box2) {
  
  bool chk1 = box1[0] <= box2[2];
  bool chk2 = box1[2] >= box2[0]; 
  bool chk3 = box1[1] <= box2[3];
  bool chk4 = box1[3] >= box2[1];
  bool ans = chk1 & chk2 & chk3 & chk4;
  return ans;
}

// [[Rcpp::export]]
bool isPointOnLine(NumericVector segP0, NumericVector segP1, NumericVector point) {
  // segp0 and segP1 are the two coordinates (x, y) of a segment.
  // translate segment to origin
  NumericVector newsegP1 = segP1 - segP0;
  NumericVector newpt = point - segP0;
  
  // calc a modified cross product:
  // a.x * b.y - b.x * a.y
  // if zero, point is on segment
  // basically, you have two vectors sharing 0,0 as one end
  double cp = newsegP1[0]*newpt[1] - newpt[0]*newsegP1[1];
  return cp == 0;
}


// [[Rcpp::export]]
bool isPointRightOfLine(NumericVector segP0, NumericVector segP1, NumericVector point) {
  // see notes in isPointOnLine
  NumericVector newsegP1(2);
  newsegP1 = segP1 - segP0;
  
  NumericVector newpt(2);
  newpt = point - segP0;
  
  double cp = newsegP1[0]*newpt[1] - newpt[0]*newsegP1[1];
  return cp < 0;
}

// [[Rcpp::export]]
bool lineSegmentTouchesOrCrossesLine(NumericVector seg1P0, NumericVector seg1P1,
                                     NumericVector seg2P0, NumericVector seg2P1) {
  // each consists of (x, y) coordinates of one of the endpoints of the segment
  
  bool ans = isPointOnLine(seg1P0, seg1P1, seg2P0) ||
    isPointOnLine(seg1P0, seg1P1, seg2P1) ||
    (isPointRightOfLine(seg1P0, seg1P1, seg2P0) ^ // ^ = xor
    isPointRightOfLine(seg1P0, seg1P1, seg2P1)) ;
  return ans;
}

// [[Rcpp::export]]
bool doSegmentsIntersect (NumericVector seg1P0, NumericVector seg1P1,
                          NumericVector seg2P0, NumericVector seg2P1) {
  // each consists of (x, y) coordinates of one of the endpoints of the segment
  NumericVector box1 = getBoundingBox(seg1P0, seg1P1);
  NumericVector box2 = getBoundingBox(seg2P0, seg2P1);
  bool ans = doBoxesIntersect(box1, box2) &&
    lineSegmentTouchesOrCrossesLine(seg1P0, seg1P1, seg2P0, seg2P1) &&
    lineSegmentTouchesOrCrossesLine(seg2P0, seg2P1, seg1P0, seg1P1);
  return ans;
}



// [[Rcpp::export]]
List anyIntersects(NumericVector posX, NumericVector posY,
                   NumericVector time, double lag){
  
  List res = List::create(NumericVector::create(0, 0));
  
  int nSteps = posX.size();
  
  double fTime = time(time.length() - 1) - lag;
  
  int fStep = 0;
  while(time(fStep) < fTime){
    fStep++;
  }
  
  fStep = fStep - 1;
  
  for(int j = 0; j < fStep; ++j ){
    
    int k = j + 2; 
    while(time(k) < time(j) + lag){
      ++k;
    }
    NumericVector seg1P0(2), seg1P1(2), seg2P0(2), seg2P1(2);
    
    seg1P0(0) = posX(j);
    seg1P0(1) = posY(j);
    seg1P1(0) = posX(j + 1);
    seg1P1(1) = posY(j + 1);
    
    for(int l = k; l < nSteps - 2; ++l){
      seg2P0(0) = posX(l);
      seg2P0(1) = posY(l);
      seg2P1(0) = posX(l + 1);
      seg2P1(1) = posY(l + 1);
      
      if(doSegmentsIntersect(seg1P0, seg1P1, seg2P0, seg2P1)){
        res.push_back(NumericVector::create(j + 1, l + 1));
      }
    }
  }
  
  return res;
}


