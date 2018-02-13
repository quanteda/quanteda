
#include "Rcpp.h"

/*
 * Detect if a box at position (x11,y11), with width sw11 and height sh11 overlaps
 * with any of the boxes in boxes1
 */
using namespace Rcpp;

RcppExport SEXP is_overlap(SEXP x11,SEXP y11,SEXP sw11,SEXP sh11,SEXP boxes1){
	double x1 = as<double>(x11);
	double y1 =as<double>(y11);
	double sw1 = as<double>(sw11);
	double sh1 = as<double>(sh11);
	Rcpp::List boxes(boxes1);
	Rcpp::NumericVector bnds;
	double x2, y2, sw2, sh2;
	bool overlap= true;
	for (int i=0;i < boxes.size();i++) {
		bnds = boxes(i);
		x2 = bnds(0);
		y2 = bnds(1);
		sw2 = bnds(2);
		sh2 = bnds(3);
		if (x1 < x2)
			overlap = (x1 + sw1) > x2;
		else
			overlap = (x2 + sw2) > x1;


		if (y1 < y2)
			overlap = (overlap && ((y1 + sh1) > y2));
		else
			overlap = (overlap && ((y2 + sh2) > y1));

		if(overlap)
			return Rcpp::wrap(true);
	}

	return Rcpp::wrap(false);
}
