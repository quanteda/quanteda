#include "Rcpp.h"
using namespace Rcpp;

/*
 * Detect if a box at position (x1_,y1_), with width w1_ and height h1_ overlaps
 * with any of the boxes in boxe_
 */

// [[Rcpp::export]]
bool qatd_cpp_is_overlap(SEXP x1_, SEXP y1_, SEXP w1_, SEXP h1_, SEXP boxe_) {
    
	double x1 = as<double>(x1_);
	double y1 =as<double>(y1_);
	double sw1 = as<double>(w1_);
	double sh1 = as<double>(h1_);
	Rcpp::List boxes(boxe_);
	Rcpp::NumericVector box_;
	double x2, y2, w2, h2;
	bool overlap= true;
	for (int i = 0; i < boxes.size(); i++) {
		box_ = boxes[i];
		x2 = box_[0];
		y2 = box_[1];
		w2 = box_[2];
		h2 = box_[3];
		if (x1 < x2) {
			overlap = (x1 + sw1) > x2;
		} else {
			overlap = (x2 + w2) > x1;
		}
		if (y1 < y2) {
			overlap = (overlap && ((y1 + sh1) > y2));
		} else {
			overlap = (overlap && ((y2 + h2) > y1));
		}
		if(overlap)
			return true;
	}
	return false;
}
