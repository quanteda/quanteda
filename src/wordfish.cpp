
// includes from the plugin
#include <RcppArmadillo.h>

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]

Rcpp::List wordfishcpp(SEXP wfm, SEXP dir, SEXP priors, SEXP tol){

	// DEFINE INPUTS
	
		Rcpp::NumericMatrix Y(wfm); 
		Rcpp::NumericVector priorvec(priors);
		Rcpp::NumericVector tolvec(tol); 
  	Rcpp::IntegerVector dirvec(dir);     
		
		double priorprecalpha = priorvec(0);
		double priorprecpsi = priorvec(1);
		double priorprecbeta = priorvec(2);
		double priorprectheta = priorvec(3);		
		
		int N = Y.nrow();
		int K = Y.ncol();

	// SET INITIAL VALUES
	
		Rcpp::NumericVector alpha(N); 
		Rcpp::NumericVector psi(K); 
		Rcpp::NumericVector beta(K); 
		Rcpp::NumericVector theta(N); 
		
		// Construct Chi-Sq Residuals	
		arma::mat C(Y.begin(),N,K); 
		arma::colvec rsum = sum(C,1);
		arma::rowvec csum = sum(C,0);
		double asum = sum(rsum);		
		for (int i=0; i < N; i++){
			for (int k=0; k < K; k++){
				C(i,k) = (Y(i,k) - rsum(i)*csum(k)/asum)/sqrt(rsum(i)*csum(k)/asum);	
			}
		}
		
		// Singular Value Decomposition of Chi-Sq Residuals
		arma::mat U(N,N);
		arma::vec s(N);
		arma::mat V(K,N);
		svd(U,s,V,C);
	
		// Load initial values
		for (int i=0; i < N; i++) theta(i) = pow(rsum(i)/asum,-0.5) * U(i,0);
		for (int k=0; k < K; k++) beta(k) = 0; // pow(csum(k)/asum,-0.5) * V(k,0);
		for (int i=0; i < N; i++) alpha = log(rsum);
		psi = log(csum/N);
		
		alpha = alpha - log(mean(rsum));
		theta = (theta - mean(theta))/sd(theta);  
		
		// Create temporary variables
		Rcpp::NumericMatrix pars(2,1);
		Rcpp::NumericMatrix newpars(2,1);		
		Rcpp::NumericMatrix G(2,1);
		Rcpp::NumericMatrix H(2,2);
		double loglambdaik;
		Rcpp::NumericVector lambdai(K);
		Rcpp::NumericVector lambdak(N);
		double stepsize = 1.0;
		double cc = 0.0;
		int inneriter = 0;
		int outeriter = 0;
		
		double lastlp = -2000000000000.0;
		double lp = -1.0*(sum(0.5 * ((alpha*alpha)*(priorprecalpha))) + sum(0.5 * ((psi*psi)*(priorprecpsi))) + sum(0.5 * ((beta*beta)*(priorprecbeta))) + sum(0.5 * ((theta*theta)*(priorprectheta))));
			for (int i=0; i < N; i++){
				for (int k=0; k < K; k++){
					loglambdaik = alpha(i) + psi(k) + beta(k)*theta(i);
					lp = lp + loglambdaik*Y(i,k)-exp(loglambdaik);
				}
			}
	
	// BEGIN WHILE LOOP
	while(((lp - lastlp) > tolvec(0)) && outeriter < 100){	
		outeriter++;
	
		// UPDATE WORD PARAMETERS
			for (int k=0; k < K; k++){
				cc = 1;
				inneriter = 0;
				if (outeriter == 1) stepsize = 0.5;
				while ((cc > tolvec(1)) && inneriter < 10){
					inneriter++;
					lambdak = exp(alpha + psi(k) + beta(k)*theta);
					G(0,0) = sum(Y(_,k) - lambdak) - psi(k)*(priorprecpsi);
					G(1,0) = sum(theta*(Y(_,k) - lambdak)) - beta(k)*(priorprecbeta);
					H(0,0) = -sum(lambdak) - priorprecpsi;
					H(1,0) = -sum(theta*lambdak);
					H(0,1) = H(1,0);
					H(1,1) = -sum((theta*theta)*lambdak) - priorprecbeta;
					pars(0,0) = psi(k);
					pars(1,0) = beta(k);
					newpars(0,0) = pars(0,0) - stepsize*(H(1,1)*G(0,0) - H(0,1)*G(1,0))/(H(0,0)*H(1,1) - H(0,1)*H(1,0));
					newpars(1,0) = pars(1,0) - stepsize*(H(0,0)*G(1,0) - H(1,0)*G(0,0))/(H(0,0)*H(1,1) - H(0,1)*H(1,0));
					psi(k) = newpars(0,0);
					beta(k) = newpars(1,0);
					cc = max(abs(newpars - pars));
					stepsize = 1.0;
				}	
			}	

		
		// UPDATE DOCUMENT PARAMETERS
			for (int i=0; i < N; i++){
				cc = 1;
				inneriter = 0;
				if (outeriter == 1) stepsize = 0.5;
				while ((cc > tolvec(1)) && inneriter < 10){
					inneriter++;
					lambdai = exp(alpha(i) + psi + beta*theta(i));
					G(0,0) = sum(Y(i,_) - lambdai) - alpha(i)*priorprecalpha;
					G(1,0) = sum(beta*(Y(i,_) - lambdai)) - theta(i)*priorprectheta;		
					H(0,0) = -sum(lambdai) - priorprecalpha;
					H(1,0) = -sum(beta*lambdai);
					H(0,1) = H(1,0);
					H(1,1) = -sum((beta* beta)*lambdai) - priorprectheta;
					pars(0,0) = alpha(i);
					pars(1,0) = theta(i);
					newpars(0,0) = pars(0,0) - stepsize*(H(1,1)*G(0,0) - H(0,1)*G(1,0))/(H(0,0)*H(1,1) - H(0,1)*H(1,0));
					newpars(1,0) = pars(1,0) - stepsize*(H(0,0)*G(1,0) - H(1,0)*G(0,0))/(H(0,0)*H(1,1) - H(0,1)*H(1,0));
					alpha(i) = newpars(0,0);
					theta(i) = newpars(1,0);
					cc = max(abs(newpars - pars));	
					stepsize = 1.0;
				}	
			}
		
		alpha = alpha - mean(alpha);
		theta = (theta - mean(theta))/sd(theta);		
		
		// CHECK LOG-POSTERIOR FOR CONVERGENCE
			lastlp = lp;
			lp = -1.0*(sum(0.5 * ((alpha*alpha)*(priorprecalpha))) + sum(0.5 * ((psi*psi)*(priorprecpsi))) + sum(0.5 * ((beta*beta)*(priorprecbeta))) + sum(0.5 * ((theta*theta)*(priorprectheta))));
			for (int i=0; i < N; i++){
				for (int k=0; k < K; k++){
					loglambdaik = alpha(i) + psi(k) + beta(k)*theta(i);
					lp = lp + loglambdaik*Y(i,k)-exp(loglambdaik);
				}
			}
			// Rprintf("%d: %f2\\n",outeriter,lp);

	// END WHILE LOOP		
	} 
  
      
  // Fix Global Polarity  
  
  // added the -1 because C counts from ZERO...  -- KB
  if (theta(dirvec(0)-1) > theta(dirvec(1)-1)) {
    beta = -beta;
    theta = -theta;
  }
		
	// DEFINE OUTPUT	
	
	return Rcpp::List::create(Rcpp::Named("theta") = theta,
                          Rcpp::Named("alpha") = alpha,
                          Rcpp::Named("psi") = psi,
                          Rcpp::Named("beta") = beta);

}



