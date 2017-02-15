
// includes from the plugin
#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;
# define RESIDUALS_LIM 0.5
# define NUMSVD 2

#if !defined(ARMA_64BIT_WORD)
#define ARMA_64BIT_WORD
#endif



// [[Rcpp::export]]

Rcpp::List wordfish_cpp(arma::sp_mat &wfm, SEXP dir, SEXP priors, SEXP tol, SEXP disp, SEXP dispfloor){
    
    // DEFINE INPUTS
    
    //Rcpp::NumericMatrix Y(wfm); 
    Rcpp::NumericVector priorvec(priors);
    Rcpp::NumericVector tolvec(tol); 
    Rcpp::IntegerVector dirvec(dir);  
    Rcpp::IntegerVector disptype(disp);
    Rcpp::NumericVector dispmin(dispfloor);
    
    double priorprecalpha = priorvec(0);
    double priorprecpsi = priorvec(1);
    double priorprecbeta = priorvec(2);
    double priorprectheta = priorvec(3);		
    
    int N = wfm.n_rows;
    int K = wfm.n_cols;
    
    // SET INITIAL VALUES
    
    arma::colvec alpha(N); 
    arma::rowvec psi(K); 
    arma::rowvec beta(K); 
    arma::colvec theta(N); 
    
    arma::colvec thetaSE(N); // document position standard errors
    arma::rowvec phi(K); // word-level dispersion parameters
    phi.fill(1.0);
    // Construct Chi-Sq Residuals	
    arma::sp_mat C(wfm); 
    arma::colvec rsum(sum(C,1));
    arma::rowvec csum(sum(C,0));
    double asum = accu(C);		
    for (int i=0; i < N; i++){
        for (int k=0; k < K; k++){
            double residual = (wfm(i,k) - rsum(i) * csum(k) / asum) / sqrt(rsum(i) * csum(k) / asum);	
            //Rprintf("%d: %f2\\n",k,residual);
            if (fabs(residual) > RESIDUALS_LIM) C(i,k) = residual;
        }
    }
    
    // Singular Value Decomposition of Chi-Sq Residuals
    const int svdk = NUMSVD;
    arma::mat U(N, svdk);
    arma::vec s(svdk);
    arma::mat V(K, svdk);
    arma::svds(U,s,V,C, svdk);
    
    // Load initial values
    for (int i=0; i < N; i++) theta(i) = pow(rsum(i)/asum, -0.5) * U(i, 0);
    //for (int k=0; k < K; k++) beta(k) = 0; // pow(csum(k)/asum,-0.5) * V(k,0);
    beta.fill(0.0);
    alpha = log(rsum);
    psi = log(csum/N);
    
    alpha = alpha - log(mean(rsum));
    theta = (theta - mean(theta)) / stddev(theta);  
    
    // Create temporary variables
    arma::mat pars(2,1);
    arma::mat newpars(2,1);		
    arma::mat G(2,1);
    arma::mat H(2,2);
    double loglambdaik;
    double mutmp;		
    double phitmp;
    arma::rowvec lambdai(K);
    arma::colvec lambdak(N);
    double stepsize = 1.0;
    double cc = 0.0;
    int inneriter = 0;
    int outeriter = 0;
    
    double lastlp = -2000000000000.0;
    double lp = -1.0*(accu(0.5 * ((alpha % alpha) * priorprecalpha)) + accu(0.5 * ((psi  % psi) * priorprecpsi)) 
                          + accu(0.5 * ((beta  % beta) * priorprecbeta)) + accu(0.5 * ((theta % theta) * priorprectheta)));
    for (int i=0; i < N; i++){
        for (int k=0; k < K; k++){
            loglambdaik = alpha(i) + psi(k) + beta(k)*theta(i);
            lp = lp + loglambdaik * wfm(i,k)-exp(loglambdaik);
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
                lambdak = exp(alpha + psi(k) + beta(k) * theta);
                arma::vec col_k(wfm.col(k));
                G(0,0) = accu(col_k - lambdak) / phi(k) - psi(k) * priorprecpsi;
                G(1,0) = accu(theta % (col_k - lambdak)) / phi(k) - beta(k) * priorprecbeta;
                H(0,0) = -accu(lambdak) / phi(k) - priorprecpsi;
                H(1,0) = -accu(theta % lambdak) / phi(k);
                H(0,1) = H(1,0);
                H(1,1) = -accu((theta % theta) % lambdak) / phi(k) - priorprecbeta;
                pars(0,0) = psi(k);
                pars(1,0) = beta(k);
                newpars(0,0) = pars(0,0) - stepsize * (H(1,1) * G(0,0) - H(0,1) * G(1,0)) / (H(0,0) * H(1,1) - H(0,1) * H(1,0));
                newpars(1,0) = pars(1,0) - stepsize * (H(0,0) * G(1,0) - H(1,0) * G(0,0)) / (H(0,0) * H(1,1) - H(0,1) * H(1,0));
                psi(k) = newpars(0,0);
                beta(k) = newpars(1,0);
                cc = abs(newpars - pars).max();
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
                lambdai = exp(alpha(i) + psi + beta * theta(i));
                arma::rowvec row_i(wfm.row(i));
                G(0,0) = accu((row_i - lambdai) / phi) - alpha(i) * priorprecalpha;
                G(1,0) = accu((beta % (row_i - lambdai)) / phi) - theta(i) * priorprectheta;		
                H(0,0) = -accu(lambdai / phi) - priorprecalpha;
                H(1,0) = -accu((beta % lambdai) / phi);
                H(0,1) = H(1,0);
                H(1,1) = -accu(((beta % beta) % lambdai) / phi) - priorprectheta;
                pars(0,0) = alpha(i);
                pars(1,0) = theta(i);
                newpars(0,0) = pars(0,0) - stepsize*(H(1,1)*G(0,0) - H(0,1)*G(1,0))/(H(0,0)*H(1,1) - H(0,1)*H(1,0));
                newpars(1,0) = pars(1,0) - stepsize*(H(0,0)*G(1,0) - H(1,0)*G(0,0))/(H(0,0)*H(1,1) - H(0,1)*H(1,0));
                alpha(i) = newpars(0,0);
                theta(i) = newpars(1,0);
                cc = abs(newpars - pars).max();	
                stepsize = 1.0;
            }	
        }
        
        // UPDATE DISPERSION PARAMETERS	  
        
        if (disptype(0) == 2) { // single dispersion parameter for all words
            phitmp = 0.0;
            for (int k=0; k < K; k++){
                for (int i=0; i < N; i++){
                    mutmp = exp(alpha(i) + psi(k) + beta(k)*theta(i));
                    phitmp = phitmp + (wfm(i,k) - mutmp) * (wfm(i,k) - mutmp)/mutmp;
                }   
            }	    
            phitmp = phitmp/(N*K - 2*N - 2*K);
            for (int k=0; k < K; k++) phi(k) = phitmp;
        }
        
        if (disptype(0) >= 3) { // individual dispersion parameter for each word
            for (int k=0; k < K; k++){
                phitmp = 0.0;
                for (int i=0; i < N; i++){
                    mutmp = exp(alpha(i) + psi(k) + beta(k) * theta(i));
                    phitmp = phitmp + (wfm(i,k) - mutmp) * (wfm(i,k) - mutmp)/mutmp;	        
                }   
                phitmp = ((K) * phitmp)/(N * K - 2 * N - 2 * K);
                phi(k) = phitmp;
                // set ceiling on underdispersion
                if (disptype(0) == 4) phi(k) = fmax(dispmin(0), phi(k));
            }
        }	  			
        
        alpha = alpha - mean(alpha);
        theta = (theta - mean(theta))/stddev(theta);		
        
        // CHECK LOG-POSTERIOR FOR CONVERGENCE
        lastlp = lp;
        lp = -1.0*(accu(0.5 * ((alpha % alpha) * priorprecalpha)) + accu(0.5 * ((psi % psi) * priorprecpsi)) 
                       + accu(0.5 * ((beta % beta) * priorprecbeta)) + accu(0.5 * ((theta % theta) * priorprectheta)));
        for (int i=0; i < N; i++){
            for (int k=0; k < K; k++){
                loglambdaik = alpha(i) + psi(k) + beta(k) * theta(i);
                lp = lp + loglambdaik * wfm(i,k) - exp(loglambdaik);
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
    
    // COMPUTE DOCUMENT STANDARD ERRORS
    for (int i=0; i < N; i++) {
        lambdai = exp(alpha(i) + psi + beta * theta(i));
        H(0,0) = -accu(lambdai / phi) - priorprecalpha;
        H(1,0) = -accu((beta % lambdai) / phi);
        H(0,1) = H(1,0);
        H(1,1) = -sum(((beta % beta) % lambdai) / phi) - priorprectheta;
        thetaSE(i) = sqrt(-1.0 * H(0,0) / (H(0,0) * H(1,1)-H(1,0) * H(0,1)));
    }  
    
    // DEFINE OUTPUT	
    return Rcpp::List::create(Rcpp::Named("theta") = theta,
                              Rcpp::Named("alpha") = alpha,
                              Rcpp::Named("psi") = psi,
                              Rcpp::Named("beta") = beta,
                              Rcpp::Named("phi") = phi,
                              Rcpp::Named("thetaSE") = thetaSE);
    
}
