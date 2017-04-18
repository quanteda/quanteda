#include <RcppArmadillo.h>
#include "quanteda.h"
using namespace quanteda;
//#include <ctime>
using namespace RcppParallel;
using namespace Rcpp;
using namespace arma;

//# define RESIDUALS_LIM 0.5
# define NUMSVD 1
# define OUTERITER 100
# define INNERITER 10
# define LASTLP -2000000000000.0

#if !defined(ARMA_64BIT_WORD)
#define ARMA_64BIT_WORD
#endif


//find the principle elements for the sparse residual matrix
void create_residual(const std::size_t row_num, const arma::sp_mat& wfm, const arma::colvec &rsum, const arma::rowvec &csum, const double &asum,
                const double residual_floor, const std::size_t K, Triplets &residual_tri)
{
    for (std::size_t k = 0; k < K; k++){
        double residual = (wfm(row_num, k) - rsum(row_num) * csum(k) / asum) / sqrt(rsum(row_num) * csum(k) / asum);
        if (fabs(residual) > residual_floor) {
            Triplet mat_triplet = std::make_tuple(row_num, k, residual);
            residual_tri.push_back(mat_triplet);
        }
    }
}
//Create the residual matrix
struct Residual : public Worker {
    const arma::sp_mat& wfm;
    const arma::colvec &rsum;
    const arma::rowvec &csum;
    const double asum;
    const double residual_floor;
    const std::size_t K;
    
    // output: residual[index0, index1, residual_value]
    Triplets &residual_tri;
    
    //constructor
    Residual(const arma::sp_mat& wfm, const arma::colvec &rsum, const arma::rowvec &csum, const double asum,
             const double residual_floor, const std::size_t K, Triplets &residual_tri):
        wfm(wfm), rsum(rsum), csum(csum), asum(asum), residual_floor(residual_floor), K(K), residual_tri(residual_tri) {}

    
    void operator() (std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; i++) {
            create_residual(i, wfm, rsum, csum, asum, residual_floor, K, residual_tri);
        }
    }
};
//Compute LOG-POSTERIOR 
struct LogPos : public Worker {
    const arma::colvec& alpha; 
    const arma::rowvec& psi;
    const arma::rowvec& beta;
    const arma::colvec& theta;
    const arma::sp_mat& wfm;
    const std::size_t K;
    
    // accumulated value
    double lp;
    
    // constructors
    LogPos(const arma::colvec& alpha, const arma::rowvec& psi, const arma::rowvec& beta, 
           const arma::colvec& theta, const arma::sp_mat& wfm, const std::size_t K) 
        : alpha(alpha), psi(psi), beta(beta), theta(theta), wfm(wfm), K(K), lp(0) {}
    LogPos(const LogPos& sum, Split) : alpha(sum.alpha), psi(sum.psi), beta(sum.beta),
    theta(sum.theta), wfm(sum.wfm), K(sum.K), lp(0) {}
    
    
    void operator() (std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; i++) {
            double temp = lp;
            for (std::size_t k = 0; k < K; k++){
                double loglambdaik = alpha(i) + psi(k) + beta(k) * theta(i);
                temp += loglambdaik * wfm(i,k) - exp(loglambdaik);
            }
            lp = temp;
        }
    }
    
    // join lp with that of another LogPos
    void join(const LogPos& rhs) { 
        lp += rhs.lp; 
    }
};

// UPDATE WORD PARAMETERS
struct WordPar : public Worker {
    const arma::colvec& alpha; 
    RVector<double> psi; 
    RVector<double> beta; 
    const arma::colvec& theta; 
    const arma::rowvec& phi;
    const arma::sp_mat& wfm;
    const NumericVector& tolvec;
    const int outeriter;
    const double priorprecpsi;
    const double priorprecbeta;
    double stepsize;
    
    WordPar(const arma::colvec& alpha,  NumericVector& psi,  NumericVector& beta, 
            const arma::colvec& theta, const arma::rowvec& phi, const arma::sp_mat& wfm,
            const NumericVector& tolvec, const int outeriter, 
            const double priorprecpsi, const double priorprecbeta, double stepsize) 
        : alpha(alpha), psi(psi), beta(beta), theta(theta), phi(phi), wfm(wfm), tolvec(tolvec), 
          outeriter(outeriter), priorprecpsi(priorprecpsi), priorprecbeta(priorprecbeta), stepsize(stepsize) {}
    
    void operator() (std::size_t begin, std::size_t end) {
        for (std::size_t k = begin; k < end; k++) {
            double cc = 1;
            int inneriter = 0;
            if (outeriter == 1) stepsize = 0.5;
            arma::colvec col_k(wfm.col(k));
            arma::colvec lambdak(alpha.size());
            arma::mat pars(2,1);
            arma::mat newpars(2,1);
            arma::mat G(2,1);
            arma::mat H(2,2);
            while ((cc > tolvec(1)) && inneriter < INNERITER){
                inneriter++;
                lambdak = exp(alpha + psi[k] + beta[k] * theta);
                G(0,0) = accu(col_k - lambdak) / phi(k) - psi[k] * priorprecpsi;
                G(1,0) = accu(theta % (col_k - lambdak)) / phi(k) - beta[k] * priorprecbeta;
                H(0,0) = -accu(lambdak) / phi(k) - priorprecpsi;
                H(1,0) = -accu(theta % lambdak) / phi(k);
                H(0,1) = H(1,0);
                H(1,1) = -accu((theta % theta) % lambdak) / phi(k) - priorprecbeta;
                pars(0,0) = psi[k];
                pars(1,0) = beta[k];
                newpars(0,0) = pars(0,0) - stepsize * (H(1,1) * G(0,0) - H(0,1) * G(1,0)) / (H(0,0) * H(1,1) - H(0,1) * H(1,0));
                newpars(1,0) = pars(1,0) - stepsize * (H(0,0) * G(1,0) - H(1,0) * G(0,0)) / (H(0,0) * H(1,1) - H(0,1) * H(1,0));
                psi[k] = newpars(0,0);
                beta[k] = newpars(1,0);
                cc = abs(newpars - pars).max();
                stepsize = 1.0;
            }
        }
    } //End of operator
};

// UPDATE DOCUMENT PARAMETERS
struct DocPar : public Worker {
    RVector<double> alpha; 
    const arma::rowvec& psi;
    const arma::rowvec& beta; 
    RVector<double> theta; 
    const arma::rowvec& phi;
    const arma::sp_mat& wfm;
    const NumericVector& tolvec;
    const int outeriter;
    const double priorprecalpha;
    const double priorprectheta;
    double stepsize;
    
    DocPar(NumericVector& alpha,  const arma::rowvec& psi,  const arma::rowvec& beta, 
           NumericVector& theta, const arma::rowvec& phi, const arma::sp_mat& wfm,
            const NumericVector& tolvec, const int outeriter, 
            const double priorprecalpha, const double priorprectheta, double stepsize) 
        : alpha(alpha), psi(psi), beta(beta), theta(theta), phi(phi), wfm(wfm), tolvec(tolvec), 
          outeriter(outeriter), priorprecalpha(priorprecalpha), priorprectheta(priorprectheta), stepsize(stepsize) {}
    
    void operator() (std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; i++) {
            double cc = 1;
            int inneriter = 0;
            if (outeriter == 1) stepsize = 0.5;
            arma::rowvec row_i(wfm.row(i));
            arma::rowvec lambdai(alpha.size());
            arma::mat pars(2,1);
            arma::mat newpars(2,1);
            arma::mat G(2,1);
            arma::mat H(2,2);
            while ((cc > tolvec(1)) && inneriter < INNERITER){
                inneriter++;
                lambdai = exp(alpha[i] + psi + beta * theta[i]);
                G(0,0) = accu((row_i - lambdai) / phi) - alpha[i] * priorprecalpha;
                G(1,0) = accu((beta % (row_i - lambdai)) / phi) - theta[i] * priorprectheta;		
                H(0,0) = -accu(lambdai / phi) - priorprecalpha;
                H(1,0) = -accu((beta % lambdai) / phi);
                H(0,1) = H(1,0);
                H(1,1) = -accu(((beta % beta) % lambdai) / phi) - priorprectheta;
                pars(0,0) = alpha[i];
                pars(1,0) = theta[i];
                newpars(0,0) = pars(0,0) - stepsize*(H(1,1)*G(0,0) - H(0,1)*G(1,0))/(H(0,0)*H(1,1) - H(0,1)*H(1,0));
                newpars(1,0) = pars(1,0) - stepsize*(H(0,0)*G(1,0) - H(1,0)*G(0,0))/(H(0,0)*H(1,1) - H(0,1)*H(1,0));
                alpha[i] = newpars(0,0);
                theta[i] = newpars(1,0);
                cc = abs(newpars - pars).max();	
                stepsize = 1.0;
            }
        }
    } //End of operator
};

// Update dispersion parameters -- single dispersion parameter for all words
struct DispPar : public Worker {
    const arma::colvec& alpha; 
    const arma::rowvec& psi;
    const arma::rowvec& beta;
    const arma::colvec& theta;
    const arma::sp_mat& wfm;
    const std::size_t& N;
    
    // accumulated value
    double phitmp;
    
    // constructors
    DispPar(const arma::colvec& alpha, const arma::rowvec& psi, const arma::rowvec& beta, 
           const arma::colvec& theta, const arma::sp_mat& wfm, const std::size_t& N) 
        : alpha(alpha), psi(psi), beta(beta), theta(theta), wfm(wfm), N(N), phitmp(0) {}
    DispPar(const DispPar& sum, Split) : alpha(sum.alpha), psi(sum.psi), beta(sum.beta),
    theta(sum.theta), wfm(sum.wfm), N(sum.N), phitmp(0) {}
    
    
    void operator() (std::size_t begin, std::size_t end) {
        for (std::size_t k = begin; k < end; k++) {
            float temp = phitmp;
            for (std::size_t i = 0; i < N; i++){
                double mutmp = exp(alpha(i) + psi(k) + beta(k) * theta(i));
                temp += (wfm(i,k) - mutmp) * (wfm(i,k) - mutmp) / mutmp;
            }
            phitmp = temp;
        }
    }
    
    // join phitmp with that of another DispPar
    void join(const DispPar& rhs) { 
        phitmp += rhs.phitmp; 
    }
};

// Update dispersion parameters -- individual dispersion parameter for each word
struct DispPar2 : public Worker {
    const arma::colvec& alpha; 
    const arma::rowvec& psi;
    const arma::rowvec& beta;
    const arma::colvec& theta;
    const arma::sp_mat& wfm;
    const IntegerVector& disptype;
    const NumericVector& dispmin;
    const std::size_t& N;
    const std::size_t& K;
    // output vector
    RVector<double> phi;
    
    // constructors
    DispPar2(const arma::colvec& alpha, const arma::rowvec& psi, const arma::rowvec& beta, 
            const arma::colvec& theta, const arma::sp_mat& wfm, const IntegerVector& disptype,
            const NumericVector& dispmin, const std::size_t& N, const std::size_t& K, NumericVector& phi) 
        : alpha(alpha), psi(psi), beta(beta), theta(theta), wfm(wfm), disptype(disptype), 
          dispmin(dispmin), N(N), K(K), phi(phi){}
    
    void operator() (std::size_t begin, std::size_t end) {
        for (std::size_t k = begin; k < end; k++) {
            double phitmp = 0.0;
            for (std::size_t i = 0; i < N; i++){
                double mutmp = exp(alpha(i) + psi(k) + beta(k) * theta(i));
                phitmp += (wfm(i,k) - mutmp) * (wfm(i,k) - mutmp) / mutmp;
            }
            phitmp = ((K)*phitmp)/(N*K - 2*N - 2*K);
            if (disptype(0) == 4) {
                phi[k] = fmax(dispmin(0), phitmp);
            }else{
                phi[k] = phitmp;
            }
        }
    }
};

// Calculate Document Standard Errors
struct DocErr : public Worker {
    const arma::colvec& alpha; 
    const arma::rowvec& psi;
    const arma::rowvec& beta;
    const arma::colvec& theta;
    const arma::rowvec& phi;
    const double& priorprecalpha;
    const double& priorprectheta;
    // output vector
    RVector<double> thetaSE;
    
    // constructors
    DocErr(const arma::colvec& alpha, const arma::rowvec& psi, const arma::rowvec& beta, 
             const arma::colvec& theta, const arma::rowvec& phi, 
             const double& priorprecalpha, const double& priorprectheta, NumericVector& thetaSE) 
        : alpha(alpha), psi(psi), beta(beta), theta(theta), phi(phi), 
          priorprecalpha(priorprecalpha), priorprectheta(priorprectheta), thetaSE(thetaSE){}
    
    void operator() (std::size_t begin, std::size_t end) {
        arma::rowvec lambdai(alpha.size());
        for (std::size_t i = begin; i < end; i++) {
            lambdai = exp(alpha(i) + psi + beta * theta(i));
            arma::mat H(2,2);
            H(0,0) = -accu(lambdai / phi) - priorprecalpha;
            H(1,0) = -accu((beta % lambdai) / phi);
            H(0,1) = H(1,0);
            H(1,1) = -accu(((beta % beta) % lambdai) / phi) - priorprectheta;
            thetaSE[i] = sqrt(-1.0 * H(0,0) / (H(0,0) * H(1,1) - H(1,0) * H(0,1)));
        }
    }
};
// [[Rcpp::export]]

Rcpp::List wordfishcpp_mt(arma::sp_mat &wfm, IntegerVector& dirvec, NumericVector& priorvec, NumericVector& tolvec, IntegerVector& disptype, NumericVector& dispmin, bool ABS,bool svd_sparse, double residual_floor){
    
    // DEFINE INPUTS
    double priorprecalpha = priorvec(0);
    double priorprecpsi = priorvec(1);
    double priorprecbeta = priorvec(2);
    double priorprectheta = priorvec(3);		
    
    // random engine
    std::random_device rd;
    std::mt19937 mt(rd());
    std::uniform_real_distribution<double> dist(0.0, 1.0);
    
    std::size_t N = wfm.n_rows;
    std::size_t K = wfm.n_cols;
    
    // SET INITIAL VALUES
    
    arma::colvec alpha(N); 
    arma::rowvec psi(K); 
    arma::rowvec beta(K); 
    arma::colvec theta(N); 
    arma::colvec thetaSE(N); // document position standard errors
    arma::rowvec phi(K); // word-level dispersion parameters
    phi.fill(1.0);
 
    // Construct Chi-Sq Residuals	
     //arma::sp_mat C(N, K); 
     arma::colvec rsum(sum(wfm,1));
     arma::rowvec csum(sum(wfm,0));
     double asum = accu(wfm);
    if (svd_sparse == true){
        //std::clock_t begin = clock();
     
     //create the residual matrix
        Triplets residual_tri;
        residual_tri.reserve(N*K);
    #if QUANTEDA_USE_TBB
        Residual residual(wfm, rsum, csum, asum, residual_floor, K, residual_tri);
        parallelFor(0, N, residual);
        //Rcout<<"used TBB"<<std::endl;
    #else        
        for (std::size_t i = 0; i < N; i++) {
            create_residual(i, wfm, rsum, csum, asum, residual_floor, K, residual_tri);
        }
        //Rcout<<"not use TBB"<<std::endl;
    #endif
        // Convert to Rcpp objects
        std::size_t mat_size = residual_tri.size();
        arma::umat index_mat(2, mat_size, arma::fill::zeros);
        arma::vec w_values(mat_size, arma::fill::zeros);
        for (std::size_t k = 0; k < residual_tri.size(); k++) {
            index_mat(0,k) = std::get<0>(residual_tri[k]);
            index_mat(1,k) = std::get<1>(residual_tri[k]);
            w_values(k) = std::get<2>(residual_tri[k]);
        }
        
        // constract the sparse matrix
        arma::sp_mat C(index_mat, w_values, N, K);

        const int svdk = NUMSVD;
        arma::mat U(N, svdk);
        arma::vec s(svdk);
        arma::mat V(K, svdk);
        arma::svds(U, s, V, C, svdk);
        for (std::size_t i = 0; i < N; i++) theta(i) = pow(rsum(i)/asum, -0.5) * U(i, 0);
        //Rcout<<"svd done"<<endl;
    } else {
        // Load initial values
        for (std::size_t i=0; i < N; i++) theta(i) = pow(rsum(i)/asum, -0.5) - dist(mt);//* U(i, 0);
    }
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
    arma::rowvec lambdai(K);
    arma::colvec lambdak(N);
    double stepsize = 1.0;
    int outeriter = 0;

    //Initialize LOG-POSTERIOR 
    double lastlp = LASTLP;
    double lp = -1.0*(accu(0.5 * ((alpha % alpha) * priorprecalpha)) + accu(0.5 * ((psi  % psi) * priorprecpsi)) 
                          + accu(0.5 * ((beta  % beta) * priorprecbeta)) + accu(0.5 * ((theta % theta) * priorprectheta)));
    
    // compute LOG posterior
    LogPos logPos(alpha, psi, beta, theta, wfm, K);
    parallelReduce(0, N, logPos);
    lp += logPos.lp;
    
    
    // BEGIN WHILE LOOP
    while(((lp - lastlp) > tolvec(0)) && outeriter < OUTERITER){	
        outeriter++;
        
        // UPDATE WORD PARAMETERS
        NumericVector psi_N(psi.begin(), psi.end());
        NumericVector beta_N(beta.begin(), beta.end());
        WordPar wordPar(alpha, psi_N, beta_N, theta, phi, wfm, tolvec,
                        outeriter, priorprecpsi, priorprecbeta, stepsize);
        parallelFor(0, K, wordPar);
        
        psi = as<arma::rowvec>(psi_N);
        beta = as<arma::rowvec>(beta_N);
        
        // UPDATE DOCUMENT PARAMETERS
        NumericVector alpha_N(alpha.begin(), alpha.end());
        NumericVector theta_N(theta.begin(), theta.end());
        DocPar docPar(alpha_N, psi, beta, theta_N, phi, wfm, tolvec,
                        outeriter, priorprecalpha, priorprectheta, stepsize);
        parallelFor(0, N, docPar);
        
        alpha = as<arma::colvec>(alpha_N);
        theta = as<arma::colvec>(theta_N);
        
        
        // UPDATE DISPERSION PARAMETERS	  
        
        if (disptype(0) == 2) { // single dispersion parameter for all words
            DispPar dispPar(alpha, psi, beta, theta, wfm, N);
            parallelReduce(0, K, dispPar);
            double phitmp = dispPar.phitmp;
            phitmp /= N*K - 2*N - 2*K;
            phi.fill(phitmp);
        }
        
        if (disptype(0) >= 3) { // individual dispersion parameter for each word
            NumericVector phi_N(phi.begin(), phi.end());
            DispPar2 dispPar2(alpha, psi, beta, theta, wfm, disptype, dispmin, N, K, phi_N);
            parallelFor(0, K, dispPar2);
            phi = as<arma::rowvec>(phi_N);
        }	  			
        
        alpha = alpha - mean(alpha);
        theta = (theta - mean(theta))/stddev(theta);		
        
        // CHECK LOG-POSTERIOR FOR CONVERGENCE
        lastlp = lp;
        lp = -1.0*(accu(0.5 * ((alpha % alpha) * priorprecalpha)) + accu(0.5 * ((psi % psi) * priorprecpsi))
                       + accu(0.5 * ((beta % beta) * priorprecbeta)) + accu(0.5 * ((theta % theta) * priorprectheta)));

        LogPos logPos2(alpha, psi, beta, theta, wfm, K);
        parallelReduce(0, N, logPos2);
        lp += logPos2.lp;
        // Rcout<<"outeriter="<<outeriter<<"  lp - lastlp= "<<lp - lastlp<<std::endl;
        // END WHILE LOOP		
    } 
    
    // Fix Global Polarity  
    //Rcout<<"outeriter="<<outeriter<<"  lp - lastlp= "<<lp - lastlp<<std::endl;
    // added the -1 because C counts from ZERO...  -- KB
    if (theta(dirvec(0)-1) > theta(dirvec(1)-1)) {
        beta = -beta;
        theta = -theta;
    }
    
    // COMPUTE DOCUMENT STANDARD ERRORS
    NumericVector thetaSE_N(thetaSE.begin(), thetaSE.end());
    DocErr docErr(alpha, psi, beta, theta, phi, priorprecalpha, priorprectheta, thetaSE_N);
    parallelFor(0, N, docErr);
    thetaSE = as<arma::colvec>(thetaSE_N);
    
    // DEFINE OUTPUT	
    return Rcpp::List::create(Rcpp::Named("theta") = theta,
                              Rcpp::Named("alpha") = alpha,
                              Rcpp::Named("psi") = psi,
                              Rcpp::Named("beta") = beta,
                              Rcpp::Named("phi") = phi,
                              Rcpp::Named("thetaSE") = thetaSE);
    
}
