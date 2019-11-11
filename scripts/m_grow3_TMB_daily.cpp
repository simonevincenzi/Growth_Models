//  Simones's growt model
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_INTEGER(linf_ind_sd);				// indicator for how to calculate sd for linf
  DATA_INTEGER(n_linf_ind_sd);				// indicator for how to calculate sd for linf
  DATA_INTEGER(k_ind_sd);					// indicator for how to calculate sd for k
  DATA_INTEGER(n_k_ind_sd);					// indicator for how to calculate sd for k
  DATA_INTEGER(n);				// Number of data points
  DATA_VECTOR(Length);          //(1,n)		// Length of fish
  DATA_VECTOR(age);          //(1,n)			// Age of fish
  DATA_INTEGER(total_marks);          //			// Number of unique individuals
  DATA_IVECTOR(start);          //(1,total_marks)	// Start row in data frame for j'th individual
  DATA_IVECTOR(stop);          //(1,total_marks)	// Stop row in data frame for j'th individual
  DATA_INTEGER(q);          //				// Number of columns in design matrix
  DATA_SPARSE_MATRIX(X);          //(1,n,1,q)		// Design matrix
  DATA_INTEGER(q_k0);          //				//   --||--
  DATA_SPARSE_MATRIX(X_k0);          //(1,n,1,q_k0)		//   --||--
  DATA_INTEGER(q_t0);          //				//   --||--
  DATA_SPARSE_MATRIX(X_t0);          //(1,n,1,q_t0)		//   --||--

  PARAMETER_VECTOR(beta);          //(1,q,-0.3,0.7,1)	// Regression parameters
  PARAMETER_VECTOR(beta_k0);          //(1,q_k0,-2.5,1.5,1)	// it was -5 the lower bound, 2 upper
  PARAMETER_VECTOR(beta_t0);          //(1,q_t0,-3,1,1)	// -5 is the lower bound
  PARAMETER(log_sigma_u);          //(-5.0,2.0,2)	// log(individual sd) for k  #if - 2 == parameter fixed and then needs to be fixed the value in the pin file
  PARAMETER(log_sigma_v);          //(-5.0,2.0,2)	// log(individual sd) for gamma
  PARAMETER(log_sigma_w);          //(-5.0,2.0,-2)	// log(individual sd) for t0  // -2 if we want to block it, 2 if we want to estimate,valid for all fixed effects
  PARAMETER(log_sigma);          //(0.0,7.0,1)	// log(measurement error sd)
  PARAMETER_VECTOR(u);          //(1,total_marks,2)	// Unscaled random effects for k
  PARAMETER_VECTOR(v);          //(1,total_marks,2)	// Unscaled random effects for gamma
  PARAMETER_VECTOR(w);          //(1,total_marks,-2)	// Unscaled random effects for t0 // -2 if we want to block it, 2 if we want to estimate,valid for all random effects. If we shut down the fixed, we also have to shut down the random
  
  Type g=0.0;   // Negative log likelihood
  Type sigma = exp(log_sigma);  
  Type sigma_u = exp(log_sigma_u);  
  Type sigma_v = exp(log_sigma_v);  
  Type sigma_w = exp(log_sigma_w);  
  
  vector<Type> mu = X*beta;            // Linear predictor
  vector<Type> mu_k0 = X_k0*beta_k0;   // Linear predictor
  vector<Type> mu_t0 = X_t0*beta_t0;   // Linear predictor
  
  vector<Type> LL(Length.size());
  vector<Type> LINF(Length.size());
  int ii=0;
  
  for(int i=0;i<total_marks;i++)   // Start at i=0; TMB is zero indexed!!!! 
  {
    Type k0 = exp(mu_k0(start(i))); 
    Type k = k0*exp(u(i));
    
    Type t0 = mu_t0(start(i)) + w(i); 
    Type gamma = exp(mu(start(i))*Type(10.0) + v(i));	// The factor 10 is to scale beta better
    
     g -= dnorm(u(i),Type(0),sigma_u,true);		
     g -= dnorm(v(i),Type(0),sigma_v,true);		
     g -= dnorm(w(i),Type(0),sigma_w,true);		
    
    for(int j=start(i);j<=stop(i);j++)
    {   
      
      Type Linf = gamma;
      Type L = Linf*(1.0-exp(-k*(age(j)-t0)));
      LL(ii)=L;
      LINF(ii++)=exp(-k*(age(j)-t0));
      g -= dnorm(Length(j),L,sigma,true);
    }	
  }
  
  // Prediction at Age 28
  vector<Type> L_28(total_marks*18); // one for each title
  vector<Type> L_inf_vb(total_marks);
  vector<Type> k_vb(total_marks);
  vector<Type> t0_vb(total_marks);
  int cont = 0;
   int j_min = 1;
   for(int i=0;i<total_marks;i++)
   {
   Type k0 = exp(mu_k0(start(i))); 
   Type k = k0*exp(u(i));
    
      Type t0 = mu_t0(start(i)) + w(i); 
      Type gamma = exp(mu(start(i))*Type(10.0) + v(i));
      Type Linf = gamma;
      L_inf_vb(i) = Linf;
      k_vb(i) = k;
      t0_vb(i) = t0;
      for(int j=j_min;j<19;j++)
        {
      L_28(cont++) = Linf*(1.0-exp(-k*(j-t0)));
    }
      }

//  vector<Type> L_28(total_marks); // one for each title
//  for(int i=0;i<total_marks;i++){
//    Type k0 = exp(mu_k0(start(i))); 
//    Type k = k0*exp(u(i));
    
    //    Type t0 = mu_t0(start(i)) + w(i); 
    //  Type gamma = exp(mu(start(i))*Type(10.0) + v(i));
    // Type Linf = gamma;
    //  L_28(i) = Linf*(1.0-exp(-k*(28-t0)));
    
    //    }
  
  
  
  
  // How you can report things back to the R session
  REPORT(LL);
  REPORT(LINF);
  REPORT(mu_k0);
  ADREPORT(L_28);
  ADREPORT(L_inf_vb);
  ADREPORT(k_vb);
  ADREPORT(t0_vb);  
  
  //ADREPORT(LL.segment(10,20))
  
  return g;
}
