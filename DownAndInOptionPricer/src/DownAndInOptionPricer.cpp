#include <Rcpp.h>
using namespace Rcpp;

// Parameters:
//   Expiration: Time to expiration
//   K: Strike price of the option
//   S0: Current S0 price of the underlying asset
//   sigma: Volatility of the underlying asset
//   r: Risk-free interest rate
//   barrier: Barrier level
//   num_sims: Number of paths for Monte Carlo simulation

// Returns:
//   Up-and-in call option price

// [[Rcpp::export]]
double DownAndInOptionPricer(double Expiration, double K, double S0, double sigma, double r, double barrier, int num_sims) {
    double sumPayoff = 0.0;
    int barrierBreachedCount = 0;

    for (int i = 0; i < num_sims; i++) {
        double S = S0;
        bool hitBarrier = false;
        
        // Simulate path until expiration
        for (double t = 0; t <= Expiration; t += Expiration / 365.0) {
            double Z = R::rnorm(0.0, 1.0);
            S *= exp((r - 0.5 * sigma * sigma) * (Expiration / 365.0) + sigma * sqrt(Expiration / 365.0) * Z);
            
            // Check if barrier is breached at any point in the simulation
            if (S <= barrier) {
                hitBarrier = true;
            }
        }
        
        // Calculate payoff at expiration for paths where the barrier was breached
        if (hitBarrier) {
            barrierBreachedCount++;
            double payoff = std::max(S - K, 0.0); // S is the price at expiration
            sumPayoff += payoff;
        }
    }
    
    Rcpp::Rcout << "Barrier breached in " << barrierBreachedCount << " out of " << num_sims << " simulations." << std::endl;

    return (sumPayoff / num_sims) * exp(-r * Expiration); // Average payoff discounted to present value
}




