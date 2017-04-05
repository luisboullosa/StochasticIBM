/*

Luis F.V.V. Boullosa
04/Mar/2017
IB Stochastic Model

luis.boullosa@evobio.eu

Implementation of an individual-based model of resource competition.

*/

#include <iostream>
#include <cassert>
#include <fstream>
#include <vector>
#include <random>
#include <algorithm>
#include <cassert>

using namespace std;

void terminateProgram()
{
	cout << "Press any key and <ENTER> to exit"
		<< endl;
	char anyChar;
	cin >> anyChar;
};

// Model parameters
const double p_birth = 0.02;								// Maximum rate of division
const double p_death = 0.0005;								// Minimum risk of death 
int report_time = 100;										// Report every x time points
const int consumption_rate = 10;							
const int division_cost = 1000;
const int use_rate = 1;										// Metabolic cost of living
const int MAXTIME = 20000;
const int INITN = 11;
const int INITX = 50;
const int INITR = 5000000;

// Model-specific functions
void divide(vector<int>& pop, int iInd);
void die(vector<int>& pop, int iInd, int &R);
void stayAlive(vector<int>& pop, int iInd, int &R, poisson_distribution<int> intake, poisson_distribution<int> waste, default_random_engine generator);
double calculateSD(vector<int>& pop, double Mean, int iSize);

int main()
{
	// Seed
	srand(42);
	default_random_engine generator;
	poisson_distribution<int> distribution(INITX);
	poisson_distribution<int> intake(consumption_rate);
	poisson_distribution<int> waste(use_rate);
	ofstream outputFile("simulation.txt");

	// Initial conditions
	vector<int> pop(INITN);

	int iSum = 0; double dAvg; int iCount = 0;

	int R = INITR;

	int t = 1;
	for (int iInd = 0; iInd < INITN; iInd++)
	{
		pop[iInd] = distribution(generator);
		iSum += pop[iInd];
		//cout << pop[iInd] << "\t";
	}
	outputFile << "t\tN\tiR\tSD(iR)\t\teR" << endl;
	cout << "t\tN\t\tiR\t\tSD(iR)\t\teR" << endl;

	dAvg = static_cast<double> (iSum) / static_cast<double> (pop.size());

	double dSum = 0;
	for (int iInd = 0; iInd < pop.size(); iInd++)
	{
		dSum += (static_cast<double>(pop[iInd]) - dAvg) * (static_cast<double>(pop[iInd]) - dAvg);
		//cout << pop[iInd] << "\t";
	}
	double dSD = sqrt(dSum / static_cast<double> (pop.size()));
	//cout << calculateSD(pop, dAvg, pop.size()) << "\t" << dSD << endl;

	//std::cout << t << "\t" << pop.size() << "\t\t" << iSum << "\t\t" << dSD << "\t\t" << R << endl;

	dSD = 0;
	outputFile << t << "\t" << pop.size() << "\t" << iSum << "\t" << dSD << "\t" << R << endl;
	t++;
	while ((t < MAXTIME) && !pop.empty())
	{
		iSum = 0; iCount = 0;
		for (int iInd = 0; iInd < pop.size(); iInd++)
		{

			if (pop[iInd] > 0)
			{
				//cout << pop[iInd] << endl;

				discrete_distribution<> probabilities{
					p_birth * (pop[iInd]) / (1 + pop[iInd]),
					1 - (1 - p_death) * pop[iInd] / (1 + pop[iInd]),
					1 - (p_birth * (pop[iInd]) / (1 + pop[iInd]) + 1 - (1 - p_death) * pop[iInd] / (1 + pop[iInd])) };
				discrete_distribution<> p(probabilities.param());

				int event = p(generator);

				if (event == 2)				// survive
				{
					stayAlive(pop, iInd, R, intake, waste, generator);
					//cout << "Eat " << intake << ". \tUse" << waste << endl;
				}
				else
				{
					if (event == 0)         // divide if you have the resources to do so
					{
						int out = waste(generator);
						if (pop[iInd] >= 2 + division_cost + out)
						{
							pop[iInd] -= out;
							divide(pop, iInd);
						}

					}
					else					
					{
						if (event == 1)			// die	
						{
							die(pop, iInd, R);
						}
						else
							cout << "Some weird error happenned while calculating the probabilities." << endl;
					}
				}

				if (pop[iInd] > 0)
				{
					iSum += pop[iInd];
					iCount += 1;
					if (iCount < 1)
					{
						cout << iSum << endl;
					}
				}
			}								// End of individual loop
		}
		pop.erase(std::remove(pop.begin(), pop.end(), 0), pop.end());
			if (!pop.empty())
			{
				dAvg = static_cast<double> (iSum) / static_cast<double> (pop.size());
				dSD = calculateSD(pop, dAvg, pop.size());

				outputFile << t << "\t" << pop.size() << "\t" << iSum << "\t" << dSD << "\t" << R << endl;

				auto engine = default_random_engine{};
				shuffle(begin(pop), end(pop), engine);
				t++;
			}
			else
			{
				break;
			}
			if (t % report_time == 1)
			{
				//std::cout << t << "\t" << pop.size() << "\t\t" << iSum << "\t\t" << dSD << "\t\t" << R << endl;
			}


	}
	std::cout << "End of simulation" << endl;
	outputFile.close();

	// END OF PROGRAM
	terminateProgram();
	return 0;
}

// Model specific functions
void divide(vector<int>& pop, int iInd)
{
	pop[iInd] = (pop[iInd] - division_cost) / 2;
	pop.push_back(pop[iInd]);
}

void die(vector<int>& pop, int iInd, int &R)
{
	R += pop[iInd];
	pop[iInd] = 0;
}

void stayAlive(vector<int>& pop, int iInd, int &R, poisson_distribution<int> intake, poisson_distribution<int> waste, default_random_engine generator)
{
	int out = waste(generator);
	if (R > 0) // If there are resources,
	{
		int rand_cons = intake(generator);
		int in = rand_cons * R / ( R + rand_cons) - 1;

		if (R >= (in - out))                       // eat as much as you can.
		{
			pop[iInd] += in - out;
			R -= in;
		}
		else
		{
			if ((pop[iInd] - out) < 0)
			{
				die(pop, iInd, R);
			}
			else
			{
				pop[iInd] += R - out;
			}
		R = 0;
		}
	}
	else
	{
		pop[iInd] += R - out;
	}
	if (R < 0)
	{
		cout << "Error, negative resources" << endl;
		abort();
	}
}

double calculateSD(vector<int>& pop, double Mean, int iSize) 
{
	double dSum = 0; double dSD;
	for (int iInd = 0; iInd < iSize; iInd++)
	{
		dSum +=  (static_cast<double>(pop[iInd]) - Mean ) * (static_cast<double>(pop[iInd]) - Mean);
	}

	dSD = sqrt(dSum / static_cast<double>(iSize));
	return dSD;
}