#include <iostream>
using namespace std;

//Programmer: Chengyun Li
//Date: September 18th-20th, 2020
//Purpose: EECS402 project1, to accrue interest for an investment account

const int MIN_NUM_MONTH = 1; //number of month must be larger than or equal to 1
const double MIN_BALANCE = 0.00; //account balance must be larger than or equal to 0
const double SMALL_BALANCE = 1000.00; //upper limit for a small balance
const double STANDARD_BALANCE = 15000.00; //upper limit for a standard balance
const double MIN_INTEREST_RATE = 0.0015;
const double STD_INTEREST_RATE = 0.00225;
const double MAX_INTEREST_RATE = 0.004;

bool accrueOneMonthsInterest(
                             double &balanceAmt,
                             const bool doPrintInf
                             )
{
    double interestAccrued = 0; //initial interest is 0
    double initialBalance = balanceAmt;
    double initialRate = 0; //0 does not have meaning, will be determined later
    bool validBalance = true; //assume account balance is valid
    
    if (balanceAmt < MIN_BALANCE) //ERROR: account balance is less than 0
    {
        validBalance = false; //account balance invalid, print error
        cout << "ERROR in accrueOneMonthInterest: balanceAmt must be >= 0, but the value "
             << balanceAmt << " was provided!" << endl;
        cout << "Sorry, an error was detected.  Unable to provide results!" << endl;
    }
    else if ( balanceAmt < SMALL_BALANCE) //determine the initial interest rate
    {
        initialRate = MIN_INTEREST_RATE;
    }
    else if (balanceAmt >= SMALL_BALANCE && balanceAmt < STANDARD_BALANCE)
    {
        initialRate = STD_INTEREST_RATE;
    }
    else //account balance larger than STANDARD_BALANCE
    {
        initialRate = MAX_INTEREST_RATE;
    }
    
    interestAccrued = balanceAmt * initialRate;
    balanceAmt += interestAccrued;
    
    if (validBalance && doPrintInf)
    {
        cout << "Accruing interest for 1 month:" << endl;
        cout << "  Initial balance: " << initialBalance << endl;
        cout << "  Initial rate: " << initialRate << endl;
        cout << "  Interest accrued: " << interestAccrued << endl;
        cout << "  New balance: " << balanceAmt << endl;
    }
    
    return validBalance;
}

bool accrueInterest(
                    double &balanceAmt,
                    const int numMonths,
                    const bool doPrintEachMonth
                    )
{
    bool validMonth = true; //assume number of Months is valid
    bool successOneMonth;
    
    if (numMonths < MIN_NUM_MONTH ) //ERROR: number of months is less than 0
    {
        validMonth = false;
        cout << "ERROR in accrueInterest: numMonths must be > 0, but the value "
             << numMonths << " was provided!" << endl;
        cout << "Sorry, an error was detected.  Unable to provide results!" << endl;
    }
    else
    {
        for (int i = 0; i < numMonths; i++)
        {
            successOneMonth = accrueOneMonthsInterest(balanceAmt, doPrintEachMonth);
            if (!successOneMonth) //if accrueOneMonthsInterest() has invalid account
                                  //balance, this for loop will stop
            {
                i += numMonths; //so (i > numMonths), this for loop ends
            }
        }
    }
    
    return validMonth;
}

#ifdef ANDREW_TEST
#include "andrewTest.h"
#else
int main()
{
    double accountBalance;
    double initialBalance;
    int numMonths;
    char monthByMonth;
    bool eachMonth;
    bool successInterest;
    cout << "Enter the initial balance of the account: ";
    cin >> accountBalance;
    cout << "Enter the number of months to accrue interest: ";
    cin >> numMonths;
    cout << "Show month-by-month results - 'y' for yes, any other for no: ";
    cin >> monthByMonth;
    
    initialBalance = accountBalance;
    
    if (monthByMonth == 'y') //whether month-by-month result is prefered
    {
        eachMonth = true;
    }
    else
    {
        eachMonth = false;
    }
    
    successInterest = accrueInterest(accountBalance, numMonths, eachMonth);
    
    if (initialBalance >= MIN_BALANCE && successInterest) //print summary only when numMonths and
                                                          //initial balance are valid
    {
        cout << "Interest accured for " << numMonths << " months!" << endl << endl;
        cout << "Initial balance: " << initialBalance << endl;
        cout << "Total interest accrued: " << (accountBalance - initialBalance) << endl;
        cout << "Final balance: " << accountBalance << endl;
    }

}
#endif
