#include <iostream>
#include <vector>
using namespace std;

vector<vector<long long>> dp;
long long count (int n, int i){
    if (dp[n][i]==-1){
        if (i==1) dp[n][i]=(count(n-1,8)+count(n-1,6))%1000000000;
        else if (i==2) dp[n][i]=(count(n-1,7)+count(n-1,9))%1000000000;
        else if (i==3) dp[n][i]=(count(n-1,8)+count(n-1,4))%1000000000;
        else if (i==4) dp[n][i]=(count(n-1,3)+count(n-1,9)+count(n-1,0))%1000000000;
        else if (i==6) dp[n][i]=(count(n-1,7)+count(n-1,1)+count(n-1,0))%1000000000;
        else if (i==7) dp[n][i]=(count(n-1,2)+count(n-1,6))%1000000000;
        else if (i==8) dp[n][i]=(count(n-1,3)+count(n-1,1))%1000000000;
        else if (i==9) dp[n][i]=(count(n-1,4)+count(n-1,2))%1000000000;
        else if (i==0) dp[n][i]=(count(n-1,6)+count(n-1,4))%1000000000;
    }
    return dp[n][i];
}

int main() {
    int n;
    cin >> n;
    dp.resize(n, vector<long long> (10, -1));
    for (int i=0; i<10; i++) dp[0][i]=1;
    dp[0][0]=0;
    dp[0][8]=0;
    long long num=0;
    for (int i=0; i<10; i++) if (count(n-1,i)!=-1) num+=count(n-1,i);
    cout << (num%1000000000);
}
