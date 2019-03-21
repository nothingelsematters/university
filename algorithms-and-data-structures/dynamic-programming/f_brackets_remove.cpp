#include <iostream>
#include <vector>
using namespace std;

int main() {
    string str;
    cin >> str;
    vector<vector<int>> dp(str.size(), vector<int> (str.size()));
    for (int i=0; i<str.size(); i++)
        for (int j=i; j>=0; j--) {
            if (i == j) dp[j][i] = 1;
            else {
                int min=1000;
                if (((str[j] == '(') && (str[i] == ')')) || ((str[j] == '[') && (str[i] == ']')) || ((str[j] == '{') && (str[i] == '}'))) min=dp[j+1][i-1];
                for (int k=j; k<i; k++) if (dp[j][k]+dp[k+1][i]<min) min=dp[j][k]+dp[k+1][i];
                dp[j][i]=min;
            }
        }
    cout << (str.size()-dp[0][str.size()-1]);
}
