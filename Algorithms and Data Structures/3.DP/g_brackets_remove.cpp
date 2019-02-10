#include <iostream>
#include <vector>
using namespace std;

string str;
vector<vector<int>> dp;
vector<vector<int>> help;
void print(int a, int b){
    if (dp[a][b]!=b-a+1){
        if (dp[a][b]==0) {
            cout << str.substr(a,b-a+1);
        }
        else if (help[a][b]==-1){
            cout << str[a];
            print(a+1,b-1);
            cout << str[b];
        } else {
            print(a,help[a][b]);
            print(help[a][b]+1,b);
        }
    }
}

int main() {
    cin >> str;
    dp.resize(str.size(), vector<int> (str.size()));
    help.resize(str.size(), vector<int> (str.size()));
    for (int i=0; i<str.size(); i++)
        for (int j=i; j>=0; j--) {
            if (i == j) dp[j][i] = 1;
            else {
                int min=1000;
                int minnum=-1;
                if (((str[j] == '(') && (str[i] == ')')) || ((str[j] == '[') && (str[i] == ']')) || ((str[j] == '{') && (str[i] == '}'))) min=dp[j+1][i-1];
                for (int k=j; k<i; k++) if (dp[j][k]+dp[k+1][i]<min) {
                        minnum=k;
                        min=dp[j][k]+dp[k+1][i];
                    }
                dp[j][i]=min;
                help[j][i]=minnum;
            }
        }
    print(0,str.size()-1);
}
