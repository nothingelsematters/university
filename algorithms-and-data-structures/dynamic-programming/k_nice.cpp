#include <fstream>
#include <vector>
using namespace std;

int n,m;
void ability (vector<vector<bool>> &a){
    for (int i=0; i<a.size(); i++)
        for (int j=0; j<a.size(); j++)
            for (int k=0; k<n-1; k++) if ((i >> k) % 2 == (i >> (k + 1)) % 2) {
                    if (((i >> k) % 2 == 1) && ((j >> k) % 2 == 1) && ((j >> (k + 1)) % 2 == 1)) a[i][j] = false;
                    else if (((i >> k) % 2 == 0) && ((j >> k) % 2 == 0) && ((j >> (k + 1)) % 2 == 0)) a[i][j] = false;
                }
}
void base(vector<vector<int>> &a){
    for (int i=0; i<a[0].size(); i++) a[0][i]=1;
}

int main() {
    ifstream fin("nice.in");
    ofstream fout("nice.out");
    fin >> n >> m;
    if (n>m) {
        int t=n;
        n=m;
        m=t;
    }
    if ((n==1) && (m==1)) fout << 2;
    else {
        vector<vector<int>> dp(m, vector<int> (1<<n, 0));
        vector<vector<bool>> can((1<<n), vector<bool> (1<<n, true));
        ability(can);
        base(dp);
        int sum=0;
        for (int i=1; i<dp.size(); i++)
            for (int j=0; j<(1<<n); j++)
                for (int k=0; k<(1<<n); k++){
                    if (can[k][j]) {
                        dp[i][j]+=dp[i-1][k];
                        if (i==dp.size()-1) sum+=dp[i-1][k];
                    }
                }
        fout << sum;
    }
    fin.close();
    fout.close();
}