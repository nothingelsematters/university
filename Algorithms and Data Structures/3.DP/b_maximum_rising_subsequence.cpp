#include <iostream>
#include <vector>
using namespace std;

int main() {
    int n;
    cin >> n;
    vector<int> row(n);
    vector<int> dp(n);
    vector<int> help(n, -1);
    int supermax=0;
    int helpus=0;
    for (int i=0; i<n; i++) {
        cin >> row[i];
        int max=0;
        for (int j=0; j<i; j++) if ((dp[j]>max) && (row[i]>row[j])) {
                help[i]=j;
                max=dp[j];
            }
        if (max==0) dp[i]=1;
        else dp[i]=max+1;
        if (dp[i]>supermax) {
            helpus=i;
            supermax=dp[i];
        }
    }
    cout << supermax << "\n";
    vector<int> ans;
    while (helpus!=-1) {
        ans.push_back(row[helpus]);
        helpus=help[helpus];
    }
    for (int i=ans.size()-1; i>=0; i--) cout << ans[i] << ' ';
}
