#include <iostream>
#include <vector>
using namespace std;

int main(){
    int n;
    cin >> n;
    if (n==0) cout << 0 << "\n" << 0 << " " << 0;
    else if (n==1) {
        int t;
        cin >> t;
        if (t>100) cout << t << "\n" << 1 << " " << 0;
        else cout << t << "\n" << 0 << ' ' << 0;
    }
    else {
        vector<int> price(n);
        vector<vector<int>> dp(n, vector<int>(n, 1000000));
        vector<vector<bool>> b(n, vector<bool>(n, false));
        for (int i = 0; i < n; i++) cin >> price[i];
        for (int i = 0; i < n; i++)
            for (int j = 0; j < n; j++) {
                if ((i == 0) && (j == 0)) dp[i][j] = price[0];
                else if ((i == 0) && (j == 1) && (price[0] > 100)) dp[i][j] = price[0];
                else if (i != 0) {
                    if (price[i] <= 100) {
                        if (j < n - 1) {
                            if ((dp[i - 1][j] + price[i] < dp[i - 1][j + 1])) dp[i][j] = dp[i - 1][j] + price[i];
                            else {
                                b[i][j] = true;
                                dp[i][j] = dp[i - 1][j + 1];
                            }
                        } else dp[i][j] = dp[i - 1][j] + price[i];
                    } else if (price[i] > 100) {
                        if (j > 0 && j < n - 1) {
                            if (dp[i - 1][j - 1] + price[i] > dp[i - 1][j + 1]) {
                                b[i][j] = true;
                                dp[i][j] = dp[i - 1][j + 1];
                            } else dp[i][j] = dp[i - 1][j - 1] + price[i];
                        } else if (j < n - 1) {
                            b[i][j] = true;
                            dp[i][j] = dp[i - 1][j + 1];
                        } else dp[i][j] = dp[i - 1][j - 1] + price[i];
                    }
                }
            }
        int count = 0;
        int max = 0;
        if (dp[n-1][0]>=dp[n-1][1]) max++;
        cout << dp[n-1][max] << "\n";
        int j = max;
        vector<int> help;
        for (int i = n - 1; i >= 0; i--) {
            if (b[i][j]) {
                count++;
                j++;
                help.push_back(i + 1);
            } else if (price[i] > 100) j--;
        }
        cout << max << ' ' << count << "\n";
        for (int i = help.size() - 1; i >= 0; i--) cout << help[i] << "\n";
    }
}
