#include <fstream>
#include <cstring>
using namespace std;

int main() {
        ios_base::sync_with_stdio(false);
        ifstream fin("markchain.in");
        ofstream fout("markchain.out");
        int n;
        fin >> n;
        double arr[n][n];
        for (int i = 0; i < n; ++i)
                for (int j = 0; j < n; ++j)
                        fin >> arr[i][j];
        bool flag = true;
        while (flag) {
                flag = false;
                double temp[n][n];
                double sum;
                for (int i = 0; i < n; ++i) {
                        sum = 0;
                        for (int k = 0; k < n; ++k) sum += arr[0][k] * arr[k][i];
                        temp[0][i] = sum;
                }
                for (int i = 1; i < n; ++i){
                        for (int j = 0; j < n; ++j) {
                                sum = 0;
                                for (int k = 0; k < n; ++k) sum += arr[i][k] * arr[k][j];
                                temp[i][j] = sum;
                                if (!flag && abs(temp[0][j] - temp[i][j]) > 0.000001) flag = true;
                        }
                }
                memcpy(arr, temp, sizeof(arr));
        }
        for (int i = 0; i < n; ++i) {
                fout.precision(4);
                fout << arr[0][i] << '\n';
        }
}
