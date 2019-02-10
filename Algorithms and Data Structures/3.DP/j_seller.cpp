#include <iostream>
#include <vector>
using namespace std;

int infinity,n;
vector<vector<int> > dist;
class town {
public:
int d,l;
};
void townt(vector<vector<town> > &a, int w){
        for (int i=0; i<n; i++)
                if (!((w&(1<<i))>>i))
                        for (int j=0; j<n; j++)
                                if  ((w&(1<<j))>>j) {
                                        if (a[w + (1 << i)][i].d>a[w][j].d + dist[i][j]) {
                                                a[w + (1 << i)][i].d = a[w][j].d + dist[i][j];
                                                a[w + (1 << i)][i].l = j;
                                        }
                                }
}

int main(){
        cin >> n;
        dist.resize(n, vector<int> (n));
        infinity=1000000*(n+1);
        town t;
        t.d=infinity;
        t.l=-1;
        vector<vector<town> > dp(1<<n, vector<town> (n, t));
        for (int i=0; i<n; i++) for (int j=0; j<n; j++) {
                        cin >> dist[i][j];
                        if (i==j) dist[i][j]=infinity;
                }
        for (auto i: dp[0]) i.d=0;
        for (int i=0; i<n; i++) {
                dp[1<<i][i].d=0;
                dp[1<<i][i].l=0;
        }
        for (int i=1; i<(1<<n); i++) townt(dp,i);
        int min=infinity;
        int mini=0;
        for (int i=0; i<n; i++) if (dp[dp.size()-1][i].d<min) {
                        min=dp[dp.size()-1][i].d;
                        mini=i;
                }
        cout << min << "\n";
        int check_mask=(1<<n)-1;
        while(check_mask!=0) {
                cout << mini+1 << ' ';
                check_mask-=(1<<mini);
                mini=dp[check_mask+(1<<mini)][mini].l;
        }
}
