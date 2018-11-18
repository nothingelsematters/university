#include <fstream>
#include <vector>
using namespace std;

int n,W;
vector<int> weight;
int infinity=20;
class cow{
public:
    int count, lw, ln;
};
void cownt(vector<cow> &a, int q){
    for (int i=0; i<n; i++)
        if ((q>>i)%2==0) {
            cow temp=a[q];
            if (a[q].lw+weight[i]<=W) temp.lw+=weight[i];
            else {
                temp.count++;
                temp.lw=weight[i];
            }
            temp.ln=i;
            if ((a[q+(1<<i)].count>temp.count) || ((a[q+(1<<i)].count=temp.count) && (a[q+(1<<i)].lw>temp.lw)))  a[q+(1<<i)]=temp;
        }
}

int main(){
    ifstream fin ("skyscraper.in");
    ofstream fout ("skyscraper.out");
    fin >> n >> W;
    weight.resize(n);
    for (int i=0; i<n; i++) fin >> weight[i];
    cow temp;
    temp.count=infinity;
    vector<cow> dp(1<<n, temp);
    dp[0].count=0;
    dp[0].ln=0;
    dp[0].lw=0;
    for (int i=0; i<(1<<n); i++) cownt(dp,i);
    fout << dp[dp.size()-1].count+1 << "\n";
    int q=(1<<n)-1;
    int sum=0;
    vector<int> help;
    while (q!=0){
        help.push_back(dp[q].ln);
        int t=q;
        q-=(1<<dp[q].ln);
        if (dp[q].count<dp[t].count){
            fout << help.size() << ' ';
            for (auto i: help) fout << i+1 << ' ';
            fout << "\n";
            help.clear();
        }
    }
    fout << help.size() << ' ';
    for (auto i: help) fout << i+1 << ' ';
    fin.close();
    fout.close();
}