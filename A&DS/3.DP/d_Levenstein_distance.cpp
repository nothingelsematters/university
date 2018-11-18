#include <fstream>
#include <vector>
using namespace std;

string one, two;
vector<vector<int>> dp;
int supercount(int a, int b){
    if (dp[a][b]==-1){
        if ((a==0) && (b==0)) dp[a][b]=0;
        else if ((b==0) && (a>0)) dp[a][b]=a;
        else if ((a==0) && (b>0)) dp[a][b]=b;
        else if (one.substr(a-1,1)==two.substr(b-1,1)) dp[a][b]=supercount(a-1, b-1);
        else {
            int min=supercount(a-1,b)+1;
            if (supercount(a,b-1)+1<min) min=dp[a][b-1]+1;
            if (supercount(a-1,b-1)+1<min) min=dp[a-1][b-1]+1;
            dp[a][b]=min;
        }
    }
    return dp[a][b];
}

int main(){
    ifstream fin ("input.txt");
    ofstream fout ("output.txt");
    fin >> one >> two;
    dp.resize(one.size()+1, vector<int> (two.size()+1, -1));
    fout << supercount(one.size(), two.size());
    fin.close();
    fout.close();
}
