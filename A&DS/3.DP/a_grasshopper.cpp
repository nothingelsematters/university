#include <fstream>
#include <vector>
using namespace std;

class rub{
public:
    int bal;
    int jump;
};

int k;
vector<int> gold;
vector<rub> optimus;
vector<int> help;
long long counting(int a){
    if (optimus[a].bal==-1) {
        int max=0;
        int maxj=0;
        int number=-1;
        for (int i=a-1; ((i>=0) && (i>=a-k)); i--) if (optimus[i].bal>max) {
                number=i;
                maxj=optimus[i].jump;
                max=optimus[i].bal;
            }
        help[a]=number;
        optimus[a].bal=max+gold[a];
        optimus[a].jump=maxj+1;
    }
    return optimus[a].bal;
}

int main(){
    ifstream fin("input.txt");
    ofstream fout("output.txt");
    int n;
    fin >> n >> k;
    gold.resize(n);
    optimus.resize(n);
    help.resize(n);
    optimus[n-1].bal=-1;
    optimus[0].bal=-1;
    gold[0]=0;
    gold[n-1]=0;
    for (int i=1; i<n-1; i++) {
        optimus[i].bal=-1;
        fin >> gold[i];
    }
    for (int i=1; i<n; i++) int num=counting(i);
    fout << optimus[n-1].bal << "\n" << optimus[n-1].jump << "\n";
    n--;
    vector<int> amb;
    while (help[n]!=-1) {
        amb.push_back(n+1);
        n=help[n];
    }
    amb.push_back(n+1);
    amb.push_back(1);
    for (int i=amb.size()-1; i>=0; i--) fout << amb[i] << ' ';
    fin.close();
    fout.close();
}
