#include <fstream>
#include <algorithm>
#include <cmath>

struct sheets_set {
    double price;
    int default_price, quantity;
    sheets_set() {}
    sheets_set(double p, int dp, int q) : price(p), default_price(dp), quantity(q) {}
};

int find(sheets_set* array, int i, int n) {
    return (n ? (array[i].price * array[i].quantity) * (n / array[i].quantity) +
        std::min(array[i].default_price, find(array, i + 1, n % array[i].quantity)): 0);
}

int main() {
    std::ifstream fin("printing.in");
    std::ofstream fout("printing.out");
    int sheets;
    sheets_set array[7];
    fin >> sheets;
    for (int i = 0; i < 7; ++i) {
        int temp;
        fin >> temp;
        array[i] = sheets_set(temp / pow(10, i), temp, pow(10, i));
    }
    fin.close();
    std::sort(array, array + 7, [](sheets_set a, sheets_set b) {
        return a.price < b.price;
    });
    fout << find(array, 0, sheets);
    fout.close();
    return 0;
}
