#include <fstream>
#include <algorithm>

void fase(int quantity, int** ar, int** equiv, int number) {
    int back_up[quantity];
    int* eq = *equiv;
    int * arr = *ar;
    for (int i = 0; i < quantity; ++i) {
        back_up[arr[i]] = i;
    }
    std::sort(arr, arr + quantity, [&back_up, &eq, number, quantity](int a, int b) {
        return std::less<std::pair<int, int>>()(
            std::make_pair(eq[back_up[a]], eq[back_up[(a + (1 << (number - 1))) % quantity]]),
            std::make_pair(eq[back_up[b]], eq[back_up[(b + (1 << (number - 1))) % quantity]]));
    });

    int* new_eq = new int[quantity];
    new_eq[0] = 0;
    for (int i = 1; i < quantity; ++i) {
        if (eq[back_up[arr[i]]] != eq[back_up[arr[i - 1]]]) {
            new_eq[i] = new_eq[i - 1] + 1;
            continue;
        }
        if (eq[back_up[(arr[i] + (1 << (number - 1))) % quantity]] !=
            eq[back_up[(arr[i - 1] + (1 << (number - 1))) % quantity]]) {

            new_eq[i] = new_eq[i - 1] + 1;
            continue;
        }

        new_eq[i] = new_eq[i - 1];
    }
    delete[] eq;
    *equiv = new_eq;
}

std::pair<int*, int*> suffix_array(std::string const& str) {
    int quantity = str.size();
    int* result = new int[quantity];
    int* equivalent = new int[quantity];

    for (int i = 0; i < quantity; ++i) {
        result[i] = i;
    }

    std::sort(result, result + quantity, [&str](int a, int b) {
        return str[a] < str[b];
    });

    equivalent[0] = 0;
    for (int i = 1; i < quantity; ++i) {
        equivalent[i] = equivalent[i - 1] + (str[result[i]] == str[result[i - 1]] ? 0 : 1);
    }

    for (int i = 1; (1 << (i - 1)) <= quantity && equivalent[quantity - 1] != quantity - 1; ++i) {
        fase(quantity, &result, &equivalent, i);
    }

    return {result, equivalent};
}

int main() {
    std::ifstream fin("shifts.in");
    std::string str;
    int number;
    fin >> str >> number;
    fin.close();

    std::ofstream fout("shifts.out");
    auto [suf_ar, eq] = suffix_array(str);
    if (eq[str.size() - 1] + 1 < number) {
        fout << "IMPOSSIBLE";
        return 0;
    }
    int index = suf_ar[std::lower_bound(eq, eq  + str.size(), number - 1) - eq];
    fout << str.substr(index, str.size() - index) << str.substr(0, index);
    fout.close();

    delete[] suf_ar;
    delete[] eq;
}
