#include <fstream>
#include <algorithm>

void fase(int quantity, int** ar, int** equiv, int number) {
    int back_up[quantity];
    int* eq = *equiv;
    int* arr = *ar;
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

int* suffix_array(std::string const& str) {
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
    delete[] equivalent;

    return result;
}

int* lcp(std::string const& str, int* suf_ar) {
    int quantity = str.size();
    int* result = new int[quantity];
    int rev_suf[quantity];
    for (int i = 0; i < quantity; ++i) {
        rev_suf[suf_ar[i]] = i;
    }

    int current = 0;
    for (int i = 0; i < quantity; ++i) {
        if (current > 0) {
            --current;
        }

        if (rev_suf[i] == quantity - 1) {
            result[quantity - 1] = -1;
            current = 0;
            continue;
        }

        for (int tmp = suf_ar[rev_suf[i] + 1];
             std::max(i + current, tmp + current) < quantity && str[i + current] == str[tmp + current];
             ++current);

        result[rev_suf[i]] = current;
    }
    return result;
}

int main() {
    std::ifstream fin("array.in");
    std::string str;
    fin >> str;
    fin.close();

    std::ofstream fout("array.out");
    int* suf_ar = suffix_array(str + '\0');
    for (size_t i = 1; i < str.size() + 1; ++i) {
        fout << suf_ar[i] + 1 << ' ';
    }

    fout << '\n';

    int* lcp_ar = lcp(str + '\0', suf_ar);
    for (size_t i = 1; i < str.size(); ++i) {
        fout << lcp_ar[i] << ' ';
    }
    fout.close();

    delete[] suf_ar;
    delete[] lcp_ar;
    return 0;
}
