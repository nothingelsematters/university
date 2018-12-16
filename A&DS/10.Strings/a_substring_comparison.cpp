#include <iostream>

int main() {
    std::string str;
    size_t requests;
    std::cin >> str >> requests;

    int hash[str.size()];
    int pow[str.size()];

    hash[0] = str[0];
    pow[0] = 1;
    for (int i = 1; i < str.size(); ++i) {
	    pow[i] = pow[i - 1] * 3571;
        hash[i] = hash[i - 1] + str[i] * pow[i];
    }

    for (size_t i = 0; i < requests; ++i) {
        int l1, r1, l2, r2;
        std::cin >> l1 >> r1 >> l2 >> r2;
        std::cout << ((hash[r1 - 1] - (l1 == 1 ? 0 : hash[l1 - 2])) * pow[l2 - 1] ==
                      (hash[r2 - 1] - (l2 == 1 ? 0 : hash[l2 - 2])) * pow[l1 - 1] ? "Yes" : "No") << '\n';
    }

    return 0;
}
