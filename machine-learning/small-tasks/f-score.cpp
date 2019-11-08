#include <iostream>
#include <iomanip>


long double harmonic_mean(long double a, long double b) {
    return a + b == 0 ? 0 : 2 * a * b / (a + b);
}

int main() {
    size_t size;
    std::cin >> size;

    unsigned int sum = 0;
    unsigned int column[size] = {};
    unsigned int row[size] = {};
    unsigned int trace[size];

    for (size_t i = 0; i < size; ++i) {
        for (size_t j = 0; j < size; ++j) {
            unsigned int value;
            std::cin >> value;

            if (i == j) {
                trace[i] = value;
            }
            sum += value;
            row[i] += value;
            column[j] += value;
        }
    }

    long double precision = 0;
    long double recall = 0;
    long double score = 0;

    for (size_t i = 0; i < size; ++i) {
        long double local_precision = row[i] == 0 ? 0 : (double) trace[i] / row[i];
        long double local_recall = column[i] == 0 ? 0 : (double) trace[i] / column[i];
        long double weight = row[i];

        precision += local_precision * weight;
        recall += local_recall * weight;
        score += harmonic_mean(local_precision, local_recall) * weight;
    }

    std::cout
        << std::setprecision(9)
        << harmonic_mean(precision, recall) / sum << '\n' // macro
        << score / sum << '\n'; // micro

    return 0;
}
