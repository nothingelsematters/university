#include <iostream>
#include <iomanip>
#include <cmath>
#include <algorithm>
#include <numeric>
#include <unordered_map>
#include <unordered_set>
#include <vector>

std::unordered_set<std::string> read_words() {
    size_t message_size;
    std::cin >> message_size;

    std::unordered_set<std::string> words;

    for (size_t j = 0; j < message_size; ++j) {
        std::string word;
        std::cin >> word;
        words.insert(word);
    }

    return words;
}

int main() {
    size_t class_amount, message_amount;
    long double alpha;

    std::cin >> class_amount;

    long double fine[class_amount];
    for (size_t i = 0; i < class_amount; ++i) {
        std::cin >> fine[i];
    }

    std::cin >> alpha >> message_amount;

    size_t class_word_amount[class_amount] = {};
    std::unordered_map<std::string, std::vector<size_t>> word_amount;

    for (size_t i = 0; i < message_amount; ++i) {
        size_t class_number;
        std::cin >> class_number;
        ++class_word_amount[class_number - 1];
        std::unordered_set<std::string> words = read_words();

        for (const auto& word : words) {
            if (!word_amount.count(word)) {
                word_amount[word] = std::vector<size_t>(class_amount, 0);
            }
            ++word_amount[word][class_number - 1];
        }
    }


    size_t test_message_amount;
    std::cin >> test_message_amount;

    for (size_t i = 0; i < test_message_amount; ++i) {
        std::unordered_set<std::string> words = read_words();

        std::vector<long double> answers(class_amount);
        for (size_t j = 0; j < class_amount; ++j) {
            answers[j] = std::log(fine[j]) + std::log(class_word_amount[j]);

            for (const auto& [word, class_count] : word_amount) {
                long double probability = (class_count[j] + alpha) / (class_word_amount[j] + 2 * alpha);
                answers[j] += std::log(words.count(word) ? probability : (1 - probability));
            }
        }

        long double denominator = 0.0;
        for (size_t j = 0; j < class_amount; ++j) {
            denominator += std::exp(answers[j]);
        }
        for (size_t j = 0; j < class_amount; ++j) {
            std::cout << std::setprecision(15) << std::exp(answers[j]) / denominator << ' ';
        }
        std::cout << '\n';
    }

    return 0;
}
