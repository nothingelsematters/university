#include <string>
#include <iostream>
#include <fstream>
#include <algorithm>
#include <cctype>
#include <cstddef>
#include <iterator>
#include <ctime>

using string_size_t = std::string::size_type;


static constexpr char SHIFT = 'a';
static constexpr char FREQUENCY[] = "etaoinshrdlcumwfgypbvkxjqz";

static constexpr size_t LENGTH = std::size(FREQUENCY) - 1;
static const double INDEX = 0.065;
static const double EPS = 1e-3;

char getNumber(char ch) {
    return std::tolower(ch) - SHIFT;
}

char getChar(char ch) {
    return ch + SHIFT;
}

std::string encrypt(const std::string& key, const std::string& message) {
    std::string encrypted;
    encrypted.reserve(message.size());

    for (string_size_t i = 0, counter = 0; i < message.size(); ++i) {
        if (std::isalpha(message[i])) {
            encrypted += getChar((getNumber(message[i]) + getNumber(key[counter++ % key.size()])) % LENGTH);
        }
    }

    return encrypted;
}

double calcCoef(const std::string& str, size_t ind, size_t length) {
    string_size_t letterAmount[LENGTH] = {};
    long long amount = 0;
    while (ind < str.size()) {
        letterAmount[getNumber(str[ind])]++;
        amount++;
        ind += length;
    }

    long double res = 0;
    for (long long i : letterAmount) {
        res += i * i;
    }

    amount *= amount;
    return (double) (res / amount);
}

size_t getKeySize(const std::string& str) {
    size_t minLen = 0;
    size_t maxLen = 25;
    if (maxLen > str.size()) {
        maxLen = str.size();
    }

    double minEps = 1;
    for (size_t length = 1; length < maxLen; ++length) {
        double res = 0;
        for (size_t ind = 0; ind < length; ++ind) {
            res += calcCoef(str, ind, length);
        }

        res /= length;
        if (std::abs(res - INDEX) < minEps) {
            minLen = length;
            minEps = std::abs(res - INDEX);
        }
    }
    return minLen;
}

char decryptSymbol(char encrypted_char, char key_char) {
    return getChar((LENGTH + getNumber(encrypted_char) - getNumber(key_char)) % LENGTH);
}

std::string decrypt(const std::string& encrypted) {
    string_size_t keyLength = getKeySize(encrypted);
    std::string key;
    key.reserve(keyLength);

    for (string_size_t i = 0; i < keyLength; ++i) {
        std::pair<string_size_t, char> amount[LENGTH];
        std::generate(std::begin(amount), std::end(amount),
            [n = std::pair<string_size_t, char>(0, getChar(-1))] () mutable -> std::pair<string_size_t, char>
                { return {n.first, n.second = getChar(getNumber(n.second) + 1)}; });

        for (string_size_t j = i; j < encrypted.size(); j += keyLength) {
            ++amount[getNumber(encrypted[j])].first;
        }

        std::sort(std::begin(amount), std::end(amount));

        // statistic analysis
        const size_t TOP = 7;
        const size_t BOTTOM = 2;
        const size_t BOTTOM_SEARCH = 5;

        bool succeeded = false;
        for (size_t j = LENGTH - 1; j > 0 && j > LENGTH - 1 - TOP; --j) {
            char localShift = decryptSymbol(amount[j].second, FREQUENCY[LENGTH - 1 - j]);
            bool failed = false;

            if (std::none_of(std::cbegin(amount), std::cbegin(amount) + BOTTOM, [it = std::cend(FREQUENCY), localShift] (auto p)
                    { return std::find(it - 1 - BOTTOM_SEARCH, it, decryptSymbol(p.second, localShift)) == it; })) {

                key += localShift;
                succeeded = true;
                break;
            }
        }

        if (!succeeded) {
            // common statistics doesn't work properly case
            key += decryptSymbol(amount[LENGTH - 1].second, FREQUENCY[0]);
        }

    }

    std::string message;
    message.reserve(encrypted.size());
    for (string_size_t i = 0; i < encrypted.size(); ++i) {
        message += decryptSymbol(encrypted[i], key[i % key.size()]);
    }

    return message;
}

std::pair<size_t, size_t> test(const std::string& testName, const std::string& key, const std::string& message) {
    static std::pair<size_t, size_t> result = {0, 0};

    std::string realMessage = message;
    realMessage.erase(std::remove_if(realMessage.begin(), realMessage.end(),
        [] (unsigned char x) {return !std::isalpha(x);} ), realMessage.end());
    std::transform(realMessage.begin(), realMessage.end(), realMessage.begin(), [] (unsigned char c) { return std::tolower(c); });

    std::string decrypted = decrypt(encrypt(key, message));

    string_size_t correct = 0;
    for (string_size_t i = 0; i < realMessage.size(); ++i) {
        if (realMessage[i] == decrypted[i]) {
            ++correct;
        }
    }

    bool success = realMessage == decrypted;
    ++result.second;
    result.first += success;

    std::cout << "test \"" << testName << "\":\n"
        << "    symbols: " << realMessage.size() << std::endl
        << "        key: " << key << std::endl
        << "     result: " << (success ? "SUCCEEDED" : "FAILED") << std::endl
        << "    correct: " << (correct * 100 / realMessage.size()) << "%" << std::endl << std::endl;

    return result;
}

std::pair<size_t, size_t> test_file(const std::string& testName, const std::string& key, const std::string& file_name) {
    std::ifstream ifs(file_name);
    std::string message;
    message.assign(std::istreambuf_iterator<char>(ifs), std::istreambuf_iterator<char>());
    return test(testName, key, message);
}

std::string randomKey() {
    std::srand(rand());
    string_size_t keyLength = rand() % 14 + 1;

    std::string key;
    for (string_size_t i = 0; i < keyLength; ++i) {
        key += getChar(rand() % LENGTH);
    }
    return key;
}

