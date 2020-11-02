#include <iostream>
#include <iomanip>
#include <limits>
#include <algorithm>
#include <vector>
#include <set>
#include <random>

using std::pair;
using std::vector;
using std::set;
using std::mt19937;


class SupportVectorMachine {

public:
    SupportVectorMachine(
        vector<vector<double>> objects,
        vector<double> results,
        unsigned int c,
        double tolerance = 1e-3) :
        tolerance(tolerance),
        c(c),
        objects_amount(objects.size()),
        objects(objects),
        results(results) {

        alphas.resize(objects_amount, 0.0);
        errors.resize(objects_amount, 0.0);

        for (bool examine_all = true, changed = false; changed || examine_all; examine_all = !examine_all && !changed) {
            changed = false;

            if (examine_all) {
                for (size_t i = 0; i < objects_amount; ++i) {
                    changed |= examine_example(i);
                }
            } else {
                for (size_t i : vector<size_t>(non_bound.begin(), non_bound.end())) {
                    changed |= examine_example(i);
                }
            }
        }
    }

    pair<vector<double>, double> coefficients() {
        return {alphas, b};
    }

private:
    bool equals(double a, double b = 0, double e = epsilon) {
        return std::abs(a - b) < e;
    }

    double f(size_t x) {
        double result = -b;
        for (size_t i = 0; i < objects_amount; ++i) {
            result += alphas[i] * results[i] * objects[x][i];
        }
        return result;
    }

    bool on_bounds(double alpha) {
        return equals(alpha) || equals(alpha, c);
    }

    double svm_output(double alpha, size_t i, double y) {
        return !equals(alpha) && equals(alpha, 0, c - epsilon) ? errors[i] : (f(i) - y);
    }

    bool take_step(size_t i1, size_t i2, double E2) {
        if (i1 == i2) {
            return false;
        }

        double y1 = results[i1];
        double y2 = results[i2];

        double alpha1 = alphas[i1];
        double alpha2 = alphas[i2];

        double E1 = svm_output(alpha1, i1, y1);
        double s = y1 * y2;

        double L = std::max(0.0, equals(y1, y2) ? alpha1 + alpha2 - c : alpha2 - alpha1);
        double H = std::min(static_cast<double>(c), equals(y1, y2) ? alpha1 + alpha2 : c + alpha2 - alpha1);

        if (equals(L, H)) {
            return false;
        }

        double k11 = objects[i1][i1];
        double k12 = objects[i1][i2];
        double k22 = objects[i2][i2];

        double eta = k11 + k22 - 2 * k12;

        double a2;

        if (!equals(eta)) {
            a2 = alpha2 + y2 * (E1 - E2) / eta;
            if (a2 < L) {
                a2 = L;
            } else if (a2 > H) {
                a2 = H;
            }
        } else {
            double f1 = y1 * (E1 + b) - alpha1 * k11 - s * alpha2 * k12;
            double f2 = y2 * (E2 + b) - s * alpha1 * k12 - alpha2 * k22;
            double L1 = alpha1 + s * (alpha2 - L);
            double H1 = alpha1 + s * (alpha2 - H);
            double objL = L1 * f1 + L * f2 + 0.5 * L1 * L1 * k11 + 0.5 * L * L * k22 + s * L * L1 * k12;
            double objH = H1 * f1 + H * f2 + 0.5 * H1 * H1 * k11 + 0.5 * H * H * k22 + s * H * H1 * k12;

            if (objL < objH - epsilon) {
                a2 = L;
            } else if (objL > objH + epsilon) {
                a2 = H;
            } else {
                a2 = alpha2;
            }
        }

        if (equals(a2, alpha2, epsilon * (a2 + alpha2 + epsilon))) {
            return false;
        }

        double a1 = alpha1 + s * (alpha2 - a2);

        if (a1 < 0) {
            a2 += s * a1;
            a1 = 0;
        } else if (a1 > c) {
            a2 += s * (a1 - c);
            a1 = c;
        }

        double b1 = E1 + y1 * (a1 - alpha1) * k11 + y2 * (a2 - alpha2) * k12 + b;
        double b2 = E2 + y1 * (a1 - alpha1) * k12 + y2 * (a2 - alpha2) * k22 + b;

        double b_new;
        if (0 < a1 && a1 < c) {
            b_new = b1;
        } else if (0 < a2 && a2 < c) {
            b_new = b2;
        } else {
            b_new = (b1 + b2) / 2.0;
        }

        double deltaB = b_new - b;
        b = b_new;

        double deltaA1 = a1 - alpha1;
        double deltaA2 = a2 - alpha2;

        for (size_t i : non_bound) {
            errors[i] += deltaA1 * y1 * objects[i1][i] + deltaA2 * y2 * objects[i2][i] - deltaB;
        }
        errors[i2] = 0.0;

        if (!on_bounds(a1)) {
            non_bound.insert(i1);
        } else {
            non_bound.erase(i1);
        }

        if (!on_bounds(a2)) {
            non_bound.insert(i2);
        } else {
            non_bound.erase(i2);
        }

        alphas[i1] = a1;
        alphas[i2] = a2;

        return true;
    }

    bool examine_example(size_t i2) {

        int y2 = results[i2];
        double alpha2 = alphas[i2];

        double E2 = svm_output(alpha2, i2, y2);
        double r2 = E2 * y2;

        if ((r2 < -tolerance && alpha2 < c - epsilon) || (r2 > tolerance && alpha2 > epsilon)) {
            if (non_bound.size() > 1) {
                size_t i1 = 0;

                for (size_t i : non_bound) {
                    if (fabs(E2 - errors[i]) > fabs(E2 - errors[i1])) {
                        i1 = i;
                    }
                }

                if (take_step(i1, i2, E2)) {
                    return true;
                }
            }

            vector<size_t> non_bound_ids(non_bound.begin(), non_bound.end());
            std::shuffle(non_bound_ids.begin(), non_bound_ids.end(), random_generator);

            for (size_t i1 : non_bound_ids) {
                if (take_step(i1, i2, E2)) {
                    return true;
                }
            }

            size_t random_start = random_generator() % objects_amount;

            for (size_t i = random_start; i < random_start + objects_amount; ++i) {
                size_t i1 = i % objects_amount;
                if (take_step(i1, i2, E2)) {
                    return true;
                }
            }
        }
        return false;
    }

    static constexpr double epsilon = std::numeric_limits<double>::epsilon();

    mt19937 random_generator = mt19937(std::random_device()());
    double b = 0.0;

    double tolerance;
    unsigned int c;
    size_t objects_amount;
    vector<vector<double>> objects;
    vector<double> results;
    vector<double> alphas;
    vector<double> errors;
    set<double> non_bound;
};

int main() {
    std::ios_base::sync_with_stdio(false);

    size_t objects_amount;
    std::cin >> objects_amount;

    vector<vector<double>> objects(objects_amount, vector<double>(objects_amount));
    vector<double> results(objects_amount);
    int c;

    for (size_t i = 0; i < objects_amount; ++i) {
        for (size_t j = 0; j < objects_amount; ++j) {
            std::cin >> objects[i][j];
        }
        std::cin >> results[i];
    }
    std::cin >> c;

    auto [alphas, b] = SupportVectorMachine(objects, results, c).coefficients();

    for (size_t i = 0; i < objects_amount; ++i) {
       std::cout << std::setprecision(10) << alphas[i] << '\n';
    }
    std::cout << std::setprecision(10) << -b << '\n';

    return 0;
}
