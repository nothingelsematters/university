#include <cstdio>
#include <algorithm>
#include <iostream>
#include <ctime>

int main() {
    std::ios_base::sync_with_stdio(false);

    int objects_amount, features_amount;
    scanf("%d %d\n", &objects_amount, &features_amount);
    int input[objects_amount][features_amount + 1];
    double x[objects_amount][features_amount + 1];

    int maxs = -1e9;
    int mins = 1e9;
    for (int i = 0; i < objects_amount; ++i) {
        for (int j = 0; j < features_amount; ++j) {
            scanf("%d", &input[i][j]);
            maxs = std::max(maxs, input[i][j]);
            mins = std::min(mins, input[i][j]);
        }
        x[i][features_amount] = 1;
        scanf("%d", &input[i][features_amount]);
    }

    int diff = maxs - mins;
    for (int i = 0; i < objects_amount; ++i) {
        for (int j = 0; j < features_amount; ++j) {
            x[i][j] = ((double) input[i][j] - mins) / diff;
        }
    }

    if (objects_amount <= 100) {
        double momentum[features_amount + 1] = {};
        double w[features_amount + 1] = {};

        for (int epoch = 0; epoch < 410000; ++epoch) {
            int i = epoch % objects_amount;

            double diff = 0;
            for (int j = 0; j < features_amount + 1; ++j) {
                diff += w[j] * x[i][j];
            }
            diff -= input[i][features_amount];

            double grad[features_amount + 1];
            for (int j = 0; j < features_amount + 1; ++j) {
                grad[j] = diff * x[i][j];
                momentum[j] = momentum[j] * 0.9 + 0.008 * grad[j];
                w[j] -= momentum[j];
            }
        }

        double s = 0.0;
        for (int j = 0; j < features_amount; ++j) {
            printf("%f\n", w[j] /= (maxs - mins));
            s += mins * w[j];
        }
        printf("%f\n", w[features_amount] - s);
        return 0;
    }


    double momentum[features_amount + 1] = {};
    double w[features_amount + 1] = {};

    for (int epoch = 0; epoch < 390000; ++epoch) {
        int i = rand() % objects_amount;

        double diff = 0;
        for (int j = 0; j < features_amount + 1; ++j) {
            diff += w[j] * x[i][j];
        }
        diff -= input[i][features_amount];

        double grad[features_amount + 1];
        for (int j = 0; j < features_amount + 1; ++j) {
            grad[j] = diff * x[i][j];
            momentum[j] = momentum[j] * 0.9 + 1e-2 * grad[j];
            w[j] -= momentum[j];
        }
    }

    double s = 0.0;
    for (int j = 0; j < features_amount; ++j) {
        printf("%f\n", w[j] /= (maxs - mins));
        s += mins * w[j];
    }
    printf("%f\n", w[features_amount] - s);

    return 0;
}
