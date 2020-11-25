#include <queue>
#include <cmath>
#include <vector>
#include <iomanip>
#include <iostream>
#include <algorithm>

using std::cin;
using std::cout;
using std::pair;
using std::queue;
using std::vector;
using std::distance;
using predicate_t = pair<unsigned, double>;

static constexpr double EPSILON = 1e-15;
static constexpr predicate_t null_predicate = {0, -std::numeric_limits<double>::max()};

struct object {
    object(vector<double> features, unsigned int class_id) : features(move(features)), class_id(class_id) {}

    template<class RandomIt>
    static void sort_feature(RandomIt first, RandomIt last, size_t i) {
        sort(first, last, [i](const auto& a, const auto& b) { return a.features[i] < b.features[i]; });
    }

    vector<double> features;
    unsigned int class_id;
};

using dataset_t = vector<object>;
using dataset_ptr = dataset_t::iterator;

void update_gini_variable(size_t &val, long long &sum, int delta) {
    sum -= val * val;
    val += delta;
    sum += val * val;
}

predicate_t gini_predicate(dataset_ptr begin, dataset_ptr end, size_t feature_size, size_t class_size) {
    predicate_t best_predicate = null_predicate;
    double best_gain = null_predicate.second;

    for (size_t i = 0; i < feature_size; ++i) {
        object::sort_feature(begin, end, i);
        if (begin->features[i] == (end - 1)->features[i]) continue;

        vector<size_t> l_count(class_size);
        vector<size_t> r_count(class_size);
        size_t l_size = 0;
        size_t r_size = distance(begin, end);
        long long l_sum = 0;
        long long r_sum = 0;

        for (auto j = begin; j != end; ++j) {
            update_gini_variable(r_count[j->class_id], r_sum, 1);
        }

        for (auto j = begin + 1; j != end; ++j) {
            auto previous_class = (j - 1)->class_id;
            update_gini_variable(r_count[previous_class], r_sum, -1);
            update_gini_variable(l_count[previous_class], l_sum, 1);
            ++l_size;
            --r_size;

            auto current_value = j->features[i];
            auto previous_value = (j - 1)->features[i];

            if (previous_value != current_value) {
                double gain = 1.0 * l_sum / l_size + 1.0 * r_sum / r_size;

                if (gain > best_gain) {
                    best_gain = gain;
                    best_predicate = {i, (previous_value + current_value) / 2.0};
                }
            }
        }
    }
    return best_predicate;
}

double entropy(const vector<size_t>& counts, size_t s) {
    double sum = 0;
    for (auto i : counts) {
        if (i != 0) {
            sum -= (i * 1.0 / s) * log(i * 1.0 / s);
        }
    }
    return sum;
}

predicate_t entropy_predicate(dataset_ptr begin, dataset_ptr end, size_t feature_size, size_t class_size) {
    predicate_t best_predicate = null_predicate;
    double best_gain = null_predicate.second;

    for (unsigned i = 0; i < feature_size; ++i) {
        object::sort_feature(begin, end, i);
        if (begin->features[i] == (end - 1)->features[i]) continue;

        vector<size_t> l_count(class_size);
        vector<size_t> r_count(class_size);
        size_t l_size = 0;
        size_t r_size = distance(begin, end);

        for (auto i = begin; i != end; ++i) {
            ++r_count[i->class_id];
        }

        for (auto j = begin + 1; j != end; ++j) {
            auto previous = (j - 1)->class_id;

            --r_count[previous];
            ++l_count[previous];
            ++l_size;
            --r_size;

            auto current_value = j->features[i];
            auto previous_value = (j - 1)->features[i];

            if (previous_value == current_value) continue;

            double gain = -entropy(l_count, l_size) * l_size - entropy(r_count, r_size) * r_size;
            if (gain > best_gain) {
                best_gain = gain;
                best_predicate = {i, (previous_value + current_value) / 2.0};
            }
        }
    }
    return best_predicate;
}

struct decision_tree {
    const decision_tree* left;
    const decision_tree* right;
    const size_t size;
    const unsigned int class_id;
    const predicate_t predicate;

    decision_tree(decision_tree* left, decision_tree* right, predicate_t predicate) :
        left(left),
        right(right),
        size((left ? left->size : 0) + (right ? right->size : 0) + 1),
        class_id(std::numeric_limits<unsigned int>::max()),
        predicate(move(predicate)) {}

    explicit decision_tree(unsigned int class_id) :
        left(nullptr),
        right(nullptr),
        size(1),
        class_id(class_id),
        predicate() {}

    ~decision_tree() {
        delete left;
        delete right;
    }

    static decision_tree* make_tree(const dataset_t& train_set, size_t class_size, int max_level) {
        return decision_tree_builder(class_size, train_set, max_level).build_tree();
    }

    void print() const {
        cout << std::fixed << std::setprecision(10) << size << '\n';

        queue<const decision_tree*> nodes;
        nodes.emplace(this);
        int node_index = 1;

        while (!nodes.empty()) {
            const auto& node = nodes.front();
            if (node->size == 1) {
                cout << "C " << node->class_id + 1 << '\n';
            } else {
                cout << "Q " << node->predicate.first + 1 << ' ' << node->predicate.second << ' '
                    << ++node_index << ' ';
                cout << ++node_index << '\n';
                nodes.emplace(node->right);
                nodes.emplace(node->left);
            }
            nodes.pop();
        }
    }

    class decision_tree_builder {
    private:
        const size_t class_size;
        dataset_t objects;
        size_t max_level;

        vector<unsigned int> class_distribution(dataset_ptr begin, dataset_ptr end) const {
            vector<unsigned int> distribution(class_size);
            for (auto object = begin; object != end; ++object) {
                ++distribution[object->class_id];
            }
            return distribution;
        }

        decision_tree* get_leaf(dataset_ptr begin, dataset_ptr end) {
            auto distribution = class_distribution(begin, end);
            auto max_iterator = std::max_element(distribution.begin(), distribution.end());
            unsigned int most_common_class = static_cast<unsigned int>(distance(distribution.begin(), max_iterator));
            return new decision_tree(most_common_class);
        }

    public:
        decision_tree_builder(const size_t class_size, dataset_t objects, size_t max_level) :
                class_size(class_size),
                objects(move(objects)),
                max_level(max_level) {}

        decision_tree* build_tree() {
            return build_tree(objects.begin(), objects.end());
        }

        decision_tree* build_tree(dataset_ptr begin, dataset_ptr end, size_t level = 0) {
            const size_t features_size = begin->features.size();

            auto class_not_eq = [](dataset_t::const_reference a, dataset_t::const_reference b) {
                return a.class_id != b.class_id;
            };
            if (adjacent_find(begin, end, class_not_eq) == end) {
                return new decision_tree(begin->class_id);
            }

            if (level == max_level) {
                return get_leaf(begin, end);
            }

            auto get_predicate = objects.size() < 1000 ? entropy_predicate : gini_predicate;
            predicate_t predicate = get_predicate(begin, end, features_size, class_size);

            if (predicate == null_predicate) {
                return get_leaf(begin, end);
            }

            auto middle = std::partition(begin, end, [&predicate](dataset_t::const_reference obj) {
                return obj.features[predicate.first] > predicate.second;
            });

            return new decision_tree(
                build_tree(begin, middle, level + 1),
                build_tree(middle, end, level + 1),
                predicate
            );
        }
    };
};

int main() {
    std::ios_base::sync_with_stdio(false);
    cin.tie(nullptr);

    size_t features_size, class_size, max_level, objects_size;
    cin >> features_size >> class_size >> max_level >> objects_size;
    dataset_t train_set(objects_size, {vector<double>(features_size), 0});

    for (auto& train_object : train_set) {
        for (auto& f : train_object.features) {
            cin >> f;
        }
        cin >> train_object.class_id;
        --train_object.class_id;
    }

    decision_tree* dt = decision_tree::make_tree(train_set, class_size, max_level);
    dt->print();
    delete dt;

    return 0;
}
