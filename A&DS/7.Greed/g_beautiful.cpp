#include <fstream>
#include <set>
#include <algorithm>
using namespace std;

bool keep_going = true;

void next_permutation(int* a, int n) {
    for (int i = n - 2; i >= 0; --i)
        if (a[i] < a[i + 1]) {
            int min = i + 1;
            for (int j = i + 1; j < n; ++j)
                if ((a[j] < a[min]) && (a[j] > a[i]))
                    min = j;
            swap(a[i], a[min]);
            reverse(a + i + 1, a + n);
            return;
        }
    keep_going = false;
}

int beautify(int* a, int n, int r) {
    int result = 0;
    for (int i = 0; i < n - 1; ++i)
        result += a[i] * a[i + 1];
    return result % r;
}

int main() {
    ifstream fin("beautiful.in");
    ofstream fout("beautiful.out");
    int n, r;
    fin >> n >> r;
    fin.close();
    set<int> beautiful({0, 4, 9, 12, 18, 20, 25, 28, 32, 36, 44, 45, 49, 50, 52, 60, 63, 68, 72, 75, 76, 84, 90, 92, 96, 98, 99, 100, 108, 116, 117, 121, 124, 126, 132, 140, 144, 147, 148, 150, 153, 156, 160, 164, 169, 171, 172, 175, 180, 188, 196, 198, 200, 204, 207, 212, 220, 224, 225, 228, 234, 236, 242, 243, 244, 245, 252, 256, 260, 261, 268, 275, 276, 279, 284, 288, 289, 292, 294, 300, 306, 308, 315, 316, 324, 325, 332, 333, 338, 340, 342, 348, 350, 352, 356, 360, 361, 363, 364, 369, 372, 380, 387, 388, 392, 396, 400, 404, 412, 414, 416, 420, 423, 425, 428, 436, 441, 444, 450, 452, 460, 468, 475, 476, 477, 480, 484, 486, 490, 492, 495, 500, 504, 507, 508, 516, 522, 524, 525, 529, 531, 532, 539, 540, 544, 548, 549, 550, 556, 558, 564, 572, 575, 576, 578, 580, 585, 588, 596, 600, 603, 604, 605, 608, 612, 620, 628, 630, 636, 637, 639, 644, 650, 652, 657, 660, 666, 668, 672, 675, 676, 684, 692, 693, 700, 708, 711, 716, 720, 722, 724, 725, 726, 732, 735, 736, 738, 740, 747, 748, 756, 764, 765, 768, 772, 774, 775, 780, 784, 788, 792, 796, 800, 801, 804, 812, 819, 820, 825, 828, 833, 836, 841, 844, 845, 846, 847, 850, 852, 855, 860, 864, 867, 868, 873, 876, 882, 884, 892, 900, 908, 909, 916, 924, 925, 927, 928, 931, 932, 936, 940, 948, 950, 954, 956, 961, 963, 964, 968, 972, 975, 980, 981, 988, 990, 992, 996});
    int a[n], answer = 0;
    for (int i = 0; i < n; ++i)
        a[i] = i + 1;
    while (keep_going) {
        if (beautiful.find(beautify(a, n, r)) != beautiful.end())
            ++answer;
        next_permutation(a, n);
    }
    fout << answer;
    fout.close();
    return 0;
}