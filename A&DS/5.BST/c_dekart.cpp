#include <iostream>
#include <algorithm>
using namespace std;

struct tree {
        int x, y, n, left, right, parent, curpar, curright;
        tree() : parent(-1), left(-1), right(-1), curpar(-1), curright(-1) {}
};

int main() {
        ios_base::sync_with_stdio(false);
        int n;
        cin >> n;
        tree arr[n];
        for (int i = 0; i < n; ++i) {
                cin >> arr[i].x >> arr[i].y;
                arr[i].n = i;
        }
        sort(arr, arr + n, [](tree a, tree b) {
                return (a.x < b.x);
        });
        for (int i = 1; i < n; ++i) {
                if (arr[i].y > arr[i - 1].y) {
                        arr[i - 1].right = arr[i].n;
                        arr[i - 1].curright = i;
                        arr[i].parent = arr[i - 1].n;
                        arr[i].curpar = i - 1;
                } else {
                        int temp = i - 1, dad = -1;
                        while (temp != -1 && arr[i].y < arr[temp].y) {
                                dad = temp;
                                temp = arr[temp].curpar;
                        }
                        if (temp == -1) {
                                arr[i].left = arr[dad].n;
                                arr[dad].curpar = i;
                                arr[dad].parent = arr[i].n;
                        } else {
                                int y = arr[temp].right, z = arr[temp].curright;
                                arr[temp].right = arr[i].n;
                                arr[temp].curright = i;
                                arr[i].parent = arr[temp].n;
                                arr[i].curpar = temp;
                                arr[i].left = y;
                                arr[z].parent = arr[i].n;
                                arr[z].curpar = i;
                        }
                }
        }
        sort(arr, arr + n, [](tree a, tree b) {
                return (a.n < b.n);
        });
        cout << "YES\n";
        for (int i = 0; i < n; ++i)
                cout << arr[i].parent + 1 << ' ' << arr[i].left + 1 << ' ' << arr[i].right + 1 << '\n';
}
