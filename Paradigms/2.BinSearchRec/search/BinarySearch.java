package search;

public class BinarySearch {
//Pre: a - array of Z: forall i = 0..a.length-2: a[i] >= a[i + 1], k, l, r - Z, -1 <= l <= r <= a.length, ((a[l..r] > k && r == a.length) || a[l] >= k >= a[r])
//Post: (a[a.length-1] > x) ? (Natural = a.length) : (Natural = i: i = min(a[i] <= x))
//inv: array a stays unchanged
static int binSearch (String[] a, int k, int l, int r) {
        if (l >= r - 1) {
                return r;
        }
        //(r - l >= 1) => needed is r: (a[r] <= k && a[r] <= a[l]) || (l = a.length-1 && a[l] > k && r = a.length)
        //r - l > 1 => we have to narrow searching area
        if (new Integer(Integer.parseInt(a[(l + r) / 2 + 1])) > k) {
                //a[l..(l +r) / 2] > k => needed i in a[(l + r) / 2..r]
                return binSearch (a, k, (l + r) / 2, r);
        } else {
                //a[(l + r) / 2..r] <= k => needed i in a[l..(l + r) / 2]
                return binSearch (a, k, l, (l + r) / 2);
        }

}

//Pre: a - array of Z: forall i = 1..a.length-2: a[i] >= a[i + 1]
public static void main (String[] args) throws Exception {
        System.out.println(binSearch(args, Integer.parseInt(args[0]), -1, args.length - 1));
}
}
