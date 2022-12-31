use std::{
    collections::{BTreeSet, HashMap},
    fs::File,
    io::{self, Write},
};

use crate::utils::Scanner;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
struct OrdF64(f64);

impl Eq for OrdF64 {}

#[allow(clippy::derive_ord_xor_partial_ord)]
impl Ord for OrdF64 {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

struct DisjunctionSets {
    sets: Vec<usize>,
}

impl DisjunctionSets {
    fn new(n: usize) -> DisjunctionSets {
        DisjunctionSets {
            sets: (0..n).collect(),
        }
    }

    fn find(&mut self, i: usize) -> usize {
        if self.sets[i] == i {
            return i;
        }
        self.sets[i] = self.find(self.sets[i]);
        self.sets[i]
    }

    fn unite(&mut self, mut x: usize, mut y: usize) {
        x = self.find(x);
        y = self.find(y);
        self.sets[x] = y;
    }
}

fn schedule_p1_outree_sum_wc(
    old_times: &[u64],
    old_weights: &[u64],
    outree: &[(usize, usize)],
) -> (u64, Vec<u64>) {
    let n = old_times.len();
    let mut weights = old_weights.to_vec();
    let mut times = old_times.to_vec();

    let mut sequences: Vec<_> = (0..n).collect();
    let mut parents: HashMap<_, _> = outree.iter().cloned().collect();

    let mut sets = DisjunctionSets::new(n);

    let root = (0..n).find(|i| !parents.contains_key(i)).unwrap();
    let mut metrics: Vec<_> = (0..n)
        .map(|i| weights[i] as f64 / times[i] as f64)
        .collect();
    let mut queue: BTreeSet<_> = metrics
        .iter()
        .enumerate()
        .filter(|(i, _)| *i != root)
        .map(|(i, x)| (OrdF64(*x), i))
        .collect();

    while let Some((metrics_j, j)) = queue.iter().rev().next().cloned() {
        queue.remove(&(metrics_j, j));

        let i = sets.find(parents[&j]);
        weights[i] += weights[j];
        times[i] += times[j];

        if queue.remove(&(OrdF64(metrics[i]), i)) {
            metrics[i] = weights[i] as f64 / times[i] as f64;
            queue.insert((OrdF64(metrics[i]), i));
        }

        parents.insert(j, sequences[i]);
        sequences[i] = sequences[j];
        sets.unite(j, i);
    }

    let mut current = sequences[root];
    let mut schedule_sequence = vec![current];
    while let Some(k) = parents.get(&current) {
        current = *k;
        schedule_sequence.push(current);
    }

    let mut schedule = vec![0; n];
    let (sumwc, _) = schedule_sequence
        .iter()
        .rev()
        .fold((0, 0), |(sumwc, sum), i| {
            schedule[*i] = sum;
            (
                sumwc + (sum + old_times[*i]) * old_weights[*i],
                sum + old_times[*i],
            )
        });

    (sumwc, schedule)
}

#[allow(dead_code)]
fn main() -> Result<(), io::Error> {
    let mut scanner = Scanner::from_file("p1outtreewc.in")?;

    let n: usize = scanner.read_next();
    let times: Vec<_> = (0..n).map(|_| scanner.read_next()).collect();
    let weights: Vec<_> = (0..n).map(|_| scanner.read_next()).collect();
    let outree: Vec<_> = (0..n - 1)
        .map(|_| {
            (
                scanner.read_next::<usize>() - 1,
                scanner.read_next::<usize>() - 1,
            )
        })
        .collect();

    let (opt, schedule) = schedule_p1_outree_sum_wc(&times, &weights, &outree);

    let mut output = File::create("p1outtreewc.out")?;
    writeln!(output, "{}", opt)?;

    for i in schedule {
        write!(output, "{} ", i)?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::schedule_p1_outree_sum_wc;

    #[test]
    fn sample_0() {
        let times = vec![1, 3, 2];
        let weights = vec![1, 6, 4];
        let outree = vec![(1, 0), (2, 0)];

        let (opt, schedule) = schedule_p1_outree_sum_wc(&times, &weights, &outree);
        assert_eq!(opt, 49);
        assert!(
            schedule == vec![0, 1, 4] || schedule == vec![0, 3, 1],
            "{schedule:?}",
        )
    }

    #[test]
    fn sample_1() {
        let times = vec![3, 4, 2, 1];
        let weights = vec![2, 3, 3, 2];
        let outree = vec![(0, 1), (3, 2), (2, 1)];

        let (opt, schedule) = schedule_p1_outree_sum_wc(&times, &weights, &outree);
        assert_eq!(opt, 64);
        assert_eq!(schedule, vec![7, 0, 4, 6])
    }

    #[test]
    fn sample_2() {
        let times = vec![1, 2, 3, 4, 5, 6, 7];
        let weights = vec![7, 6, 5, 4, 3, 2, 1];
        let outree = vec![(1, 0), (2, 0), (3, 1), (4, 1), (5, 2), (6, 2)];

        let (opt, schedule) = schedule_p1_outree_sum_wc(&times, &weights, &outree);
        assert_eq!(opt, 210);
        assert_eq!(schedule, vec![0, 1, 3, 6, 10, 15, 21])
    }
}
