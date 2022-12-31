use std::{
    cmp::{max, min},
    collections::{BTreeSet, HashSet},
    fs::File,
    io::{self, Write},
};

use crate::utils::Scanner;

struct BitVec {
    internal: Vec<u32>,
}

impl BitVec {
    const BITS: usize = u32::BITS as usize;

    fn new(size: usize) -> BitVec {
        BitVec {
            internal: vec![0; size / Self::BITS + 1],
        }
    }

    fn get(&self, index: usize) -> bool {
        (self.internal[index / Self::BITS] >> (index % Self::BITS)) & 1 == 1
    }

    fn set_true(&mut self, index: usize) {
        self.internal[index / Self::BITS] |= 1 << (index % Self::BITS);
    }

    fn set_internal_or(&mut self, index: usize, element: u32) {
        self.internal[index] |= element;
    }
}

fn update(dependencies: &mut [BitVec]) {
    (0..dependencies.len()).for_each(|i| {
        for x in 0..dependencies.len() {
            if dependencies[x].get(i) {
                (0..dependencies[0].internal.len()).for_each(|y| {
                    let dependency_i_y = dependencies[i].internal[y];
                    dependencies[x].set_internal_or(y, dependency_i_y);
                });
            }
        }
    })
}

fn topological_sort(
    current: usize,
    dependencies: &[BitVec],
    used: &mut HashSet<usize>,
    sorted: &mut Vec<usize>,
) {
    used.insert(current);

    for i in 0..dependencies.len() {
        if !used.contains(&i) && dependencies[current].get(i) {
            topological_sort(i, dependencies, used, sorted);
        }
    }

    sorted.push(current)
}

fn sort_by_deadlines(
    deadlines: &[i64],
    dependencies: &mut [BitVec],
    order: &[usize],
) -> BTreeSet<(i64, usize)> {
    let mut sorted: BTreeSet<(i64, usize)> =
        deadlines.iter().enumerate().map(|(i, x)| (*x, i)).collect();

    order.iter().cloned().for_each(|u| {
        let mut new_deadline = deadlines[u];

        sorted
            .iter()
            .filter(|(_, x)| dependencies[u].get(*x))
            .enumerate()
            .for_each(|(index, (deadline, i))| {
                let count = (index + 1) as i64;
                if *i != u {
                    //&& *deadline > count / 2 + count % 2 {
                    new_deadline = min(new_deadline, deadline - (count / 2 + count % 2));
                }
            });

        sorted.remove(&(deadlines[u], u));
        sorted.insert((new_deadline, u));
    });

    sorted
}

fn schedule_p2_prec_p1_l_max(
    deadlines: &[i64],
    dependencies: &mut [BitVec],
) -> (i64, i64, Vec<i64>, Vec<i64>) {
    update(dependencies);

    let mut used = HashSet::new();
    let mut topological_sorted = Vec::new();

    for i in 0..dependencies.len() {
        if !used.contains(&i) && (0..dependencies.len()).any(|j| dependencies[i].get(j)) {
            topological_sort(i, dependencies, &mut used, &mut topological_sorted);
        }
    }

    let mut sorted = sort_by_deadlines(deadlines, dependencies, &topological_sorted);

    let mut late = i64::MIN;
    let mut time = 0;
    let mut first_machine = Vec::new();
    let mut second_machine = Vec::new();

    let mut waited = vec![0; dependencies.len()];
    dependencies.iter().for_each(|i| {
        (0..dependencies.len())
            .filter(|j| i.get(*j))
            .for_each(|j| waited[j] += 1)
    });

    while !sorted.is_empty() {
        time += 1;

        let jobs: Vec<_> = sorted
            .iter()
            .filter(|(_, i)| waited[*i] == 0)
            .take(2)
            .cloned()
            .collect();

        first_machine.push((jobs[0].1 + 1) as i64);
        second_machine.push(jobs.get(1).map(|(_, x)| (*x + 1) as i64).unwrap_or(-1));

        jobs.into_iter().for_each(|element| {
            sorted.remove(&element);
            // if time > deadlines[element.1] {
            late = max(late, time - deadlines[element.1]);
            // }

            (0..dependencies.len())
                .filter(|i| dependencies[element.1].get(*i))
                .for_each(|i| waited[i] -= 1);
        })
    }

    (late, time, first_machine, second_machine)
}

#[allow(dead_code)]
fn main() -> Result<(), io::Error> {
    let mut scanner = Scanner::from_file("p2precp1lmax.in")?;
    let n: usize = scanner.read_next();

    let deadlines: Vec<i64> = (0..n).map(|_| scanner.read_next()).collect();
    let mut dependencies: Vec<BitVec> = (0..n)
        .map(|_| {
            let mut bv = BitVec::new(n);
            (0..n)
                .filter(|_| scanner.read_next::<u32>() == 1)
                .for_each(|i| bv.set_true(i));
            bv
        })
        .collect();

    let (late, time, first_machine, second_machine) =
        schedule_p2_prec_p1_l_max(&deadlines, &mut dependencies);

    let mut output = File::create("p2precp1lmax.out")?;
    writeln!(output, "{} {}", late, time)?;

    for i in first_machine {
        write!(output, "{} ", i)?;
    }
    writeln!(output)?;

    for i in second_machine {
        write!(output, "{} ", i)?;
    }
    writeln!(output)?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{schedule_p2_prec_p1_l_max, BitVec};

    #[test]
    fn sample() {
        let deadlines = vec![4, 2, 1, 1];
        let mut dependencies = vec![
            {
                let mut l = BitVec::new(4);
                l.set_true(1);
                l.set_true(3);
                l
            },
            { BitVec::new(4) },
            {
                let mut l = BitVec::new(4);
                l.set_true(1);
                l.set_true(3);
                l
            },
            { BitVec::new(4) },
        ];

        assert_eq!(
            (1, 2, vec![1, 4], vec![3, 2]),
            schedule_p2_prec_p1_l_max(&deadlines, &mut dependencies)
        );
    }
}
