use std::{
    collections::BinaryHeap,
    fs::File,
    io::{self, Write},
};

use crate::utils::Scanner;

pub fn schedule_q_sum_c(times: &[u64], speeds: &[u64]) -> (u64, Vec<(usize, u64)>) {
    let mut heap: BinaryHeap<_> = speeds
        .iter()
        .enumerate()
        .map(|(index, i)| (-(*i as i64), index, 1, -(*i as i64)))
        .collect();

    let mut reverse_sorted_schedule = vec![vec![]; speeds.len()];

    for i in 0..times.len() {
        let (_, index, up, down) = heap.pop().unwrap();
        reverse_sorted_schedule[index].push(i);
        heap.push(((up + 1) * down, index, up + 1, down));
    }

    let mut sorted_times: Vec<_> = times
        .iter()
        .enumerate()
        .map(|(index, x)| (*x, index))
        .collect();
    sorted_times.sort_by(|a, b| a.cmp(b).reverse());

    let mut schedule = vec![(0, 0); times.len()];
    reverse_sorted_schedule
        .into_iter()
        .enumerate()
        .for_each(|(index, v)| {
            v.into_iter().rev().fold(0, |time, i| {
                let job = sorted_times[i].1;
                schedule[job] = (index, time);
                times[job] * speeds[index] + time
            });
        });

    let sum_c = schedule
        .iter()
        .enumerate()
        .map(|(index, (machine, time))| time + times[index] * speeds[*machine])
        .sum();

    (sum_c, schedule)
}

#[allow(dead_code)]
fn main() -> Result<(), io::Error> {
    let mut scanner = Scanner::from_file("qsumci.in")?;
    let n: usize = scanner.read_next();
    let m: usize = scanner.read_next();
    let times: Vec<_> = (0..n).map(|_| scanner.read_next()).collect();
    let speeds: Vec<_> = (0..m).map(|_| scanner.read_next()).collect();

    let (sum_c, schedule) = schedule_q_sum_c(&times, &speeds);

    let mut output = File::create("qsumci.out")?;
    writeln!(output, "{}", sum_c)?;

    for (machine, time) in schedule.iter() {
        writeln!(output, "{} {}", machine + 1, time)?;
    }

    Ok(())
}

#[cfg(test)]
pub mod tests {
    use super::schedule_q_sum_c;

    #[test]
    fn sample_0() {
        let (sum_c, schedule) = schedule_q_sum_c(&[5, 2, 3, 1], &[2]);
        assert_eq!(sum_c, 42);
        assert_eq!(schedule, vec![(0, 12), (0, 2), (0, 6), (0, 0)]);
    }

    #[test]
    fn sample_1() {
        let (sum_c, _) = schedule_q_sum_c(&[2, 2, 2, 2, 2, 2], &[1, 2]);
        assert_eq!(sum_c, 32);
    }

    #[test]
    fn sample_2() {
        let (sum_c, _) = schedule_q_sum_c(&[1, 1, 4, 13, 3, 2, 8], &[2, 4, 1]);
        assert_eq!(sum_c, 62);
    }
}
