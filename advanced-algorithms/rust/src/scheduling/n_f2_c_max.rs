use std::{
    cmp::{max, min},
    collections::{BinaryHeap, VecDeque},
    fs::File,
    io::{self, Write},
};

use crate::utils::Scanner;

pub fn schedule_f2_c_max(p1: &[i128], p2: &[i128]) -> (i128, VecDeque<usize>) {
    let mut todo: BinaryHeap<_> = (0..p1.len())
        .map(|i| (-min(p1[i], p2[i]), i, p1[i] < p2[i]))
        .collect();
    let mut l = VecDeque::new();
    let mut r = VecDeque::new();

    while let Some((_, index, first_flag)) = todo.pop() {
        if first_flag {
            l.push_back(index);
        } else {
            r.push_front(index);
        }
    }

    l.extend(r);
    let mut first_current = 0;
    let mut second_current = 0;

    (0..p1.len()).for_each(|i| {
        first_current += p1[l[i]];
        second_current = max(first_current, second_current) + p2[l[i]];
    });

    (second_current, l)
}

#[allow(dead_code)]
fn main() -> Result<(), io::Error> {
    let mut scanner = Scanner::from_file("f2cmax.in")?;
    let n: usize = scanner.read_next();
    let p1: Vec<_> = (0..n).map(|_| scanner.read_next()).collect();
    let p2: Vec<_> = (0..n).map(|_| scanner.read_next()).collect();

    let (size, schedule) = schedule_f2_c_max(&p1, &p2);

    let mut output = File::create("f2cmax.out")?;
    writeln!(output, "{}", size)?;

    for _ in 0..2 {
        for i in schedule.iter() {
            write!(output, "{} ", i + 1)?;
        }
        writeln!(output)?;
    }

    Ok(())
}

#[cfg(test)]
pub mod tests {
    use super::schedule_f2_c_max;

    #[test]
    fn sample_0() {
        let (c, s) = schedule_f2_c_max(&[1, 2, 3], &[5, 5, 5]);
        assert_eq!(c, 16);
        assert_eq!(&s, &[0, 1, 2]);
    }

    #[test]
    fn sample_1() {
        let (c, s) = schedule_f2_c_max(&[3, 2], &[1, 3]);
        assert_eq!(c, 6);
        assert_eq!(&s, &[1, 0]);
    }
}
