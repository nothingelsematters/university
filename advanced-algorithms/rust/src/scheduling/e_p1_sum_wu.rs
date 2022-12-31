use std::{
    collections::{BinaryHeap, HashSet},
    fs::File,
    io::{self, Write},
};

use crate::utils::Scanner;

struct Job {
    deadline: i64,
    weight: i64,
}

fn schedule_p1_sum_wu(jobs: &[Job]) -> (i64, Vec<i64>) {
    let mut jobs: Vec<(usize, &Job)> = jobs.iter().enumerate().collect();
    jobs.sort_by_key(|(_, j)| j.deadline);

    let mut selected = HashSet::new();
    let mut heap = BinaryHeap::new();
    let mut time = 1;

    jobs.iter().enumerate().for_each(|(i, (_, job))| {
        selected.insert(i);
        heap.push((-job.weight, i));

        if job.deadline >= time {
            time += 1;
        } else {
            selected.remove(&heap.pop().unwrap().1);
        }
    });

    let mut result = vec![-1; jobs.len()];
    let mut sum_wu = 0;
    time = 0;
    jobs.iter().enumerate().for_each(|(i, (j, job))| {
        if selected.contains(&i) {
            result[*j] = time;
            time += 1;
        } else {
            result[*j] = (selected.len() + j) as i64;
            sum_wu += job.weight;
        }
    });

    (sum_wu, result)
}

#[allow(dead_code)]
fn main() -> Result<(), io::Error> {
    let mut scanner = Scanner::from_file("p1sumwu.in")?;
    let n: usize = scanner.read_next();

    let jobs: Vec<Job> = (0..n)
        .map(|_| Job {
            deadline: scanner.read_next(),
            weight: scanner.read_next(),
        })
        .collect();

    let (size, schedule) = schedule_p1_sum_wu(&jobs);

    let mut output = File::create("p1sumwu.out")?;
    writeln!(output, "{}", size)?;

    for i in schedule {
        write!(output, "{} ", i)?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{schedule_p1_sum_wu, Job};

    #[test]
    fn sample() {
        let jobs = vec![
            Job {
                deadline: 1,
                weight: 2,
            },
            Job {
                deadline: 1,
                weight: 3,
            },
            Job {
                deadline: 3,
                weight: 1,
            },
        ];
        let actual = schedule_p1_sum_wu(&jobs);

        assert_eq!(actual, (2, vec![2, 0, 1]));
    }
}
