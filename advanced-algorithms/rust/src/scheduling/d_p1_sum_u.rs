use std::{
    collections::{BinaryHeap, HashSet},
    fs::File,
    io::{self, Write},
};

use crate::utils::Scanner;

pub struct Job {
    time: i32,
    deadline: i32,
}

pub fn schedule_p1_sum_u(jobs: &[Job]) -> (usize, Vec<i32>) {
    let mut jobs: Vec<(usize, &Job)> = jobs.iter().enumerate().collect();
    jobs.sort_by_key(|(_, j)| j.deadline);

    let mut selected = HashSet::new();
    let mut heap = BinaryHeap::new();
    let mut time = 0;

    jobs.iter().enumerate().for_each(|(i, (_, job))| {
        selected.insert(i);
        heap.push((job.time, i));
        time += job.time;

        if time > job.deadline {
            let (job_time, index) = heap.pop().unwrap();
            selected.remove(&index);
            time -= job_time;
        }
    });

    let mut result = vec![-1; jobs.len()];
    time = 0;
    jobs.iter()
        .enumerate()
        .filter(|(i, _)| selected.contains(i))
        .for_each(|(_, (i, j))| {
            result[*i] = time;
            time += j.time;
        });

    (selected.len(), result)
}

#[allow(dead_code)]
fn main() -> Result<(), io::Error> {
    let mut scanner = Scanner::from_file("p1sumu.in")?;
    let n: usize = scanner.read_next();

    let jobs: Vec<Job> = (0..n)
        .map(|_| Job {
            time: scanner.read_next(),
            deadline: scanner.read_next(),
        })
        .collect();

    let (size, schedule) = schedule_p1_sum_u(&jobs);

    let mut output = File::create("p1sumu.out")?;
    writeln!(output, "{}", size)?;

    for i in schedule {
        write!(output, "{} ", i)?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{schedule_p1_sum_u, Job};

    #[test]
    fn sample() {
        let jobs = vec![
            Job {
                time: 1,
                deadline: 2,
            },
            Job {
                time: 2,
                deadline: 3,
            },
            Job {
                time: 3,
                deadline: 1,
            },
        ];
        let actual = schedule_p1_sum_u(&jobs);

        assert_eq!(actual, (2, vec![0, 1, -1]));
    }
}
