use std::{
    cmp::{min, Reverse},
    collections::{BTreeSet, HashSet, VecDeque},
    fs::File,
    io::{self, BufRead, BufReader, Write},
    ops::Deref,
    str::FromStr,
};

pub struct Scanner<R> {
    buf_reader: BufReader<R>,
    buffer: VecDeque<String>,
}

impl Scanner<File> {
    pub fn from_file(file_name: &str) -> Result<Scanner<File>, io::Error> {
        let input = File::open(file_name)?;
        let buf_reader = BufReader::new(input);

        Ok(Scanner {
            buf_reader,
            buffer: VecDeque::new(),
        })
    }
}

impl<T> Scanner<T>
where
    BufReader<T>: BufRead,
{
    pub fn read_next<O: FromStr>(&mut self) -> O {
        loop {
            if let Some(token) = self.buffer.pop_front() {
                return token.parse().ok().expect("Failed parse");
            }

            let mut input = String::new();
            self.buf_reader.read_line(&mut input).expect("Failed read");
            self.buffer = input.split_whitespace().map(String::from).collect();
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
struct OrdF64(f64);

impl Eq for OrdF64 {}

#[allow(clippy::derive_ord_xor_partial_ord)]
impl Ord for OrdF64 {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl Deref for OrdF64 {
    type Target = f64;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug)]
struct Job {
    time: u64,
    release: u64,
    deadline: u64,
}

#[derive(Debug, Clone)]
struct Edge {
    to: usize,
    capacity: f64,
    flow: f64,
    reversed: usize,
}

fn move_borders(jobs: &[Job], middle: f64) -> Vec<Vec<f64>> {
    jobs.iter()
        .map(|j| vec![j.release as f64, j.deadline as f64 + middle])
        .collect()
}

fn add_edge(
    graph: &mut [Vec<usize>],
    edges: &mut Vec<Edge>,
    from: usize,
    to: usize,
    capacity: f64,
) {
    graph[from].push(edges.len());
    edges.push(Edge {
        to,
        capacity,
        flow: 0.0,
        reversed: edges.len() + 1,
    });

    graph[to].push(edges.len());
    edges.push(Edge {
        to: from,
        capacity: 0.0,
        flow: 0.0,
        reversed: edges.len() - 1,
    });
}

fn create_graph(
    jobs: &[Job],
    machines: &[u64],
    borders: &[Vec<f64>],
) -> (Vec<Vec<usize>>, Vec<Edge>) {
    let mut times = vec![0.0; borders.len() * 2];

    borders.iter().enumerate().for_each(|(i, border)| {
        times[i] = border[0];
        times[i + borders.len()] = border[1];
    });
    let times: Vec<_> = times
        .into_iter()
        .map(OrdF64)
        .collect::<BTreeSet<_>>()
        .into_iter()
        .map(|x| *x)
        .collect();

    let mut graph = vec![vec![]; 2];

    let start_time_interval_index = graph.len();
    let power_of_m: u64 = machines.iter().sum();

    let mut edges = Vec::new();

    times
        .iter()
        .zip(times.iter().skip(1))
        .enumerate()
        .for_each(|(i, (a, b))| {
            graph.push(vec![]);

            add_edge(
                &mut graph,
                &mut edges,
                start_time_interval_index + i,
                1,
                power_of_m as f64 * a - b,
            );
        });

    let start_extra_interval_index = graph.len();
    times
        .iter()
        .zip(times.iter().skip(1))
        .enumerate()
        .for_each(|(i, (a, b))| {
            for j in 0..machines.len() {
                graph.push(Vec::new());

                let from = start_extra_interval_index + j + i * machines.len();
                let to = start_time_interval_index + i;

                let capacity = (j + 1) as f64
                    * (a - b)
                    * (if j == machines.len() - 1 {
                        machines[j]
                    } else {
                        machines[j] - machines[j + 1]
                    } as f64);

                add_edge(&mut graph, &mut edges, from, to, capacity as f64);
            }
        });

    let start_job_id = graph.len();

    for i in 0..jobs.len() {
        graph.push(Vec::new());

        let from = start_job_id + i;

        add_edge(&mut graph, &mut edges, 0, from, jobs[i].time as f64);

        let mut time_index = 0;
        while borders[i][0] > times[time_index] {
            time_index += 1;
        }

        while time_index < times.len() && borders[i][1] >= times[time_index] {
            for j in 0..machines.len() {
                let to = start_extra_interval_index + (time_index - 1) * machines.len() + j;

                let capacity = (times[time_index] - times[time_index - 1])
                    * (if j == machines.len() - 1 {
                        machines[j]
                    } else {
                        machines[j] - machines[j + 1]
                    } as f64);
                add_edge(&mut graph, &mut edges, from, to, capacity);
            }
            time_index += 1;
        }
    }

    (graph, edges)
}

fn depth_first_search(
    adjacency: &[Vec<usize>],
    edges: &mut [Edge],
    current: usize,
    to: usize,
    flow: f64,
    used: &mut HashSet<usize>,
) -> f64 {
    if current == to {
        return flow;
    }

    used.insert(current);

    for index in adjacency[current].iter() {
        let edge = edges[*index].clone();

        if !used.contains(&edge.to) && edge.flow < edge.capacity {
            let added = depth_first_search(
                adjacency,
                edges,
                edge.to,
                to,
                min(OrdF64(flow), OrdF64(edge.capacity - edge.flow)).0,
                used,
            );

            if added > 0.0 {
                edges[*index].flow += added;
                edges[edge.reversed].flow -= added;
                return added;
            }
        }
    }

    0.0
}

fn is_schedulable(jobs: &[Job], total_time: u64, machines: &[u64], borders: Vec<Vec<f64>>) -> bool {
    let (adjacency, mut edges) = create_graph(jobs, machines, &borders);
    let mut flow = 0.0;

    while {
        let added = depth_first_search(&adjacency, &mut edges, 0, 1, flow, &mut HashSet::new());
        flow += added;
        added > 0.0
    } {}

    flow == total_time as f64
}

fn schedule_cheese(jobs: &[Job], machines: &[u64]) -> f64 {
    let total_time = jobs.iter().map(|j| j.time).sum();
    let mut left = 0.0;
    let mut right = total_time as f64;

    for _ in 0..10_000 {
        let middle = (left + right) / 2.0;
        let new_borders = move_borders(jobs, middle);

        if is_schedulable(jobs, total_time, machines, new_borders) {
            right = middle;
        } else {
            left = middle;
        }
    }

    (left + right) / 2.0
}

#[allow(dead_code)]
fn main() -> Result<(), io::Error> {
    let mut scanner = Scanner::from_file("cheese.in")?;
    let n: usize = scanner.read_next();
    let m: usize = scanner.read_next();

    let jobs: Vec<Job> = (0..n)
        .map(|_| Job {
            time: scanner.read_next(),
            release: scanner.read_next(),
            deadline: scanner.read_next(),
        })
        .collect();
    let mut machines: Vec<u64> = (0..m).map(|_| scanner.read_next()).collect();
    machines.sort_by_key(|x| Reverse(*x));

    let t = schedule_cheese(&jobs, &machines);

    let mut output = File::create("cheese.out")?;
    writeln!(output, "{}", t)
}

#[cfg(test)]
mod tests {
    use super::{schedule_cheese, Job};

    #[test]
    fn sample_0() {
        assert_eq!(
            0.5,
            schedule_cheese(
                &[
                    Job {
                        time: 13,
                        release: 0,
                        deadline: 4,
                    },
                    Job {
                        time: 10,
                        release: 1,
                        deadline: 3,
                    },
                ],
                &[4, 2],
            )
        )
    }

    #[test]
    fn sample_1() {
        assert_eq!(
            0.0,
            schedule_cheese(
                &[Job {
                    time: 1,
                    release: 0,
                    deadline: 2,
                },],
                &[1],
            )
        )
    }
}
