use std::{
    collections::VecDeque,
    fs::File,
    io::{self, BufRead, BufReader, Stdin},
    str::FromStr,
};

mod extern_c {
    extern "C" {
        pub(super) fn rand() -> i32;

        #[allow(dead_code)]
        pub(super) fn srand(seed: i32);
    }
}

pub fn rand() -> i32 {
    unsafe { extern_c::rand() }
}

pub fn _srand(seed: i32) {
    unsafe { extern_c::srand(seed) }
}

pub fn random_shuffle<T>(slice: &mut [T]) {
    for i in 0..slice.len() {
        slice.swap(i, (rand().unsigned_abs() as usize) % slice.len())
    }
}

pub struct Scanner<R> {
    buf_reader: BufReader<R>,
    buffer: VecDeque<String>,
}

impl Scanner<Stdin> {
    pub fn from_stdin() -> Scanner<Stdin> {
        Scanner {
            buf_reader: BufReader::new(std::io::stdin()),
            buffer: VecDeque::new(),
        }
    }
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
