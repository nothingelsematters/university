use std::{
    fmt::Debug,
    ops::{Add, Mul, Sub},
};

use crate::utils::{random_shuffle, Scanner};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Point {
    x: f64,
    y: f64,
}

impl Point {
    pub fn new(x: f64, y: f64) -> Point {
        Point { x, y }
    }

    pub fn normalized_square(&self) -> f64 {
        self.x * self.x + self.y * self.y
    }

    pub fn normalized(&self) -> f64 {
        self.normalized_square().sqrt()
    }
}

impl Add<Point> for Point {
    type Output = Point;

    fn add(self, rhs: Point) -> Self::Output {
        Point::new(self.x + rhs.x, self.y + rhs.y)
    }
}

impl Sub<Point> for Point {
    type Output = Point;

    fn sub(self, rhs: Point) -> Self::Output {
        Point::new(self.x - rhs.x, self.y - rhs.y)
    }
}

impl Mul<f64> for Point {
    type Output = Point;

    fn mul(self, rhs: f64) -> Self::Output {
        Point::new(self.x * rhs, self.y * rhs)
    }
}

impl Mul<f64> for &Point {
    type Output = Point;

    fn mul(self, rhs: f64) -> Self::Output {
        Point::new(self.x * rhs, self.y * rhs)
    }
}

#[derive(Debug, PartialEq)]
pub struct Circle {
    center: Point,
    radius: f64,
}

pub fn solve_hell_flapper() {
    let mut scanner = Scanner::from_stdin();

    let wasps_len: usize = scanner.read_next();

    let mut wasps: Vec<Point> = (0..wasps_len)
        .map(|_i| Point::new(scanner.read_next(), scanner.read_next()))
        .collect();

    let solution = hell_flapper(&mut wasps);

    println!(
        "{:.7} {:.7}\n{:.7}",
        solution.center.x, solution.center.y, solution.radius,
    )
}

pub fn hell_flapper(wasps: &mut [Point]) -> Circle {
    if wasps.len() == 1 {
        return Circle {
            center: wasps[0],
            radius: 0.0,
        };
    }

    build_circle(&min_circle_points(wasps, Vec::new()))
}

fn min_circle_points(points: &mut [Point], fixed: Vec<Point>) -> Vec<Point> {
    let fixed_len = fixed.len();
    if fixed_len == 3 {
        return fixed;
    }

    random_shuffle(points);

    let taken = 2 - fixed_len;
    let mut circle_points = fixed.to_vec();
    circle_points.extend_from_slice(&points[..taken]);

    for i in taken..points.len() {
        if !is_in_circle(&points[i], &circle_points) {
            let mut fixed = fixed.clone();
            fixed.push(points[i]);
            circle_points = min_circle_points(&mut points[..i], fixed);
        }
    }

    circle_points
}

fn is_in_circle(point: &Point, circle_points: &[Point]) -> bool {
    match circle_points.len() {
        2 => {
            let p0 = circle_points[0];
            let p1 = circle_points[1];

            let radius = p0 - p1;
            let distance = point * 2.0 - p0 - p1;
            distance.normalized_square() <= radius.normalized_square()
        }
        3 => {
            let p0 = circle_points[0];
            let p1 = circle_points[1];
            let p2 = circle_points[2];

            let (d, dx, dy) = get_distances(p0, p1, p2);

            let left = point * d + Point::new(-dx, dy);
            let right = p0 * d + Point::new(-dx, dy);
            left.normalized_square() <= right.normalized_square()
        }
        _ => false,
    }
}

fn build_circle(points: &[Point]) -> Circle {
    match points.len() {
        2 => {
            let p0 = points[0];
            let p1 = points[1];

            let center = (p0 + p1) * 0.5;
            Circle {
                center,
                radius: (p0 - center).normalized(),
            }
        }
        3 => {
            let p0 = points[0];
            let p1 = points[1];
            let p2 = points[2];
            let (d, dx, dy) = get_distances(p0, p1, p2);

            let center = Point::new(dx / d, -dy / d);
            Circle {
                center,
                radius: (p0 - center).normalized(),
            }
        }
        _ => panic!(),
    }
}

fn get_distances(p0: Point, p1: Point, p2: Point) -> (f64, f64, f64) {
    fn square2(p0: Point, p1: Point, p2: Point) -> f64 {
        let a = p1 - p0;
        let b = p2 - p0;
        a.x * b.y - a.y * b.x
    }

    let d = 2.0 * square2(p0, p1, p2);

    let mut p0_clone = Point::new(p0.normalized_square(), p0.y);
    let mut p1_clone = Point::new(p1.normalized_square(), p1.y);
    let mut p2_clone = Point::new(p2.normalized_square(), p2.y);

    let dx = square2(p0_clone, p1_clone, p2_clone);

    p0_clone.y = p0.x;
    p1_clone.y = p1.x;
    p2_clone.y = p2.x;

    let dy = square2(p0_clone, p1_clone, p2_clone);

    (d, dx, dy)
}

#[cfg(test)]
mod tests {
    use super::{hell_flapper, Circle, Point};

    fn assert_eq_precision(a: f64, b: f64) {
        assert!((a - b).abs() <= 1e-6, "|{a} - {b}| > 1e-6")
    }

    #[test]
    fn sample() {
        let mut wasps = vec![
            Point::new(2.0, 0.0),
            Point::new(0.0, 2.0),
            Point::new(0.0, 0.0),
        ];
        let Circle { center, radius } = hell_flapper(&mut wasps);

        assert_eq_precision(center.x, 1.0);
        assert_eq_precision(center.y, 1.0);
        assert_eq_precision(radius, std::f64::consts::SQRT_2);
    }

    #[test]
    fn stress_wa5() {
        let mut wasps = vec![
            Point::new(-829.0, 917.0),
            Point::new(-739.0, -726.0),
            Point::new(146.0, 926.0),
            Point::new(537.0, -284.0),
        ];
        let Circle { center, radius } = hell_flapper(&mut wasps);

        assert_eq_precision(center.x, -317.8551792675757);
        assert_eq_precision(center.y, 121.03440892630444);
        assert_eq_precision(radius, 945.9546764696921);
    }
}
