use std::env;
use std::fs;

// This problem has a simple algebraic solution.
//
// For a race of time T and button time x, the distance the
// car will travel for that button time is x * (T - x).
//
// So to find an x that beats the record distancd D is to
// solve the inequality -x**2 + Tx - D > 0.
//
// This is equivalent to finding the range of x where
// the resulting parabola is above the x-axis. For parabola
// where the square coefficient is < 0, that is of the form
// x \in [root1, root2].
//
// So to find the button times that will beat the record, just
// plug -1, T and -D into the quadratic formula, round to
// integers and take their difference to get the number of
// whole numbers between [root1, root2).
//
// Edge case: if the roots are already whole numbers, the upper
// root must be subtracted by 1.

fn parse_line(line: &str) -> Vec<&str> {
    line.split(' ').skip(1).filter(|s| s.len() > 0).collect()
}

fn parse_data(contents: &str) -> (f64, f64) {
    let mut lines = contents.lines();

    let time_vals: Vec<&str> = parse_line(lines.next().unwrap());

    let time: f64 = time_vals.join("").parse().unwrap();

    let dist_vals: Vec<&str> = parse_line(lines.next().unwrap());

    let dist: f64 = dist_vals.join("").parse().unwrap();

    return (time, dist);
}

fn quadratic_formula(t: f64, d: f64) -> (f64, f64) {
    let inner = t * t - 4. * d;

    let first = t / 2. - inner.sqrt() / 2.;
    let second = t / 2. + inner.sqrt() / 2.;

    return (first.min(second), first.max(second));
}

fn main() {
    let contents = env::args()
        .last()
        .and_then(|fname| fs::read_to_string(fname).ok())
        .expect("Couldn't read file");

    let (t, d) = parse_data(&contents);
    let (lo, _hi): (f64, f64) = quadratic_formula(t, d);
    let mut hi = _hi;
    if hi.floor() == hi {
        hi -= 1.;
    }
    let diff = hi.floor() as i64 - lo.floor() as i64;
    println!("{}", diff);
}
