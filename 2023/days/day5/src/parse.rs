#[derive(Debug, Clone, Copy)]
pub struct Range {
    pub start: i64,
    pub end: i64,
}

impl Range {
    fn nil() -> Range {
        Range { start: 0, end: 0 }
    }

    fn is_nil(&self) -> bool {
        self.end == self.start
    }

    fn contains(&self, v: i64) -> bool {
        self.start <= v && self.end > v
    }

    fn intersect(&self, other: Range) -> Range {
        if !(self.contains(other.start)
            || self.contains(other.end)
            || other.contains(self.start)
            || other.contains(self.end))
        {
            return Range::nil();
        }
        Range {
            start: self.start.max(other.start),
            end: self.end.min(other.end),
        }
    }

    fn shift(&self, by: i64) -> Range {
        Range {
            start: self.start + by,
            end: self.end + by,
        }
    }
}

#[derive(Debug)]
pub struct Map {
    pub name: String,
    pub ranges: Vec<(Range, i64)>,
}

impl Map {
    pub fn map_one(&self, n: i64) -> i64 {
        for &(rng, s) in &self.ranges {
            if rng.contains(n) {
                return n + s;
            }
        }
        return n;
    }

    pub fn map_range(&self, r: Range) -> Vec<Range> {
        let mut new_ranges = Vec::new();

        for &(map_rng, shift_by) in &self.ranges {
            let intersection = r.intersect(map_rng);
            if intersection.is_nil() {
                continue;
            }
            new_ranges.push(intersection.shift(shift_by));
        }
        if new_ranges.is_empty() {
            new_ranges.push(r)
        }
        new_ranges
    }

    fn clean_up(&mut self) {
        self.ranges.sort_by_key(|r| r.0.start);
        let mut new_ranges = Vec::new();
        if self.ranges[0].0.start > 0 {
            new_ranges.push((
                Range {
                    start: 0,
                    end: self.ranges[0].0.start,
                },
                0,
            ));
        }
        for (&f, &s) in self.ranges.iter().zip(self.ranges.iter().skip(1)) {
            new_ranges.push(f);
            if f.0.end < s.0.start {
                new_ranges.push((
                    Range {
                        start: f.0.end,
                        end: s.0.start,
                    },
                    0,
                ));
            }
        }
        new_ranges.push((
            Range {
                start: self.ranges.last().unwrap().0.end,
                end: std::i64::MAX,
            },
            0,
        ));
        self.ranges = new_ranges;
    }
}

fn from_line(line: &str) -> (Range, i64) {
    let vals: Vec<i64> = line
        .split(' ')
        .map(|s| s.parse().unwrap())
        .take(3)
        .collect();

    let (to, from, size) = (vals[0], vals[1], vals[2]);

    (
        Range {
            start: from,
            end: from + size,
        },
        to - from,
    )
}

pub fn parse_contents(contents: &str) -> (Vec<Range>, Vec<Map>) {
    let lines: Vec<&str> = contents.lines().collect();

    let parsed: Vec<i64> = lines[0]
        .split(' ')
        .skip(1)
        .map(|s| s.parse().unwrap())
        .collect();

    let seeds: Vec<Range> = parsed
        .chunks(2)
        .map(|chunk| Range {
            start: chunk[0],
            end: chunk[0] + chunk[1],
        })
        .collect();

    let mut maps: Vec<Map> = Vec::new();

    let mut idx = 2;
    while idx < lines.len() {
        maps.push(Map {
            name: lines[idx].split(' ').next().unwrap().to_string(),
            ranges: Vec::new(),
        });
        idx += 1; // header
        let m = maps.last_mut().unwrap();
        while idx < lines.len() && lines[idx].len() > 0 {
            m.ranges.push(from_line(lines[idx]));
            idx += 1;
        }
        m.clean_up();
        idx += 1;
    }

    return (seeds, maps);
}
