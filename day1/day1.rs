use std::fs;
use anyhow::Result;

#[derive(Debug)]
struct Rotation {
    direction: char,
    distance: isize,
}

struct Dial {
    start_position: isize,
    current_position: isize,
    zero_count_part1: isize,
    zero_count_part2: isize,
}

impl Dial {
    fn new(start_position: isize) -> Self {
        Dial {
            start_position: start_position,
            current_position: start_position,
            zero_count_part1: 0,
            zero_count_part2: 0,
        }
    }

    fn turn_dial(&mut self, rotation: &Rotation) {
        let mut position_norm: isize = self.current_position;

        if rotation.direction == 'L' {
            position_norm = (100 - self.current_position).rem_euclid(100);
            self.current_position = (self.current_position - rotation.distance).rem_euclid(100);
        } else if rotation.direction == 'R' {
            position_norm = self.current_position;
            self.current_position = (self.current_position + rotation.distance).rem_euclid(100);
        }

        if self.current_position == 0 {
            self.zero_count_part1 += 1;
        }

        let zeros: isize = (position_norm + rotation.distance) / 100;

        self.zero_count_part2 += zeros;
    }
}

fn main() -> Result<()> {
    let file_name = "data/input.txt";

    // Read in data
    let data_in = fs::read_to_string(file_name)?;

    // Parse Instructions into rotation struct
    let lines = data_in.split('\n');

    let mut rotations: Vec<Rotation> = Vec::new();

    for line in lines {
        let (direction, distance) = line.split_at(1);

        rotations.push(Rotation{
            direction: direction.parse::<char>().unwrap(),
            distance: distance.parse::<isize>().unwrap(),
        });
    }

    let mut my_dial: Dial = Dial::new(50);

    for rotation in rotations {
        my_dial.turn_dial(&rotation);
    }

    println!("Part 1 Answer: {}", my_dial.zero_count_part1);
    println!("Part 2 Answer: {}", my_dial.zero_count_part2);

    Ok(())
}
